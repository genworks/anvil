;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :anvil)

;;
;; Passwords, tokens, sessions, and who a request is from.
;;
;; A password is kept as PBKDF2-SHA256 with a salt of its own.  A
;; personal access token is shown once and kept hashed; so is an OAuth
;; access token.  A session is a random id in an HttpOnly cookie,
;; pointing at a record with the user and an expiry.
;;

(defparameter *session-cookie* "anvil_session")

(defun user-active? (user) (eq (getf user :state) :active))

;;; Passwords

(defun hash-password (password &optional (salt (random-hex 16)) (iterations *password-iterations*))
  "A string: iterations$salt$hash."
  (format nil "~d$~a$~a" iterations salt
          (ingot:hex (ingot::pbkdf2-sha256 password (ingot:string-to-octets salt) iterations 32))))

(defun check-password (password stored)
  "True when PASSWORD is the one STORED hashes."
  (and (stringp stored) (stringp password)
       (let ((parts (uiop:split-string stored :separator "$")))
         (and (= (length parts) 3)
              (let ((iterations (ignore-errors (parse-integer (first parts)))))
                (and iterations
                     (constant-time-equal? (hash-password password (second parts) iterations) stored)))))))

(defun constant-time-equal? (a b)
  (and (= (length a) (length b))
       (zerop (loop for x across a for y across b sum (logxor (char-code x) (char-code y))))))

;;; Tokens: the plain text is "anvil-" and 32 random characters; what
;;; is kept is its SHA-256.

(defparameter *token-prefix* "anvil-")

(defun new-token-text ()
  (strcat *token-prefix* (random-token 32)))

(defun token-hash (text) (sha256-hex text))

(define-index :token :hash (lambda (token) (getf token :hash)))

(defun make-token! (user name scopes &key expires oauth-app-id)
  "A new token for USER with NAME and SCOPES (a list of strings among
api, read_api, read_user, read_repository, write_repository).  Answers
the token's plain text, which is not kept, and the record."
  (let* ((text (new-token-text))
         (record (save-record! (new-record :token :user-id (getf user :id) :name name
                                                  :scopes scopes :hash (token-hash text)
                                                  :expires expires :oauth-app-id oauth-app-id
                                                  :last-used nil))))
    (values text record)))

(defun find-token (text)
  "The live token record for TEXT, or nil."
  (when (and (stringp text) (starts-with? *token-prefix* text))
    (let ((token (lookup :token :hash (token-hash text))))
      (and token
           (or (null (getf token :expires)) (> (getf token :expires) (get-universal-time)))
           (not (getf token :revoked))
           token))))

(defun revoke-token! (token)
  (update-record! token :revoked t))

(defun user-tokens (user)
  (records :token (lambda (token) (and (eql (getf token :user-id) (getf user :id))
                                       (not (getf token :revoked))
                                       (null (getf token :oauth-app-id))))))

(defun token-allows? (token scope)
  "Whether TOKEN (nil: a session, which allows everything) carries
SCOPE: :api lets everything; :read-api reads; :read-repository and
:write-repository the repository."
  (or (null token)
      (let ((scopes (getf token :scopes)))
        (or (member "api" scopes :test #'string=)
            (ecase scope
              (:api nil)
              (:read-api (member "read_api" scopes :test #'string=))
              (:read-user (or (member "read_user" scopes :test #'string=) (member "read_api" scopes :test #'string=)))
              (:read-repository (or (member "read_repository" scopes :test #'string=)
                                    (member "write_repository" scopes :test #'string=)
                                    (member "read_api" scopes :test #'string=)))
              (:write-repository (member "write_repository" scopes :test #'string=)))))))

;;; Sessions

(define-index :session :cookie (lambda (session) (getf session :cookie)))

(defun make-session! (user)
  (save-record! (new-record :session :user-id (getf user :id) :cookie (random-token 40)
                                     :expires (+ (get-universal-time) (* *session-days* 86400)))))

(defun find-session (cookie)
  (let ((session (lookup :session :cookie cookie)))
    (and session (> (getf session :expires) (get-universal-time)) session)))

(defun cookie-value (req name)
  (let ((header (header req :cookie)))
    (when header
      (loop for part in (uiop:split-string header :separator ";")
            for trimmed = (trim part)
            for eq = (position #\= trimmed)
            when (and eq (string= name trimmed :end2 eq))
              return (subseq trimmed (1+ eq))))))

(defun session-cookie-header (session &key clear?)
  (if clear?
      (format nil "~a=; Path=/; Max-Age=0; HttpOnly; SameSite=Lax" *session-cookie*)
      (format nil "~a=~a; Path=/; Max-Age=~d; HttpOnly; SameSite=Lax~:[~;; Secure~]"
              *session-cookie* (getf session :cookie) (* *session-days* 86400)
              (starts-with? "https:" *site-url*))))

(defun reap-sessions! ()
  (dolist (session (records :session (lambda (s) (< (getf s :expires) (get-universal-time)))))
    (delete-record! session)))

;;; Who is asking

(defvar *current-user* nil "The user record of the request being served, or nil.")
(defvar *current-token* nil "The token record it came with, or nil for a session or nobody.")
(defvar *current-session* nil)

(defun basic-credentials (req)
  "Two values, the user name and the password of an Authorization:
Basic header, or nil."
  (let ((auth (header req :authorization)))
    (when (and auth (> (length auth) 6) (string-equal "Basic " auth :end2 6))
      (let* ((decoded (ignore-errors (ingot:octets-to-string (cl-base64:base64-string-to-usb8-array (trim (subseq auth 6))))))
             (colon (and decoded (position #\: decoded))))
        (when colon
          (values (subseq decoded 0 colon) (subseq decoded (1+ colon))))))))

(defun bearer-token (req)
  (let ((auth (header req :authorization)))
    (when (and auth (> (length auth) 7) (string-equal "Bearer " auth :end2 7))
      (trim (subseq auth 7)))))

(defun authenticate (req)
  "Three values: the user asking, the token it came with (or nil), and
the session (or nil).  Looks at PRIVATE-TOKEN, a Bearer token, Basic
credentials (a password, or any name with a token as the password),
then the session cookie."
  (flet ((by-token (text)
           (let ((token (find-token text)))
             (when token
               (let ((user (record :user (getf token :user-id))))
                 (when (and user (user-active? user))
                   (unless (eql (getf token :last-used) (iso-date-number))
                     (ignore-errors (update-record! token :last-used (iso-date-number))))
                   (return-from authenticate (values user token nil))))))))
    (let ((private (header req :private-token)))
      (when private (by-token private)))
    (let ((bearer (bearer-token req)))
      (when bearer (by-token bearer)))
    (multiple-value-bind (name password) (basic-credentials req)
      (when name
        (by-token password)
        (let ((user (funcall 'find-user name)))  ; model.lisp, loaded after this file
          (when (and user (user-active? user) (check-password password (getf user :password)))
            (return-from authenticate (values user nil nil))))))
    (let* ((cookie (cookie-value req *session-cookie*))
           (session (and cookie (find-session cookie)))
           (user (and session (record :user (getf session :user-id)))))
      (when (and user (user-active? user))
        (return-from authenticate (values user nil session))))
    (values nil nil nil)))

(defun iso-date-number ()
  "Today as 20261007, cheap to compare."
  (multiple-value-bind (s mi h d mo y) (decode-universal-time (get-universal-time) 0)
    (declare (ignore s mi h))
    (+ (* y 10000) (* mo 100) d)))

(defmacro with-request-user ((req) &body body)
  "Bind *current-user*, *current-token* and *current-session* for REQ."
  `(multiple-value-bind (*current-user* *current-token* *current-session*) (authenticate ,req)
     (let ((*request-json* nil))
       ,@body)))

(defun require-user (&optional (scope :api))
  "The current user, who must be there and whose token (if any) must
carry SCOPE; else 401."
  (unless *current-user* (unauthorized "401 Unauthorized"))
  (unless (token-allows? *current-token* scope) (forbidden "The token's scope does not allow this."))
  *current-user*)

(defun admin? (&optional (user *current-user*))
  (and user (getf user :admin?) t))

(defun require-admin ()
  (require-user)
  (unless (admin?) (forbidden "An administrator's action."))
  *current-user*)
