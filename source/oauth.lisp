;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :anvil)

;;
;; An OAuth2 provider: the authorization code flow with PKCE, which is
;; what the git gate in front of common-lisp.net's prompt lab speaks
;; to GitLab.  /oauth/authorize asks the signed-in user to allow the
;; application and sends the browser back with a code; /oauth/token
;; trades the code (and the PKCE verifier) for a token, which is a
;; token record like a personal one, carrying the application's id;
;; /api/v4/user answers who it is.
;;
;; Applications are records an administrator makes (the settings
;; page): a name, redirect URIs, scopes, a secret shown once.
;;

(define-index :oauth-app :client-id (lambda (app) (getf app :client-id)))

(defun make-oauth-app! (name redirect-uris &key (scopes '("api")) (confidential? t) owner)
  "Values: the application record, and its secret (kept hashed)."
  (let ((secret (random-hex 24)))
    (values (save-record! (new-record :oauth-app :name name :client-id (random-hex 16)
                                                 :secret-hash (sha256-hex secret)
                                                 :redirect-uris (ylist redirect-uris)
                                                 :scopes scopes :confidential? confidential?
                                                 :owner-id (and owner (getf owner :id))))
            secret)))

(defun find-oauth-app (client-id) (lookup :oauth-app :client-id client-id))

(defvar *oauth-codes* (make-hash-table :test #'equal) "code -> plist, for ten minutes")
(defvar *oauth-lock* (bt:make-lock "anvil oauth"))

(defun prune-oauth-codes! ()
  (let ((now (get-universal-time)) (gone nil))
    (maphash (lambda (code grant) (when (> (- now (getf grant :made)) 600) (push code gone))) *oauth-codes*)
    (dolist (code gone) (remhash code *oauth-codes*))))

(defun redirect-uri-allowed? (app uri)
  (and (stringp uri) (member uri (getf app :redirect-uris) :test #'string=)))

(defun oauth-authorize-page (req ent)
  "GET: the consent page; POST: the user's answer."
  (let* ((client-id (param req "client_id"))
         (app (and client-id (find-oauth-app client-id)))
         (redirect-uri (param req "redirect_uri"))
         (state (param req "state"))
         (scope (or (param req "scope") "api"))
         (challenge (param req "code_challenge"))
         (method (or (param req "code_challenge_method") "plain")))
    (cond
      ((null app) (respond-html req ent (simple-page "Unknown application" "<p>No OAuth application has that client id.</p>") :status 400))
      ((not (redirect-uri-allowed? app redirect-uri))
       (respond-html req ent (simple-page "Redirect refused" "<p>The redirect address is not one the application registered.</p>") :status 400))
      ((not (equal (param req "response_type" "code") "code"))
       (redirect req ent (format nil "~a?error=unsupported_response_type~@[&state=~a~]" redirect-uri (and state (url-encode state)))))
      ((null *current-user*)
       (redirect req ent (format nil "/users/sign_in?return_to=~a" (url-encode (strcat (request-path req) "?" (or (net.uri:uri-query (net.aserve:request-uri req)) ""))))))
      ((eq (http-method req) :get)
       (respond-html req ent (the-object (site) (oauth-consent-html app redirect-uri state scope challenge method))))
      ((equal (param req "decision") "deny")
       (redirect req ent (format nil "~a?error=access_denied~@[&state=~a~]" redirect-uri (and state (url-encode state)))))
      (t
       (let ((code (random-hex 20)))
         (bt:with-lock-held (*oauth-lock*)
           (prune-oauth-codes!)
           (setf (gethash code *oauth-codes*)
                 (list :app-id (getf app :id) :user-id (getf *current-user* :id) :redirect-uri redirect-uri
                       :scope scope :challenge challenge :method method :made (get-universal-time))))
         (redirect req ent (format nil "~a~a~a=~a~@[&state=~a~]" redirect-uri (if (position #\? redirect-uri) "&" "?")
                                   "code" code (and state (url-encode state)))))))))

(defun pkce-ok? (grant verifier)
  (let ((challenge (getf grant :challenge)))
    (cond ((null challenge) t)
          ((null verifier) nil)
          ((string-equal (getf grant :method) "S256")
           (string= challenge (base64url (ingot::sha256-digest (ingot:string-to-octets verifier)))))
          (t (string= challenge verifier)))))

(defun base64url (octets)
  (string-right-trim "=" (substitute #\_ #\/ (substitute #\- #\+ (cl-base64:usb8-array-to-base64-string octets)))))

(defun oauth-token-door (req ent)
  "POST grant_type=authorization_code&code=...&redirect_uri=...&client_id=...
[&client_secret=...][&code_verifier=...]: the token, as JSON."
  (flet ((fail (error description &optional (status 400))
           (respond-json req ent (json-table "error" error "error_description" description) :status status)))
    (let* ((grant-type (param req "grant_type"))
           (code (param req "code"))
           (client-id (param req "client_id"))
           (secret (param req "client_secret"))
           (app (and client-id (find-oauth-app client-id))))
      (multiple-value-bind (basic-user basic-password) (basic-credentials req)
        (when (and basic-user (null app))
          (setq app (find-oauth-app basic-user) secret basic-password)))
      (cond
        ((not (equal grant-type "authorization_code")) (fail "unsupported_grant_type" "Only authorization_code is offered."))
        ((null app) (fail "invalid_client" "No such application." 401))
        ((and (getf app :confidential?) secret (not (string= (sha256-hex secret) (getf app :secret-hash))))
         (fail "invalid_client" "The client secret is wrong." 401))
        (t
         (let ((grant (bt:with-lock-held (*oauth-lock*) (prune-oauth-codes!) (prog1 (gethash code *oauth-codes*) (remhash code *oauth-codes*)))))
           (cond
             ((or (null grant) (not (eql (getf grant :app-id) (getf app :id)))) (fail "invalid_grant" "The code is unknown or spent."))
             ((not (equal (getf grant :redirect-uri) (param req "redirect_uri"))) (fail "invalid_grant" "The redirect address differs."))
             ((not (pkce-ok? grant (param req "code_verifier"))) (fail "invalid_grant" "The PKCE verifier does not match."))
             (t
              (let ((user (record :user (getf grant :user-id))))
                (multiple-value-bind (text token)
                    (make-token! user (format nil "OAuth: ~a" (getf app :name))
                                 (split-on (getf grant :scope) #\Space)
                                 :expires (+ (get-universal-time) (* 2 3600)) :oauth-app-id (getf app :id))
                  (respond-json req ent (json-table "access_token" text "token_type" "Bearer"
                                                    "expires_in" 7200 "scope" (getf grant :scope)
                                                    "created_at" (unix-time (getf token :created))))))))))))))

(defun oauth-revoke-door (req ent)
  (let ((token (find-token (param req "token"))))
    (when token (revoke-token! token))
    (respond-json req ent (json-table))))
