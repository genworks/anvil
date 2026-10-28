;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :anvil)

;;
;; What every handler shares: answering a request, reading one, JSON,
;; text, time, and the errors a door answers with a status.
;;

(defparameter *whitespace* '(#\Space #\Tab #\Newline #\Return))

(defun trim (string) (string-trim *whitespace* (or string "")))

(defun blank? (string) (or (null string) (zerop (length (trim string)))))

(defun strcat (&rest strings) (apply #'concatenate 'string (mapcar (lambda (s) (or s "")) strings)))

(defun starts-with? (prefix string)
  (and (stringp string) (>= (length string) (length prefix)) (string= prefix string :end2 (length prefix))))

(defun ends-with? (suffix string)
  (and (stringp string) (>= (length string) (length suffix))
       (string= suffix string :start2 (- (length string) (length suffix)))))

(defun split-on (string separator)
  (remove "" (uiop:split-string (or string "") :separator (string separator)) :test #'string=))

(defun plural (n singular &optional (plural (strcat singular "s")))
  (format nil "~d ~a" n (if (= n 1) singular plural)))

;;; Errors a handler turns into a status

(define-condition http-error (error)
  ((status :initarg :status :reader http-error-status)
   (text :initarg :text :reader http-error-text))
  (:report (lambda (c s) (format s "~d ~a" (http-error-status c) (http-error-text c)))))

(defun http-error (status control &rest args)
  (error 'http-error :status status :text (apply #'format nil control args)))

(defun not-found (&optional (text "Not found."))
  (http-error 404 "~a" text))

(defun forbidden (&optional (text "Forbidden."))
  (http-error 403 "~a" text))

(defun unauthorized (&optional (text "Sign in."))
  (http-error 401 "~a" text))

(defun bad-request (control &rest args)
  (error 'http-error :status 400 :text (apply #'format nil control args)))

(defun response-for (status)
  (case status
    (200 net.aserve:*response-ok*)
    (201 net.aserve:*response-created*)
    (202 net.aserve:*response-accepted*)
    (204 net.aserve:*response-no-content*)
    (301 net.aserve:*response-moved-permanently*)
    (302 net.aserve:*response-found*)
    (303 net.aserve:*response-see-other*)
    (304 net.aserve:*response-not-modified*)
    (400 net.aserve:*response-bad-request*)
    (401 net.aserve:*response-unauthorized*)
    (403 net.aserve:*response-forbidden*)
    (404 net.aserve:*response-not-found*)
    (405 net.aserve:*response-method-not-allowed*)
    (409 (net.aserve::make-resp 409 "Conflict"))
    (422 (net.aserve::make-resp 422 "Unprocessable Entity"))
    (429 (net.aserve::make-resp 429 "Too Many Requests"))
    (500 net.aserve:*response-internal-server-error*)
    (501 net.aserve:*response-not-implemented*)
    (502 (net.aserve::make-resp 502 "Bad Gateway"))
    (503 (net.aserve::make-resp 503 "Service Unavailable"))
    (t (net.aserve::make-resp status "Status"))))

;;; Answering

(defun respond (req ent body &key (status 200) content-type cache-control headers)
  "Answer REQ with BODY, a string (sent as UTF-8) or octets.  HEADERS is
a plist of further reply headers."
  (net.aserve:with-http-response (req ent :response (response-for status) :content-type content-type)
    (when cache-control
      (setf (net.aserve:reply-header-slot-value req :cache-control) cache-control))
    (loop for (name value) on headers by #'cddr
          when value do (setf (net.aserve:reply-header-slot-value req name) (princ-to-string value)))
    (net.aserve:with-http-body (req ent :external-format (if (stringp body) :utf-8
                                                             net.aserve:*default-aserve-external-format*))
      (let ((stream (net.aserve:request-reply-stream req)))
        (if (stringp body)
            (write-string body stream)
            (write-sequence body stream))))))

(defun respond-html (req ent html &key (status 200) headers)
  (respond req ent html :status status :content-type "text/html; charset=utf-8"
                        :cache-control "no-store" :headers headers))

(defun respond-text (req ent text &key (status 200))
  (respond req ent text :status status :content-type "text/plain; charset=utf-8" :cache-control "no-store"))

(defun redirect (req ent location &key (status 303) headers)
  (respond req ent "" :status status :cache-control "no-store" :headers (append (list :location location) headers)))

(defun json-table (&rest keys-and-values)
  "A hash table standing for a JSON object (nil encodes as null)."
  (let ((table (make-hash-table :test #'equal)))
    (loop for (key value) on keys-and-values by #'cddr
          do (setf (gethash key table) value))
    table))

(defun json-string (value)
  (with-output-to-string (out) (yason:encode value out)))

(defun parse-json (string)
  (yason:parse string :json-arrays-as-vectors t :json-booleans-as-symbols t :json-nulls-as-keyword t))

(defun json-boolean (value) (if value 'yason:true 'yason:false))

(defun json-get (table key &optional default)
  (if (hash-table-p table)
      (multiple-value-bind (value found?) (gethash key table)
        (if found? (if (eq value :null) nil value) default))
      default))

(defun respond-json (req ent object &key (status 200) headers)
  (respond req ent (json-string object) :status status :cache-control "no-store"
                                        :content-type "application/json; charset=utf-8" :headers headers))

(defun respond-error-json (req ent status text)
  (respond-json req ent (json-table "message" text) :status status))

;;; Reading

(defun http-method (req) (net.aserve:request-method req))

(defun request-path (req)
  (or (net.uri:uri-path (net.aserve:request-uri req)) "/"))

(defun query-of (req)
  "The query as an alist of strings, from the URL and, for a form POST,
the body."
  (net.aserve:request-query req :external-format :utf-8))

(defun query-value (req name &optional default)
  (let ((pair (assoc name (query-of req) :test #'string=)))
    (if (and pair (cdr pair) (plusp (length (cdr pair)))) (cdr pair) default)))

(defun query-values (req name)
  (loop for (key . value) in (query-of req) when (string= key name) collect value))

(defun url-query (req)
  "Only the URL's query, as an alist (a POST's body left alone)."
  (let ((query (net.uri:uri-query (net.aserve:request-uri req))))
    (and query (plusp (length query))
         (ignore-errors (net.aserve:form-urlencoded-to-query query :external-format :utf-8)))))

(defun url-query-value (req name &optional default)
  (let ((pair (assoc name (url-query req) :test #'string=)))
    (if (and pair (cdr pair) (plusp (length (cdr pair)))) (cdr pair) default)))

(defun header (req name)
  "The value of request header NAME (a keyword or a string), or nil."
  (let ((value (net.aserve:header-slot-value req (if (stringp name) (intern (string-upcase name) :keyword) name))))
    (and (stringp value) (plusp (length value)) value)))

(defun chunked-request? (req)
  (equalp "chunked" (header req :transfer-encoding)))

(defun read-chunked-request-body (req)
  "The body of a chunked request (git sends a push of more than a
megabyte so) as octets, read from the socket chunk by chunk rather
than through AllegroServe's unchunking stream, which not every Lisp
can read from.  The body is marked read, so the server does not look
for it again."
  (let ((socket (net.aserve:request-socket req))
        (buffer (ingot::make-buffer (* 1024 1024))))
    (labels ((line ()
               (let ((text (make-array 16 :element-type 'character :adjustable t :fill-pointer 0)))
                 (loop for byte = (read-byte socket nil nil)
                       do (cond ((or (null byte) (= byte 10)) (return))
                                ((= byte 13))
                                (t (vector-push-extend (code-char byte) text))))
                 text))
             (exactly (n)
               (let ((octets (ingot:make-octets n)) (at 0))
                 (loop while (< at n)
                       do (let ((got (read-sequence octets socket :start at)))
                            (when (<= got at) (http-error 400 "The body ended inside a chunk."))
                            (setq at got)))
                 octets)))
      (loop
        (let* ((head (line))
               (size (ignore-errors (parse-integer head :end (or (position #\; head) (length head)) :radix 16))))
          (when (or (null size) (zerop size))
            ;; the trailers, up to the empty line
            (loop for text = (line) while (plusp (length text)))
            (return))
          (when (> (+ (fill-pointer buffer) size) *max-request-body*)
            (http-error 413 "The body is too large."))
          (ingot::buffer-write buffer (exactly size))
          (line))))
    (setf (net.aserve::request-request-body req) "")
    (ingot::buffer-octets buffer)))

(defun request-body-octets (req)
  "The request's body as octets, inflated when it came gzip-encoded;
nil when there is none.  Refuses a body over *max-request-body*."
  (let* ((raw (if (chunked-request? req)
                  (read-chunked-request-body req)
                  (ignore-errors (net.aserve:get-request-body req))))
         (octets (cond ((and (typep raw '(vector (unsigned-byte 8))) (plusp (length raw))) raw)
                       ((and (stringp raw) (plusp (length raw)))
                        (let ((out (ingot:make-octets (length raw))))
                          (dotimes (i (length raw) out)
                            (setf (aref out i) (logand #xff (char-code (char raw i)))))))
                       (t nil))))
    (when (and octets (> (length octets) *max-request-body*))
      (http-error 413 "The body is too large."))
    (if (and octets (equal (header req :content-encoding) "gzip"))
        (ingot::gunzip octets)
        octets)))

(defun request-body-text (req)
  "The body as UTF-8 text, \"\" when there is none."
  (let ((octets (request-body-octets req)))
    (if octets (ingot:octets-to-string octets) "")))

(defun request-json (req)
  "The body as a JSON object (a hash table), or nil."
  (let ((text (request-body-text req)))
    (and (plusp (length text))
         (let ((json (ignore-errors (parse-json text))))
           (and (hash-table-p json) json)))))

(defun header-line-value (headers name)
  "The value of the header NAME (case aside) among the lines HEADERS, or nil."
  (dolist (line (split-on headers #\Newline))
    (let ((colon (position #\: line)))
      (when (and colon (string-equal name (trim (subseq line 0 colon))))
        (return (string-trim '(#\Space #\Return) (subseq line (1+ colon))))))))

(defun disposition-parameter (disposition name)
  "The parameter NAME of a Content-Disposition header value: name=\"x\"."
  (when disposition
    (let ((at (search (strcat name "=") disposition)))
      (when at
        (let* ((start (+ at (length name) 1))
               (quoted? (and (< start (length disposition)) (char= (char disposition start) #\")))
               (end (if quoted? (position #\" disposition :start (1+ start)) (or (position #\; disposition :start start) (length disposition)))))
          (subseq disposition (if quoted? (1+ start) start) (or end (length disposition))))))))

(defun multipart-parts (req)
  "The parts of a multipart/form-data body: a list of plists (:name
:filename :content-type :data), the data as octets.  Refuses any other
body."
  (let* ((type (or (header req :content-type) ""))
         (marker (search "boundary=" type)))
    (unless (and (starts-with? "multipart/form-data" type) marker) (bad-request "A multipart/form-data body."))
    (let* ((boundary (string-trim "\" " (subseq type (+ marker 9) (position #\; type :start marker))))
           (delimiter (ingot:string-to-octets (strcat "--" boundary)))
           (crlfcrlf (ingot:octets 13 10 13 10))
           (body (or (request-body-octets req) (ingot:make-octets 0)))
           (parts nil) (at 0))
      (loop
        (let ((start (search delimiter body :start2 at)))
          (unless start (return))
          (let ((after (+ start (length delimiter))))
            ;; the last delimiter ends in --
            (when (and (<= (+ after 2) (length body)) (= (aref body after) 45) (= (aref body (1+ after)) 45)) (return))
            (let* ((headers-start (+ after 2))
                   (headers-end (search crlfcrlf body :start2 headers-start)))
              (unless headers-end (return))
              (let* ((headers (ingot:octets-to-string (subseq body headers-start headers-end)))
                     (data-start (+ headers-end 4))
                     (next (search delimiter body :start2 data-start))
                     (data-end (max data-start (if next (- next 2) (length body))))   ; the CRLF before the next delimiter
                     (disposition (header-line-value headers "content-disposition")))
                (push (list :name (disposition-parameter disposition "name")
                            :filename (disposition-parameter disposition "filename")
                            :content-type (header-line-value headers "content-type")
                            :data (subseq body data-start data-end))
                      parts)
                (setq at (or next (length body))))))))
      (nreverse parts))))

(defun multipart-part (parts name)
  (find name parts :key (lambda (part) (getf part :name)) :test #'equal))

(defun param (req name &optional default)
  "A parameter from the JSON body, the form or the query, as GitLab's
API takes them.  Strings; a JSON number or boolean comes as it is."
  (let ((json (request-json-cached req)))
    (multiple-value-bind (value found?) (if json (gethash name json) (values nil nil))
      (if found?
          (if (eq value :null) default value)
          (query-value req name default)))))

(defvar *request-json* nil "The parsed body of the request being served, once read.")

(defun request-json-cached (req)
  (if (and *request-json* (eq (car *request-json*) req))
      (cdr *request-json*)
      (let ((json (and (member (http-method req) '(:post :put :patch :delete))
                       (starts-with? "application/json" (or (header req :content-type) ""))
                       (request-json req))))
        (setq *request-json* (cons req json))
        json)))

(defun integer-param (req name &optional default)
  (let ((value (param req name)))
    (cond ((integerp value) value)
          ((and (stringp value) (plusp (length value))) (or (ignore-errors (parse-integer value)) default))
          (t default))))

(defun boolean-param (req name &optional default)
  (let ((value (param req name :unset)))
    (cond ((eq value :unset) default)
          ((member value '(t yason:true)) t)
          ((member value '(nil yason:false)) nil)
          ((stringp value) (and (member (string-downcase value) '("true" "1" "yes" "on") :test #'string=) t))
          (t default))))

;;; HTML

(defun html-escape (text)
  (with-output-to-string (out)
    (loop for c across (princ-to-string (or text ""))
          do (case c
               (#\& (write-string "&amp;" out))
               (#\< (write-string "&lt;" out))
               (#\> (write-string "&gt;" out))
               (#\" (write-string "&quot;" out))
               (#\' (write-string "&#39;" out))
               (t (write-char c out))))))

(defun url-encode (text)
  (net.aserve:uriencode-string (princ-to-string text) :external-format :utf-8))

(defun url-decode (text)
  (ignore-errors (net.aserve:uridecode-string text :external-format :utf-8)))

(defun local-path? (text)
  "Whether TEXT is a path on this site, safe to send a browser back to:
one slash, not two (//host is another site)."
  (and (stringp text) (starts-with? "/" text) (not (starts-with? "//" text))))

(defun path-encode (path)
  "A repository path for a URL: every segment encoded, the slashes kept."
  (format nil "~{~a~^/~}" (mapcar #'url-encode (split-on path #\/))))

;;; Time

(defconstant +unix-epoch+ 2208988800)

(defun unix-time (&optional (universal (get-universal-time))) (- universal +unix-epoch+))

(defun iso-timestamp (&optional (universal (get-universal-time)))
  "2026-10-07T21:03:00Z"
  (multiple-value-bind (s mi h d mo y) (decode-universal-time universal 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ" y mo d h mi s)))

(defun iso-timestamp-ms (&optional (universal (get-universal-time)))
  "GitLab's shape: 2026-10-07T21:03:00.000Z"
  (multiple-value-bind (s mi h d mo y) (decode-universal-time universal 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d.000Z" y mo d h mi s)))

(defun iso-date (&optional (universal (get-universal-time)))
  (subseq (iso-timestamp universal) 0 10))

(defun parse-iso-timestamp (text)
  "A universal time from 2026-10-07T21:03:00Z (or with an offset), or nil."
  (when (and (stringp text) (>= (length text) 19))
    (ignore-errors
     (let ((y (parse-integer text :start 0 :end 4)) (mo (parse-integer text :start 5 :end 7))
           (d (parse-integer text :start 8 :end 10)) (h (parse-integer text :start 11 :end 13))
           (mi (parse-integer text :start 14 :end 16)) (s (parse-integer text :start 17 :end 19)))
       (encode-universal-time s mi h d mo y 0)))))

(defun time-ago (universal &optional (now (get-universal-time)))
  "\"3 minutes ago\", \"yesterday\", \"on 2026-09-01\"."
  (let ((seconds (- now universal)))
    (cond ((< seconds 60) "just now")
          ((< seconds 3600) (format nil "~a ago" (plural (floor seconds 60) "minute")))
          ((< seconds 86400) (format nil "~a ago" (plural (floor seconds 3600) "hour")))
          ((< seconds (* 2 86400)) "yesterday")
          ((< seconds (* 30 86400)) (format nil "~a ago" (plural (floor seconds 86400) "day")))
          (t (format nil "on ~a" (iso-date universal))))))

(defun git-time-universal (ident)
  "The universal time of an ident's time."
  (+ (ingot:ident-time ident) +unix-epoch+))

(defun duration-text (seconds)
  (cond ((null seconds) "")
        ((< seconds 60) (format nil "~ds" seconds))
        ((< seconds 3600) (multiple-value-bind (m s) (floor seconds 60) (format nil "~dm ~ds" m s)))
        (t (multiple-value-bind (h rest) (floor seconds 3600) (format nil "~dh ~dm" h (floor rest 60))))))

;;; Files

(defun read-text-file (file)
  (uiop:read-file-string file :external-format :utf-8))

(defun write-text-file (file text)
  (ingot:write-file-octets file (ingot:string-to-octets text)))

(defun read-secret (file &key (min-length 8))
  "The one line of FILE, or nil."
  (let ((found (and file (probe-file file))))
    (when found
      (let ((secret (trim (read-text-file found))))
        (and (>= (length secret) min-length) secret)))))

;;; Randomness

(defun random-octets (n)
  "N octets from the system's random source, else from the Lisp's."
  (or (ignore-errors
       (with-open-file (in "/dev/urandom" :element-type '(unsigned-byte 8))
         (let ((out (ingot:make-octets n)))
           (read-sequence out in)
           out)))
      (let ((state (make-random-state t)) (out (ingot:make-octets n)))
        (dotimes (i n out) (setf (aref out i) (random 256 state))))))

(defun random-hex (n-octets)
  (ingot:hex (random-octets n-octets)))

(defparameter *token-alphabet* "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_")

(defun random-token (length)
  (let ((octets (random-octets length)))
    (map 'string (lambda (byte) (char *token-alphabet* (logand byte 63))) octets)))

(defun sha256-hex (string)
  (ingot:hex (ingot::sha256-digest (ingot:string-to-octets string))))

;;; Threads that do something every so often

(defun loop-thread (thread name seconds function)
  (if (and thread (bt:thread-alive-p thread))
      thread
      (bt:make-thread (lambda ()
                        (loop (handler-case (funcall function)
                                (error (e) (format *error-output* "~&anvil ~a: ~a~%" name e)))
                              (sleep seconds)))
                      :name name)))

(defvar *log-lock* (bt:make-lock "anvil log"))

(defun log-line (control &rest args)
  "One line on the error output, whole: request threads log at once."
  (bt:with-lock-held (*log-lock*)
    (format *error-output* "~&anvil ~a: ~?~%" (iso-timestamp) control args)
    (finish-output *error-output*)))
