;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :anvil)

;;
;; The Docker Engine API over the daemon's Unix socket (or TCP), through
;; a small HTTP/1.1 client of our own, since the socket is not a host
;; and port.  `docker-connect' is the one implementation-specific
;; function in Anvil.
;;

(defparameter *docker-api-version* "v1.43")

(define-condition docker-error (error)
  ((status :initarg :status :reader docker-error-status :initform nil)
   (text :initarg :text :reader docker-error-text))
  (:report (lambda (c s) (format s "Docker: ~@[~a ~]~a" (docker-error-status c) (docker-error-text c)))))

(defun docker-error (status control &rest args)
  (error 'docker-error :status status :text (apply #'format nil control args)))

(defun docker-available? ()
  (and *docker-socket*
       (or (starts-with? "tcp://" *docker-socket*)
           (probe-file *docker-socket*))
       t))

(defun docker-connect ()
  "A bivalent stream to the daemon."
  (cond
    ((null *docker-socket*) (docker-error nil "no Docker socket is configured."))
    ((starts-with? "tcp://" *docker-socket*)
     (let* ((rest (subseq *docker-socket* 6))
            (colon (position #\: rest))
            (host (subseq rest 0 colon))
            (port (parse-integer rest :start (1+ colon))))
       (unless (find-package :usocket) (docker-error nil "a tcp:// Docker host needs usocket loaded."))
       (uiop:symbol-call :usocket :socket-stream
                         (uiop:symbol-call :usocket :socket-connect host port :element-type '(unsigned-byte 8)))))
    (t
     #+ccl (ccl:make-socket :address-family :file :remote-filename *docker-socket* :format :bivalent)
     #+sbcl (let ((socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
              (sb-bsd-sockets:socket-connect socket *docker-socket*)
              (sb-bsd-sockets:socket-make-stream socket :element-type '(unsigned-byte 8) :input t :output t
                                                        :buffering :full))
     #+allegro (socket:make-socket :type :stream :address-family :file :remote-filename *docker-socket* :format :bivalent)
     #-(or ccl sbcl allegro) (docker-error nil "no Unix sockets on this Lisp; set DOCKER_HOST=tcp://..."))))

;;; HTTP over the stream

(defun write-ascii (stream text)
  (write-sequence (ingot::ascii text) stream))

(defun read-crlf-line (stream)
  "A line of the response as a string (without CRLF), or nil at the end."
  (let ((buffer (ingot::make-buffer 128)) (any? nil))
    (loop
      (let ((byte (read-byte stream nil nil)))
        (cond ((null byte) (return (and any? (ingot:octets-to-string (ingot::buffer-octets buffer)))))
              ((= byte 10)
               (let ((octets (ingot::buffer-octets buffer)))
                 (return (ingot:octets-to-string
                          (if (and (plusp (length octets)) (= (aref octets (1- (length octets))) 13))
                              (subseq octets 0 (1- (length octets)))
                              octets)))))
              (t (setq any? t) (ingot::buffer-byte buffer byte)))))))

(defun read-headers (stream)
  "Two values: the status code and an alist of lower-case header names
to values."
  (let ((status-line (read-crlf-line stream)))
    (unless (and status-line (starts-with? "HTTP/" status-line))
      (docker-error nil "no HTTP answer (~a)." status-line))
    (let ((status (parse-integer status-line :start 9 :end 12))
          (headers nil))
      (loop for line = (read-crlf-line stream)
            while (and line (plusp (length line)))
            do (let ((colon (position #\: line)))
                 (when colon
                   (push (cons (string-downcase (subseq line 0 colon)) (trim (subseq line (1+ colon)))) headers))))
      (values status (nreverse headers)))))

(defun read-exactly (stream n)
  (let ((out (ingot:make-octets n)))
    (let ((got (read-sequence out stream)))
      (if (< got n) (subseq out 0 got) out))))

(defun read-chunked-body (stream on-chunk)
  "Read a chunked body, calling ON-CHUNK with each chunk's octets."
  (loop
    (let ((line (read-crlf-line stream)))
      (when (null line) (return))
      (let* ((semicolon (position #\; line))
             (size (parse-integer (trim (subseq line 0 semicolon)) :radix 16)))
        (when (zerop size)
          ;; trailers, until the empty line
          (loop for trailer = (read-crlf-line stream) while (and trailer (plusp (length trailer))))
          (return))
        (funcall on-chunk (read-exactly stream size))
        (read-crlf-line stream)))))

(defun read-body (stream headers on-chunk)
  "Deliver the body to ON-CHUNK by Content-Length, chunked, or to the end."
  (let ((length (cdr (assoc "content-length" headers :test #'string=)))
        (encoding (cdr (assoc "transfer-encoding" headers :test #'string=))))
    (cond ((and encoding (search "chunked" encoding)) (read-chunked-body stream on-chunk))
          (length (let ((n (parse-integer length)))
                    (when (plusp n) (funcall on-chunk (read-exactly stream n)))))
          (t (loop for chunk = (read-exactly stream 65536)
                   while (plusp (length chunk))
                   do (funcall on-chunk chunk))))))

(defun docker-request (method path &key json octets content-type on-chunk headers)
  "Ask the daemon.  Values: the status, the body as octets (unless
ON-CHUNK took the chunks as they came), the headers.  JSON (a hash
table) or OCTETS is the body; HEADERS an alist of further request
headers."
  (let ((stream (docker-connect))
        (body (cond (json (ingot:string-to-octets (json-string json)))
                    (octets octets)
                    (t nil))))
    (unwind-protect
         (progn
           (write-ascii stream (format nil "~a /~a~a HTTP/1.1~c~c" (string-upcase method) *docker-api-version* path #\Return #\Newline))
           (write-ascii stream (format nil "Host: docker~c~c" #\Return #\Newline))
           (write-ascii stream (format nil "User-Agent: anvil/~a~c~c" *version* #\Return #\Newline))
           (write-ascii stream (format nil "Connection: close~c~c" #\Return #\Newline))
           (loop for (name . value) in headers
                 do (write-ascii stream (format nil "~a: ~a~c~c" name value #\Return #\Newline)))
           (when body
             (write-ascii stream (format nil "Content-Type: ~a~c~c" (or content-type (if json "application/json" "application/x-tar")) #\Return #\Newline))
             (write-ascii stream (format nil "Content-Length: ~d~c~c" (length body) #\Return #\Newline)))
           (write-ascii stream (format nil "~c~c" #\Return #\Newline))
           (when body (write-sequence body stream))
           (finish-output stream)
           (multiple-value-bind (status headers) (read-headers stream)
             (if on-chunk
                 (progn (read-body stream headers on-chunk) (values status nil headers))
                 (let ((buffer (ingot::make-buffer 4096)))
                   (read-body stream headers (lambda (chunk) (ingot::buffer-write buffer chunk)))
                   (values status (ingot::buffer-octets buffer) headers)))))
      (ignore-errors (close stream)))))

(defun docker-json (method path &key json)
  "The daemon's JSON answer (parsed), or a docker-error with its message."
  (multiple-value-bind (status body) (docker-request method path :json json)
    (let ((parsed (and body (plusp (length body)) (ignore-errors (parse-json (ingot:octets-to-string body))))))
      (if (<= 200 status 299)
          parsed
          (docker-error status "~a: ~a" path (or (json-get parsed "message") (and body (ingot:octets-to-string body))))))))

;;; What the runner needs

(defun docker-version ()
  (let ((answer (docker-json :get "/version")))
    (json-get answer "Version")))

(defun docker-image-exists? (image)
  (multiple-value-bind (status) (docker-request :get (format nil "/images/~a/json" (url-encode image)))
    (= status 200)))

(defun registry-auth-header (image)
  "The X-Registry-Auth value for IMAGE's registry, or nil."
  (let* ((slash (position #\/ image))
         (registry (if (and slash (or (position #\. image :end slash) (position #\: image :end slash)))
                       (subseq image 0 slash)
                       "https://index.docker.io/v1/"))
         (credentials (cdr (assoc registry *registry-credentials* :test #'string=))))
    (when credentials
      (cl-base64:string-to-base64-string
       (json-string (json-table "username" (car credentials) "password" (cdr credentials) "serveraddress" registry))))))

(defun docker-pull (image &key on-progress)
  "Pull IMAGE; ON-PROGRESS gets each status line.  Signals when it fails."
  (let* ((colon (position #\: image :start (or (position #\/ image :from-end t) 0)))
         (name (if colon (subseq image 0 colon) image))
         (tag (if colon (subseq image (1+ colon)) "latest"))
         (last nil) (error-text nil))
    (multiple-value-bind (status)
        (docker-request :post (format nil "/images/create?fromImage=~a&tag=~a" (url-encode name) (url-encode tag))
                        :octets (ingot:make-octets 0) :content-type "text/plain"
                        :headers (let ((auth (registry-auth-header image))) (and auth (list (cons "X-Registry-Auth" auth))))
                        :on-chunk (lambda (chunk)
                                    (dolist (line (split-on (ingot:octets-to-string chunk) #\Newline))
                                      (let ((json (ignore-errors (parse-json line))))
                                        (when (hash-table-p json)
                                          (when (json-get json "error") (setq error-text (json-get json "error")))
                                          (let ((text (json-get json "status")))
                                            (when (and text on-progress (not (equal text last)))
                                              (setq last text)
                                              (funcall on-progress text))))))))
      (declare (ignore status))
      (when error-text (docker-error nil "pulling ~a: ~a" image error-text))
      t)))

(defun docker-create-container (&key image command entrypoint env working-dir labels memory network-mode
                                     (tty nil) name binds)
  "Create a container; answers its id."
  (let ((host (json-table "AutoRemove" 'yason:false
                          "NetworkMode" (or network-mode "none")
                          "ReadonlyRootfs" 'yason:false
                          "Binds" (coerce binds 'vector))))
    (when memory (setf (gethash "Memory" host) memory (gethash "MemorySwap" host) memory))
    (let ((config (json-table "Image" image
                              "Cmd" (coerce command 'vector)
                              "Env" (coerce env 'vector)
                              "WorkingDir" (or working-dir "/")
                              "Labels" (let ((table (make-hash-table :test #'equal)))
                                         (loop for (k . v) in labels do (setf (gethash k table) v))
                                         table)
                              "Tty" (json-boolean tty)
                              "AttachStdout" 'yason:true "AttachStderr" 'yason:true
                              "OpenStdin" 'yason:false
                              "HostConfig" host)))
      (when entrypoint (setf (gethash "Entrypoint" config) (coerce entrypoint 'vector)))
      (json-get (docker-json :post (format nil "/containers/create~@[?name=~a~]" (and name (url-encode name))) :json config) "Id"))))

(defun docker-start (id)
  (multiple-value-bind (status body) (docker-request :post (format nil "/containers/~a/start" id) :octets (ingot:make-octets 0))
    (unless (member status '(204 304))
      (docker-error status "starting ~a: ~a" id (and body (ingot:octets-to-string body))))
    t))

(defun docker-put-archive (id path tar)
  "Unpack TAR (octets) into PATH in the container."
  (multiple-value-bind (status body)
      (docker-request :put (format nil "/containers/~a/archive?path=~a&noOverwriteDirNonDir=false" id (url-encode path)) :octets tar)
    (unless (= status 200) (docker-error status "putting an archive into ~a: ~a" id (and body (ingot:octets-to-string body))))
    t))

(defun docker-get-archive (id path)
  "PATH in the container as a tar (octets), or nil when it is not there."
  (multiple-value-bind (status body) (docker-request :get (format nil "/containers/~a/archive?path=~a" id (url-encode path)))
    (and (= status 200) body)))

(defun docker-wait (id)
  "Block until the container exits; answers its exit code."
  (let ((answer (docker-json :post (format nil "/containers/~a/wait" id) :json (json-table))))
    (json-get answer "StatusCode" 1)))

(defun docker-logs (id on-text &key (follow t))
  "Read the container's output (stdout and stderr, demultiplexed),
calling ON-TEXT with each piece of text as it comes; with FOLLOW, until
the container stops."
  (let ((pending (ingot::make-buffer 256)))
    (flet ((feed (chunk)
             (ingot::buffer-write pending chunk)
             ;; frames: 8-byte header (type, 0, 0, 0, size as a big-endian 32) then the payload
             (loop
               (let ((have (fill-pointer pending)))
                 (when (< have 8) (return))
                 (let ((size (ingot::be32 pending 4)))
                   (when (< have (+ 8 size)) (return))
                   (funcall on-text (ingot:octets-to-string (subseq pending 8 (+ 8 size))))
                   (let ((rest (subseq pending (+ 8 size) have)))
                     (setf (fill-pointer pending) 0)
                     (ingot::buffer-write pending rest)))))))
      (docker-request :get (format nil "/containers/~a/logs?stdout=1&stderr=1&follow=~:[0~;1~]&timestamps=0" id follow)
                      :on-chunk #'feed)
      ;; a container with a TTY sends raw text; whatever is left is text
      (when (plusp (fill-pointer pending))
        (funcall on-text (ingot:octets-to-string (ingot::buffer-octets pending)))))))

(defun docker-kill (id)
  (ignore-errors (docker-request :post (format nil "/containers/~a/kill" id) :octets (ingot:make-octets 0))))

(defun docker-remove (id)
  (ignore-errors (docker-request :delete (format nil "/containers/~a?force=1&v=1" id))))

(defun docker-create-network (name)
  (json-get (docker-json :post "/networks/create" :json (json-table "Name" name "Driver" "bridge" "Internal" 'yason:false)) "Id"))

(defun docker-remove-network (id)
  (ignore-errors (docker-request :delete (format nil "/networks/~a" id))))

(defun docker-inspect (id)
  (docker-json :get (format nil "/containers/~a/json" id)))
