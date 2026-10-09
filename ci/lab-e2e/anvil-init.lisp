;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

;;;
;;; ci/lab-e2e/anvil-init.lisp: the forge of the lab end-to-end rig
;;; (README.md beside this file).  Loaded by the `anvil' service of
;;; compose.override.yml in a Gendl image that mounts this checkout at
;;; /anvil, Ingot's at /ingot, Monocle at /monocle and the Docker socket.  Its state is in
;;; the container's /tmp, so a restart of the container finds it again:
;;; everything below is idempotent.
;;;

(in-package :gdl-user)

(gendl:start-gendl!)
(load-quicklisp)

(asdf:load-asd "/ingot/ingot.asd")
(asdf:load-asd "/anvil/anvil.asd")
(pushnew #p"/monocle/" asdf:*central-registry* :test #'equal)
(handler-bind ((warning #'muffle-warning))
  (ql:quickload :anvil :silent t)
  (ql:quickload :monocle :silent t))

(setq anvil:*data-directory* #p"/tmp/anvil-e2e/"
      anvil:*site-url* "http://anvil:9095"
      anvil:*hosts* nil
      anvil:*docker-socket* "/var/run/docker.sock"
      anvil:*runner-tags* '("docker")
      anvil:*runner-parallelism* 2
      ;; the house: the lab on the same network, the secret the rig made
      anvil:*deploy-houses* '((:key :common-lisp.app :name "common-lisp.app"
                               :lab-url "http://lab:9080/prompt-lab"
                               :secret-file #p"/run/anvil-hosting-secret"
                               :domain "common-lisp.app")))

(anvil:start! :port 9095)

;; The user, a token for the flows, the project, and the gate's OAuth
;; application; printed for `run up' to read from the container's log.
(let* ((user (or (anvil::find-user "dave")
                 (anvil:make-user! "dave" "Dave" "dave@example.test" "password123" :admin? t)))
       (token (anvil::make-token! user "e2e" '("api"))))
  (unless (anvil::find-project "dave/hello-toll")
    (anvil:make-project! user :user "hello-toll" :name "Hello Toll" :creator user))
  (let ((app (find "clnet git gate" (anvil::records :oauth-app) :key (lambda (a) (getf a :name)) :test #'equal)))
    (if app
        (format t "~&TOKEN=~a~%CLIENT_ID=~a~%CLIENT_SECRET=(unchanged)~%" token (getf app :client-id))
        (multiple-value-bind (app secret)
            (anvil::make-oauth-app! "clnet git gate" '("http://cyclops/_git/callback") :owner user)
          (format t "~&TOKEN=~a~%CLIENT_ID=~a~%CLIENT_SECRET=~a~%" token (getf app :client-id) secret))))
  (format t "~&docker: ~a~%" (handler-case (anvil::docker-version) (error (e) e)))
  (finish-output))

(loop (sleep 60))
