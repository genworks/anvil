;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

;;;
;;; ci/runner-smoke.lisp: the forge with a real executor, for a smoke
;;; test driven from outside.  Run in a Gendl container that mounts the
;;; Docker socket and this checkout and publishes port 9098:
;;;
;;;   docker run -d --name anvil-runner-smoke --user 0 \
;;;     -v /var/run/docker.sock:/var/run/docker.sock \
;;;     -v /path/to/anvil:/anvil:ro -v /path/to/ingot:/ingot:ro -e HOME=/home/gendl-user \
;;;     -p 127.0.0.1:9098:9098 genworks/gendl:devo-ccl \
;;;     sh -c 'cd /opt/gendl && ./gdl/program/gdl-ccl -n -b --load /anvil/ci/runner-smoke.lisp'
;;;
;;; It starts the forge on 9098 with its state in /tmp, makes the user
;;; `smoke' with a token (printed as TOKEN=... in the container's log)
;;; and the empty project smoke/ci, then waits.  From the host, push a
;;; repository holding a ci.sexp to http://smoke:<token>@127.0.0.1:9098/smoke/ci.git
;;; and watch /api/v4/projects/1/pipelines.
;;;

(in-package :gdl-user)

(gendl:start-gendl!)
(load-quicklisp)

(asdf:load-asd "/ingot/ingot.asd")
(asdf:load-asd "/anvil/anvil.asd")
(handler-bind ((warning #'muffle-warning))
  (ql:quickload :anvil :silent t))

(setq anvil:*data-directory* #p"/tmp/anvil-smoke/"
      anvil:*site-url* (or (uiop:getenv "ANVIL_SITE_URL") "http://127.0.0.1:9098")
      anvil:*hosts* nil
      anvil:*docker-socket* "/var/run/docker.sock"
      anvil:*runner-tags* '("docker")
      anvil:*runner-parallelism* 2
      ;; ANVIL_READ_ONLY, non-empty, for a forge that only shows (README.md)
      anvil:*read-only?* (anvil:read-only-environment?))

(anvil:start! :port (or (ignore-errors (parse-integer (uiop:getenv "ANVIL_PORT"))) 9098))

(let* ((user (anvil:make-user! "smoke" "Smoke" "smoke@example.test" "password-of-smoke" :admin? t))
       (token (anvil::make-token! user "smoke" '("api"))))
  (anvil:make-project! user :user "ci" :name "CI smoke" :creator user)
  (format t "~&TOKEN=~a~%" token)
  (format t "~&docker: ~a~%" (handler-case (anvil::docker-version) (error (e) e)))
  (finish-output))

(loop (sleep 60))
