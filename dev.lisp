;;;; -*- coding: utf-8 -*-
;;;;
;;;; Load Anvil into a Gendl image that can see this checkout and start
;;;; it on a port of its own:
;;;;
;;;;   (load "/path/to/anvil/dev.lisp")
;;;;
;;;; Then http://localhost:9095/ .  State goes under ANVIL_DATA_DIR, else
;;;; /projects/.state/anvil-dev/ when that tree exists, else /tmp/anvil-dev/.
;;;; The first account made is an administrator (make one at
;;;; /users/sign_up, or with anvil:make-user! at the REPL).

(in-package :gdl-user)

(unless (find-package :quicklisp) (load-quicklisp))

(let ((here (make-pathname :name nil :type nil :defaults (or *load-truename* *compile-file-truename*))))
  (pushnew (merge-pathnames "ingot/" here) asdf:*central-registry* :test #'equal)
  (pushnew here asdf:*central-registry* :test #'equal))

(ql:quickload :anvil :silent t)

(let ((data (or (uiop:getenv "ANVIL_DATA_DIR")
                (and (uiop:directory-exists-p "/projects/.state/") "/projects/.state/anvil-dev/")
                "/tmp/anvil-dev/")))
  (setq anvil:*data-directory* (uiop:ensure-directory-pathname data)))

(setq anvil:*site-url* (or (uiop:getenv "ANVIL_SITE_URL") "http://localhost:9095"))

;; a Docker socket, when the image has one, gives the runner its executor
(setq anvil:*docker-socket* (and (probe-file "/var/run/docker.sock") "/var/run/docker.sock"))

(anvil:start! :port (or (ignore-errors (parse-integer (uiop:getenv "ANVIL_PORT"))) 9095))

(format t "~&Anvil is at ~a (data in ~a)~%" anvil:*site-url* anvil:*data-directory*)
