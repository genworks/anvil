;;;; lab-shim.lisp -- the lab's start-up file for the end-to-end rig
;;;; (README.md beside this file): clnet's real lab-init.lisp, then the
;;;; one thing Apache does in production -- /_git/ on the lab's name
;;;; reaches the gate.  Here the browser is sent to the gate's own name.
(load "/lab-init-real.lisp")
(in-package :gdl-user)
(setq prompt-lab::*git-approve-base* "http://cyclops/_git")
(format t "~&lab-shim: git approve base ~a~%" prompt-lab::*git-approve-base*)
