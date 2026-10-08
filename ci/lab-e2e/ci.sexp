;;; ci.sexp for the rig's test project (Monocle's hello-toll): one job
;;; in a container, and a :deploy job once the CI variable DEPLOY is on.
(:pipeline
 :stages (:test :deploy)
 :default (:image "alpine:3")
 :jobs
 ((:files :stage :test :script ("ls -la" "test -f image.sexp" "test -f monetize.sexp"))
  (:deploy :stage :deploy
           :rules ((:if (:and (:default-branch) (:variable "DEPLOY" "on"))))
           :deploy (:house :common-lisp.app))))
