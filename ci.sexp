;; Anvil's own pipeline, in Anvil's own form.  On a GitLab the
;; .gitlab-ci.yml beside this file does the same; on an Anvil this one
;; is read first.
(:pipeline
 :stages (:test)
 :default (:image "genworks/gendl:devo-ccl" :tags ("docker"))
 :variables (("ANVIL_DIR" "$CI_PROJECT_DIR"))
 :jobs
 ((:cold-load-and-smoke
   :stage :test
   :lisp (:load ("ci/test.lisp")))))
