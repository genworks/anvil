;; Anvil's own pipeline, in Anvil's own form.  On a GitLab the
;; .gitlab-ci.yml beside this file does the same; on an Anvil this one
;; is read first.
(:pipeline
 :stages (:test)
 :default (:image "genworks/gendl:devo-ccl" :tags ("docker"))
 :variables (("ANVIL_DIR" "$CI_PROJECT_DIR")
             ("INGOT_DIR" "$CI_PROJECT_DIR/ingot")
             ("INGOT_ARCHIVE" "https://gitlab.common-lisp.net/clo/ingot/-/archive/master/ingot-master.tar.gz"))
 :jobs
 ((:cold-load-and-smoke
   :stage :test
   ;; Ingot is a repository of its own (clo/ingot), unpacked into ingot/
   ;; here with curl: the image carries no git (once Ingot is stable,
   ;; Ingot itself can fetch it)
   :before-script ("rm -rf \"$CI_PROJECT_DIR/ingot\" && curl -fsSL \"$INGOT_ARCHIVE\" | tar xz -C \"$CI_PROJECT_DIR\""
                   "mv \"$CI_PROJECT_DIR/ingot-master\" \"$CI_PROJECT_DIR/ingot\"")
   :lisp (:load ("ci/test.lisp")))))
