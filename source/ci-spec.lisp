;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :anvil)

;;
;; The pipeline specification: a project's ci.sexp (Anvil's own form) or
;; its .gitlab-ci.yml, compiled to one shape -- a plist with :stages,
;; :variables, :workflow-rules and :jobs, each job a plist -- and then
;; the jobs chosen for one pipeline by evaluating their rules against
;; the push.
;;
;; A job, compiled:
;;   (:name "test" :stage "test" :image "..." :entrypoint nil :services (...)
;;    :before-script (...) :script (...) :after-script (...)
;;    :variables (("K" . "v") ...) :tags (...) :when :on-success|:manual|:always|:never|:on-failure
;;    :allow-failure nil :needs ("other" ...) :dependencies nil|("other" ...)
;;    :artifacts (:paths (...) :exclude (...) :expire-in "1 week" :when :on-success :name nil)
;;    :cache (:key "..." :paths (...)) :rules (...) :only (...) :except (...)
;;    :resource-group nil :environment (:name "production" :url "...")
;;    :timeout nil :retry 0 :kind :script|:deploy|:lisp :deploy (:house :common-lisp.app)
;;    :git-strategy :fetch|:none)
;;

(define-condition ci-spec-error (error)
  ((text :initarg :text :reader ci-spec-error-text))
  (:report (lambda (c s) (write-string (ci-spec-error-text c) s))))

(defun spec-error (control &rest args)
  (error 'ci-spec-error :text (apply #'format nil control args)))

(defparameter *reserved-yaml-keys*
  '("stages" "variables" "default" "include" "image" "services" "before_script" "after_script" "cache" "workflow" "types" "pages"))

(defparameter *default-stages* '(".pre" "build" "test" "deploy" ".post"))

(defparameter *job-keys*
  '("stage" "image" "services" "before_script" "script" "after_script" "variables" "tags" "when" "allow_failure"
    "needs" "dependencies" "artifacts" "cache" "rules" "only" "except" "extends" "resource_group" "environment"
    "timeout" "retry" "interruptible" "coverage" "parallel" "trigger" "inherit" "release" "secrets" "id_tokens" "hooks"
    "dast_configuration" "publish" "manual_confirmation"))

;;; Reading .gitlab-ci.yml

(defun read-gitlab-ci (text &key read-file)
  "The compiled spec of a .gitlab-ci.yml TEXT.  READ-FILE is a function
of a path answering that file's text from the repository, for
include: local."
  (let ((doc (handler-case (parse-yaml text)
               (yaml-error (e) (spec-error ".gitlab-ci.yml: ~a" e)))))
    (unless (ymap-p doc) (spec-error ".gitlab-ci.yml is not a mapping."))
    (setq doc (resolve-includes doc read-file 0))
    (let* ((defaults (yget doc "default" (make-ymap)))
           (global-image (or (yget doc "image") (yget defaults "image")))
           (global-services (or (yget doc "services") (yget defaults "services")))
           (global-before (or (yget doc "before_script") (yget defaults "before_script")))
           (global-after (or (yget doc "after_script") (yget defaults "after_script")))
           (global-cache (or (yget doc "cache") (yget defaults "cache")))
           (global-tags (yget defaults "tags"))
           (global-artifacts (yget defaults "artifacts"))
           (global-timeout (yget defaults "timeout"))
           (global-retry (yget defaults "retry"))
           (stages (let ((given (ylist (or (yget doc "stages") (yget doc "types")))))
                     (if given
                         (append '(".pre") (remove-if (lambda (s) (member s '(".pre" ".post") :test #'equal))
                                                       (mapcar #'ystring given))
                                 '(".post"))
                         *default-stages*)))
           (variables (read-variables (yget doc "variables")))
           (workflow (let ((w (yget doc "workflow")))
                       (and (ymap-p w) (mapcar #'read-rule (ylist (yget w "rules"))))))
           (jobs nil))
      (dolist (pair (ymap-pairs doc))
        (let ((name (car pair)) (body (cdr pair)))
          (unless (or (member name *reserved-yaml-keys* :test #'string=)
                      (not (ymap-p body)))
            (unless (starts-with? "." name)
              (let ((resolved (resolve-extends body doc name 0)))
                (push (compile-yaml-job name resolved
                                        :image global-image :services global-services
                                        :before global-before :after global-after :cache global-cache
                                        :tags global-tags :artifacts global-artifacts
                                        :timeout global-timeout :retry global-retry
                                        :stages stages)
                      jobs))))))
      (setq jobs (nreverse jobs))
      (when (null jobs) (spec-error ".gitlab-ci.yml defines no jobs."))
      (list :source :gitlab-ci :stages stages :variables variables :workflow-rules workflow :jobs jobs))))

(defun resolve-includes (doc read-file depth)
  "DOC with every include: local merged in (the including file's keys win)."
  (when (> depth 5) (spec-error "include: too deep."))
  (let ((includes (yget doc "include")))
    (when includes
      (dolist (entry (ylist includes))
        (let ((path (cond ((stringp entry) entry)
                          ((ymap-p entry) (or (yget entry "local")
                                              (spec-error "include: only `local' is supported, not ~{~a~^, ~}." (ykeys entry))))
                          (t (spec-error "include: ~s" entry)))))
          (unless read-file (spec-error "include: local needs a repository to read from."))
          (let ((text (funcall read-file (string-left-trim "/" path))))
            (unless text (spec-error "include: no file ~a in the repository." path))
            (let ((included (resolve-includes (handler-case (parse-yaml text)
                                                (yaml-error (e) (spec-error "~a: ~a" path e)))
                                              read-file (1+ depth))))
              (dolist (pair (ymap-pairs included))
                (unless (yhas? doc (car pair))
                  (yset doc (car pair) (cdr pair)))))))))
    doc))

(defun resolve-extends (body doc name depth)
  "BODY with its `extends' parents merged under it (a deep merge of
mappings; the child's value wins for anything else)."
  (when (> depth 10) (spec-error "~a: extends too deep." name))
  (let ((parents (ylist (yget body "extends"))))
    (if (null parents)
        body
        (let ((result (make-ymap)))
          (dolist (parent-name (mapcar #'ystring parents))
            (let ((parent (yget doc parent-name)))
              (unless (ymap-p parent) (spec-error "~a extends ~a, which is not a job." name parent-name))
              (setq result (deep-merge result (resolve-extends parent doc parent-name (1+ depth))))))
          (let ((merged (deep-merge result body)))
            (setf (ymap-pairs merged) (remove "extends" (ymap-pairs merged) :key #'car :test #'equal))
            merged)))))

(defun deep-merge (base over)
  "A new ymap: OVER's keys on BASE's, mappings merged within."
  (let ((result (make-ymap (mapcar (lambda (p) (cons (car p) (cdr p))) (ymap-pairs base)))))
    (dolist (pair (ymap-pairs over) result)
      (let ((existing (and (yhas? result (car pair)) (yget result (car pair)))))
        (yset result (car pair)
              (if (and (ymap-p existing) (ymap-p (cdr pair)))
                  (deep-merge existing (cdr pair))
                  (cdr pair)))))))

(defun read-variables (value)
  "An alist of (name . value) from a variables mapping (a value may be
a mapping with `value' and `description')."
  (when (ymap-p value)
    (loop for (key . v) in (ymap-pairs value)
          collect (cons key (or (ystring (if (ymap-p v) (yget v "value") v)) "")))))

(defun read-image (value)
  "Two values, the image name and its entrypoint (a list or nil)."
  (cond ((stringp value) (values value nil))
        ((ymap-p value) (values (ystring (yget value "name")) (ylist (yget value "entrypoint"))))
        (t (values nil nil))))

(defun read-script (value)
  "A script as a list of lines; a nested list is flattened."
  (loop for item in (ylist value)
        append (if (listp item) (read-script item) (list (or (ystring item) "")))))

(defun read-when (value &optional (default :on-success))
  (let ((text (ystring value)))
    (cond ((null text) default)
          ((string= text "on_success") :on-success)
          ((string= text "on_failure") :on-failure)
          ((string= text "always") :always)
          ((string= text "manual") :manual)
          ((string= text "never") :never)
          ((string= text "delayed") :delayed)
          (t (spec-error "when: ~a is not a value." text)))))

(defun read-allow-failure (value)
  (cond ((ymap-p value) t)             ; exit_codes: close enough
        (t (ytrue? value))))

(defun read-artifacts (value)
  (when (ymap-p value)
    (list :paths (mapcar #'ystring (ylist (yget value "paths")))
          :exclude (mapcar #'ystring (ylist (yget value "exclude")))
          :expire-in (ystring (yget value "expire_in"))
          :when (read-when (yget value "when") :on-success)
          :name (ystring (yget value "name"))
          :untracked (ytrue? (yget value "untracked")))))

(defun read-cache (value)
  (let ((entries (ylist value)))
    (loop for entry in entries
          when (ymap-p entry)
            collect (list :key (let ((key (yget entry "key")))
                                 (cond ((stringp key) key)
                                       ((ymap-p key) (format nil "files:~{~a~^,~}" (mapcar #'ystring (ylist (yget key "files")))))
                                       (t "default")))
                          :paths (mapcar #'ystring (ylist (yget entry "paths")))
                          :policy (or (ystring (yget entry "policy")) "pull-push")))))

(defun read-needs (value)
  (loop for entry in (ylist value)
        collect (cond ((stringp entry) entry)
                      ((ymap-p entry) (ystring (yget entry "job")))
                      (t (ystring entry)))))

(defun read-rule (value)
  "A rule as a plist: :if (a string, GitLab's syntax) :changes :exists
:when :allow-failure :variables."
  (cond ((ymap-p value)
         (list :if (ystring (yget value "if"))
               :changes (mapcar #'ystring (ylist (let ((c (yget value "changes"))) (if (ymap-p c) (yget c "paths") c))))
               :exists (mapcar #'ystring (ylist (yget value "exists")))
               :when (and (yhas? value "when") (read-when (yget value "when")))
               :allow-failure (and (yhas? value "allow_failure") (read-allow-failure (yget value "allow_failure")))
               :variables (read-variables (yget value "variables"))))
        (t (spec-error "rules: each rule is a mapping."))))

(defun read-only-except (value)
  "only/except, as a plist :refs :variables :changes; a list is refs."
  (cond ((null value) nil)
        ((eq value :null) nil)
        ((listp value) (list :refs (mapcar #'ystring value)))
        ((ymap-p value) (list :refs (mapcar #'ystring (ylist (yget value "refs")))
                              :variables (mapcar #'ystring (ylist (yget value "variables")))
                              :changes (mapcar #'ystring (ylist (yget value "changes")))))
        (t nil)))

(defun read-environment (value)
  (cond ((stringp value) (list :name value))
        ((ymap-p value) (list :name (ystring (yget value "name")) :url (ystring (yget value "url"))
                              :action (ystring (yget value "action"))))
        (t nil)))

(defun compile-yaml-job (name body &key image services before after cache tags artifacts timeout retry stages)
  (dolist (key (ykeys body))
    (unless (member key *job-keys* :test #'string=)
      (spec-error "~a: `~a' is not a job keyword." name key)))
  (dolist (unsupported '("trigger" "parallel" "release" "secrets" "id_tokens"))
    (when (yhas? body unsupported) (spec-error "~a: `~a' is not supported here." name unsupported)))
  (multiple-value-bind (image-name entrypoint) (read-image (if (yhas? body "image") (yget body "image") image))
    (let* ((stage (or (ystring (yget body "stage")) "test"))
           (script (read-script (yget body "script")))
           (environment (read-environment (yget body "environment")))
           (deploy-house (and environment (deploy-house-for-environment (getf environment :name)))))
      (unless (member stage stages :test #'string=)
        (spec-error "~a: stage ~a is not among the stages (~{~a~^, ~})." name stage stages))
      (when (and (null script) (not deploy-house) (not (yhas? body "trigger")))
        (spec-error "~a: no script." name))
      (list :name name :stage stage
            :image image-name :entrypoint entrypoint
            :services (read-services (if (yhas? body "services") (yget body "services") services))
            :before-script (read-script (if (yhas? body "before_script") (yget body "before_script") before))
            :script script
            :after-script (read-script (if (yhas? body "after_script") (yget body "after_script") after))
            :variables (read-variables (yget body "variables"))
            :tags (mapcar #'ystring (ylist (if (yhas? body "tags") (yget body "tags") tags)))
            :when (read-when (yget body "when"))
            :allow-failure (read-allow-failure (yget body "allow_failure"))
            :needs (and (yhas? body "needs") (read-needs (yget body "needs")))
            :needs-given? (yhas? body "needs")
            :dependencies (and (yhas? body "dependencies") (mapcar #'ystring (ylist (yget body "dependencies"))))
            :dependencies-given? (yhas? body "dependencies")
            :artifacts (read-artifacts (if (yhas? body "artifacts") (yget body "artifacts") artifacts))
            :cache (read-cache (if (yhas? body "cache") (yget body "cache") cache))
            :rules (and (yhas? body "rules") (mapcar #'read-rule (ylist (yget body "rules"))))
            :only (read-only-except (yget body "only"))
            :except (read-only-except (yget body "except"))
            :resource-group (ystring (yget body "resource_group"))
            :environment environment
            :timeout (parse-duration (ystring (if (yhas? body "timeout") (yget body "timeout") timeout)))
            :retry (let ((r (if (yhas? body "retry") (yget body "retry") retry)))
                     (cond ((integerp r) r) ((ymap-p r) (or (yget r "max") 0)) (t 0)))
            :git-strategy (let ((v (cdr (assoc "GIT_STRATEGY" (read-variables (yget body "variables")) :test #'string=))))
                            (if (equal v "none") :none :fetch))
            :kind (if deploy-house :deploy :script)
            :deploy (and deploy-house (list :house deploy-house))))))

(defun read-services (value)
  (loop for entry in (ylist value)
        collect (cond ((stringp entry) (list :name entry :alias (service-alias entry)))
                      ((ymap-p entry) (list :name (ystring (yget entry "name"))
                                            :alias (or (ystring (yget entry "alias")) (service-alias (ystring (yget entry "name"))))
                                            :command (mapcar #'ystring (ylist (yget entry "command")))
                                            :variables (read-variables (yget entry "variables"))))
                      (t nil))))

(defun service-alias (image)
  "docker.io/library/postgres:15 -> postgres, as GitLab names a service."
  (let* ((no-tag (subseq image 0 (or (position #\: image :start (or (position #\/ image :from-end t) 0)) (length image))))
         (last (subseq no-tag (1+ (or (position #\/ no-tag :from-end t) -1)))))
    last))

(defun deploy-house-for-environment (name)
  "The key of a deploy house whose name or domain is the environment's
NAME, or nil."
  (and name
       (let ((house (find-if (lambda (h) (or (string-equal name (getf h :name)) (string-equal name (getf h :domain))
                                             (string-equal name (symbol-name (getf h :key)))))
                             *deploy-houses*)))
         (and house (getf house :key)))))

(defun parse-duration (text)
  "Seconds from \"1 hour 30 minutes\", \"90 min\", \"2h\", or nil."
  (when (and (stringp text) (plusp (length text)))
    (let ((total 0) (found nil))
      (ppcre:do-register-groups (number unit) ("([0-9]+)\\s*([a-zA-Z]*)" text)
        (let ((n (parse-integer number))
              (unit (string-downcase unit)))
          (setq found t)
          (incf total (* n (cond ((or (string= unit "") (starts-with? "s" unit)) 1)
                                 ((starts-with? "mo" unit) (* 30 86400))
                                 ((starts-with? "m" unit) 60)
                                 ((starts-with? "h" unit) 3600)
                                 ((starts-with? "d" unit) 86400)
                                 ((starts-with? "w" unit) (* 7 86400))
                                 ((starts-with? "y" unit) (* 365 86400))
                                 (t 1))))))
      (and found total))))

;;; Reading ci.sexp

(defparameter *sexp-pipeline-keys* '(:stages :variables :default :workflow :jobs :include))
(defparameter *sexp-job-keys*
  '(:stage :image :entrypoint :services :before-script :script :after-script :variables :tags :when :allow-failure
    :needs :dependencies :artifacts :cache :rules :only :except :resource-group :environment :timeout :retry
    :lisp :deploy :extends :git-strategy))

(defun read-ci-sexp (text)
  "The compiled spec of a ci.sexp TEXT: one form, (:pipeline ...), read
with evaluation off."
  (let ((form (handler-case
                  (with-standard-io-syntax
                    (let ((*read-eval* nil) (*package* (find-package :keyword)))
                      (with-input-from-string (in text)
                        (let ((form (read in nil nil)))
                          (when (read in nil nil) (spec-error "ci.sexp holds more than one form."))
                          form))))
                (error (e) (spec-error "ci.sexp does not read: ~a" e)))))
    (unless (and (consp form) (eq (first form) :pipeline) (plist? (rest form)))
      (spec-error "ci.sexp is (:pipeline <key> <value> ...)."))
    (let ((fields (rest form)))
      (loop for key in fields by #'cddr
            unless (member key *sexp-pipeline-keys*) do (spec-error "ci.sexp: ~s is not a pipeline field." key))
      (let* ((defaults (getf fields :default))
             (stages (let ((given (getf fields :stages)))
                       (if given
                           (append '(".pre") (remove-if (lambda (s) (member s '(".pre" ".post") :test #'equal))
                                                         (mapcar #'name-string given))
                                   '(".post"))
                           *default-stages*)))
             (jobs (loop for job in (getf fields :jobs)
                         collect (compile-sexp-job job defaults stages (getf fields :jobs)))))
        (when (null jobs) (spec-error "ci.sexp defines no jobs."))
        (list :source :ci-sexp :stages stages
              :variables (sexp-variables (getf fields :variables))
              :workflow-rules (mapcar #'sexp-rule (getf (getf fields :workflow) :rules))
              :jobs jobs)))))

(defun plist? (thing)
  (and (listp thing) (evenp (length thing)) (loop for key in thing by #'cddr always (keywordp key))))

(defun name-string (thing)
  (cond ((stringp thing) thing)
        ((symbolp thing) (string-downcase (symbol-name thing)))
        (t (princ-to-string thing))))

(defun sexp-variables (value)
  "An alist from ((\"K\" \"v\") ...) or (:k \"v\" ...)."
  (cond ((null value) nil)
        ((plist? value) (loop for (k v) on value by #'cddr collect (cons (string-upcase (name-string k)) (princ-to-string v))))
        (t (loop for entry in value collect (cons (name-string (first entry)) (princ-to-string (second entry)))))))

(defun sexp-rule (rule)
  "A rule from (:if EXPR :when :always :allow-failure t) -- EXPR a string
in GitLab's syntax or a form (:and (:branch \"master\") ...)."
  (unless (plist? rule) (spec-error "a rule is a plist: ~s" rule))
  (list :if (getf rule :if) :changes (getf rule :changes) :exists (getf rule :exists)
        :when (and (getf rule :when) (read-when (name-string (getf rule :when))))
        :allow-failure (getf rule :allow-failure) :variables (sexp-variables (getf rule :variables))))

(defun compile-sexp-job (job defaults stages all-jobs &optional (depth 0))
  (unless (and (consp job) (plist? (rest job))) (spec-error "a job is (:name <key> <value> ...): ~s" job))
  (when (> depth 10) (spec-error "~a: extends too deep." (first job)))
  (let* ((name (name-string (first job)))
         (fields (rest job)))
    (loop for key in fields by #'cddr
          unless (member key *sexp-job-keys*) do (spec-error "~a: ~s is not a job field." name key))
    ;; :extends another job's fields under these
    (let ((parent-name (getf fields :extends)))
      (when parent-name
        (let ((parent (find (name-string parent-name) all-jobs :key (lambda (j) (name-string (first j))) :test #'string=)))
          (unless parent (spec-error "~a extends ~a, which is not a job." name parent-name))
          (let ((merged (copy-list (rest (compile-sexp-job-raw parent)))))
            (loop for (k v) on fields by #'cddr unless (eq k :extends) do (setf (getf merged k) v))
            (setq fields merged)))))
    (flet ((field (key &optional default) (let ((v (getf fields key :unset))) (if (eq v :unset) (getf defaults key default) v)))
           (own (key) (getf fields key)))
      (let* ((lisp (own :lisp))
             (deploy (own :deploy))
             (stage (name-string (or (own :stage) (if deploy "deploy" "test"))))
             (image (field :image))
             (script (or (mapcar #'princ-to-string (own :script))
                         (and lisp (lisp-job-script lisp))))
             (environment (let ((e (field :environment)))
                            (cond ((null e) (and deploy (list :name (name-string (getf deploy :house)))))
                                  ((stringp e) (list :name e))
                                  (t (list :name (name-string (getf e :name)) :url (getf e :url)))))))
        (unless (member stage stages :test #'string=)
          (spec-error "~a: stage ~a is not among the stages (~{~a~^, ~})." name stage stages))
        (when (and (null script) (null deploy)) (spec-error "~a: no script." name))
        (when deploy
          (unless (plist? deploy) (spec-error "~a: :deploy is (:house <key>)." name))
          (unless (find (getf deploy :house) *deploy-houses* :key (lambda (h) (getf h :key)))
            (spec-error "~a: no deploy house ~s on this forge." name (getf deploy :house))))
        (list :name name :stage stage
              :image (and image (name-string image)) :entrypoint (field :entrypoint)
              :services (loop for s in (field :services)
                              collect (if (stringp s) (list :name s :alias (service-alias s))
                                          (list :name (getf s :name) :alias (or (getf s :alias) (service-alias (getf s :name)))
                                                :command (getf s :command) :variables (sexp-variables (getf s :variables)))))
              :before-script (mapcar #'princ-to-string (field :before-script))
              :script (or script nil)
              :after-script (mapcar #'princ-to-string (field :after-script))
              :variables (sexp-variables (own :variables))
              :tags (mapcar #'name-string (field :tags))
              :when (read-when (and (own :when) (name-string (own :when))))
              :allow-failure (and (own :allow-failure) t)
              :needs (mapcar #'name-string (own :needs)) :needs-given? (and (getf fields :needs) t)
              :dependencies (mapcar #'name-string (own :dependencies)) :dependencies-given? (and (member :dependencies fields) t)
              :artifacts (let ((a (field :artifacts)))
                           (and a (list :paths (mapcar #'princ-to-string (getf a :paths))
                                        :exclude (mapcar #'princ-to-string (getf a :exclude))
                                        :expire-in (and (getf a :expire-in) (princ-to-string (getf a :expire-in)))
                                        :when (read-when (and (getf a :when) (name-string (getf a :when))))
                                        :name (getf a :name))))
              :cache (let ((c (field :cache)))
                       (and c (list (list :key (or (getf c :key) "default") :paths (mapcar #'princ-to-string (getf c :paths))
                                          :policy (or (getf c :policy) "pull-push")))))
              :rules (mapcar #'sexp-rule (own :rules))
              :only (let ((o (own :only))) (and o (if (plist? o) (list :refs (mapcar #'name-string (getf o :refs))
                                                                        :variables (getf o :variables))
                                                      (list :refs (mapcar #'name-string o)))))
              :except (let ((o (own :except))) (and o (if (plist? o) (list :refs (mapcar #'name-string (getf o :refs))
                                                                            :variables (getf o :variables))
                                                          (list :refs (mapcar #'name-string o)))))
              :resource-group (and (own :resource-group) (name-string (own :resource-group)))
              :environment environment
              :timeout (let ((timeout (field :timeout))) (if (integerp timeout) timeout (parse-duration (and timeout (princ-to-string timeout)))))
              :retry (or (own :retry) 0)
              :git-strategy (if (eq (field :git-strategy) :none) :none :fetch)
              :kind (cond (deploy :deploy) (lisp :lisp) (t :script))
              :lisp lisp
              :deploy deploy)))))

(defun compile-sexp-job-raw (job) job)

(defun lisp-job-script (lisp)
  "The shell lines of a :lisp job: (:load (\"ci/test.lisp\") :eval
\"(form)\" :lisp \"ccl\") -- the Gendl image's own program, else
a Lisp named in the image, with each file loaded in order."
  (unless (plist? lisp) (spec-error ":lisp is a plist: (:load (...) :eval ...)."))
  (let ((files (mapcar #'princ-to-string (getf lisp :load)))
        (evals (mapcar (lambda (e) (if (stringp e) e (let ((*print-case* :downcase)) (prin1-to-string e)))) (getf lisp :eval)))
        (program (getf lisp :program)))
    (list (format nil "~a~{ --load \"$CI_PROJECT_DIR/~a\"~}~{ --eval ~s~}"
                  (or program "cd /opt/gendl && ./gdl/program/gdl-$ANVIL_LISP -n -b")
                  files evals))))

;;; Reading whatever the tree has

(defun read-spec-from-tree (repository commit-oid)
  "Two values: the compiled spec from the commit's ci.sexp or
.gitlab-ci.yml (ci.sexp first), and which file it came from; nil and
nil when the tree has neither.  Signals ci-spec-error for a file that
does not compile."
  (flet ((text-at (path)
           (let ((data (ingot:blob-at repository commit-oid path)))
             (and data (ingot:octets-to-string data)))))
    (let ((sexp (text-at "ci.sexp")))
      (if sexp
          (values (read-ci-sexp sexp) "ci.sexp")
          (let ((yaml (text-at ".gitlab-ci.yml")))
            (if yaml
                (values (read-gitlab-ci yaml :read-file #'text-at) ".gitlab-ci.yml")
                (values nil nil)))))))

;;; Choosing the jobs for a pipeline: rules, only, except, workflow

(defun variable-value (name context)
  "The value of $NAME in CONTEXT's variables (an alist), or nil."
  (cdr (assoc name (getf context :variables) :test #'string=)))

(defun eval-rule-if (expression context)
  "Whether EXPRESSION holds in CONTEXT: a string in GitLab's syntax
($VAR == \"x\", &&, ||, =~ /re/, parentheses, null), or a form
((:and ...) (:or ...) (:not ...) (:branch \"x\") (:tag [\"x\"])
(:variable \"X\" [\"v\"]) (:source \"push\") (:default-branch))."
  (cond ((null expression) t)
        ((stringp expression) (eval-gitlab-expression expression context))
        ((consp expression)
         (case (first expression)
           (:and (every (lambda (e) (eval-rule-if e context)) (rest expression)))
           (:or (some (lambda (e) (eval-rule-if e context)) (rest expression)))
           (:not (not (eval-rule-if (second expression) context)))
           (:branch (and (getf context :branch)
                         (or (null (second expression)) (ref-matches? (second expression) (getf context :branch)))))
           (:tag (and (getf context :tag)
                      (or (null (second expression)) (ref-matches? (second expression) (getf context :tag)))))
           (:default-branch (and (getf context :branch) (equal (getf context :branch) (getf context :default-branch))))
           (:source (string-equal (name-string (second expression)) (name-string (getf context :source))))
           (:variable (let ((value (variable-value (string-upcase (name-string (second expression))) context)))
                        (if (cddr expression) (equal value (princ-to-string (third expression))) (and value t))))
           (t (spec-error "a rule's :if does not understand ~s." (first expression)))))
        (t (spec-error "a rule's :if is a string or a form: ~s" expression))))

(defun ref-matches? (pattern ref)
  "PATTERN against REF: a plain name, or /regexp/."
  (let ((pattern (name-string pattern)))
    (if (and (> (length pattern) 1) (char= (char pattern 0) #\/) (char= (char pattern (1- (length pattern))) #\/))
        (and (ppcre:scan (subseq pattern 1 (1- (length pattern))) ref) t)
        (string= pattern ref))))

;;; GitLab's expression syntax

(defun tokenize-expression (text)
  (let ((tokens nil) (at 0) (n (length text)))
    (loop while (< at n)
          do (let ((c (char text at)))
               (cond ((member c '(#\Space #\Tab)) (incf at))
                     ((char= c #\$)
                      (let ((end (or (position-if-not (lambda (ch) (or (alphanumericp ch) (char= ch #\_))) text :start (1+ at)) n)))
                        (push (list :variable (subseq text (1+ at) end)) tokens)
                        (setq at end)))
                     ((char= c #\")
                      (let ((close (position #\" text :start (1+ at))))
                        (unless close (spec-error "an unclosed string in ~a" text))
                        (push (list :string (subseq text (1+ at) close)) tokens)
                        (setq at (1+ close))))
                     ((char= c #\')
                      (let ((close (position #\' text :start (1+ at))))
                        (unless close (spec-error "an unclosed string in ~a" text))
                        (push (list :string (subseq text (1+ at) close)) tokens)
                        (setq at (1+ close))))
                     ((char= c #\/)
                      (let ((close (loop for i from (1+ at) below n
                                         when (and (char= (char text i) #\/) (char/= (char text (1- i)) #\\)) return i)))
                        (unless close (spec-error "an unclosed /regexp/ in ~a" text))
                        (let ((flags-end (or (position-if-not #'alpha-char-p text :start (1+ close)) n)))
                          (push (list :regexp (subseq text (1+ at) close) (subseq text (1+ close) flags-end)) tokens)
                          (setq at flags-end))))
                     ((char= c #\() (push (list :open) tokens) (incf at))
                     ((char= c #\)) (push (list :close) tokens) (incf at))
                     ((and (char= c #\&) (< (1+ at) n) (char= (char text (1+ at)) #\&)) (push (list :and) tokens) (incf at 2))
                     ((and (char= c #\|) (< (1+ at) n) (char= (char text (1+ at)) #\|)) (push (list :or) tokens) (incf at 2))
                     ((and (char= c #\=) (< (1+ at) n) (char= (char text (1+ at)) #\=)) (push (list :eq) tokens) (incf at 2))
                     ((and (char= c #\!) (< (1+ at) n) (char= (char text (1+ at)) #\=)) (push (list :ne) tokens) (incf at 2))
                     ((and (char= c #\=) (< (1+ at) n) (char= (char text (1+ at)) #\~)) (push (list :match) tokens) (incf at 2))
                     ((and (char= c #\!) (< (1+ at) n) (char= (char text (1+ at)) #\~)) (push (list :no-match) tokens) (incf at 2))
                     ((alpha-char-p c)
                      (let ((end (or (position-if-not #'alphanumericp text :start at) n)))
                        (let ((word (subseq text at end)))
                          (cond ((string= word "null") (push (list :null) tokens))
                                (t (spec-error "~a is not a word of a rule's if (~a)." word text))))
                        (setq at end)))
                     (t (spec-error "a rule's if does not read: ~a" text)))))
    (nreverse tokens)))

(defun eval-gitlab-expression (text context)
  (let ((tokens (tokenize-expression text)))
    (labels ((peek () (first tokens))
             (next () (pop tokens))
             (parse-or ()
               (let ((left (parse-and)))
                 (loop while (eq (first (peek)) :or)
                       do (next) (let ((right (parse-and))) (setq left (or left right))))
                 left))
             (parse-and ()
               (let ((left (parse-comparison)))
                 (loop while (eq (first (peek)) :and)
                       do (next) (let ((right (parse-comparison))) (setq left (and left right))))
                 left))
             (parse-comparison ()
               (let ((left (parse-primary)))
                 (case (first (peek))
                   (:eq (next) (equal (value-of left) (value-of (parse-primary))))
                   (:ne (next) (not (equal (value-of left) (value-of (parse-primary)))))
                   (:match (next) (match? (value-of left) (parse-primary)))
                   (:no-match (next) (not (match? (value-of left) (parse-primary))))
                   (t (truthy? left)))))
             (parse-primary ()
               (let ((token (next)))
                 (case (first token)
                   (:open (let ((value (parse-or)))
                            (unless (eq (first (next)) :close) (spec-error "a missing ) in ~a" text))
                            (list :value value)))
                   ((:variable :string :regexp :null) token)
                   (t (spec-error "a rule's if does not parse: ~a" text)))))
             (value-of (token)
               (case (first token)
                 (:variable (variable-value (second token) context))
                 (:string (second token))
                 (:null nil)
                 (:value (second token))
                 (t (spec-error "a regexp where a value was expected in ~a" text))))
             (truthy? (token)
               (case (first token)
                 (:variable (let ((v (variable-value (second token) context))) (and v (plusp (length v)))))
                 (:string (plusp (length (second token))))
                 (:null nil)
                 (:value (second token))
                 (t nil)))
             (match? (value token)
               (and (stringp value)
                    (if (eq (first token) :regexp)
                        (let ((pattern (if (find #\i (third token)) (strcat "(?i)" (second token)) (second token))))
                          (and (ppcre:scan pattern value) t))
                        (let ((v (value-of token)))
                          (and (stringp v) (ppcre:scan v value) t))))))
      (let ((result (parse-or)))
        (when tokens (spec-error "a rule's if has text left over: ~a" text))
        result))))

;;; only/except

(defun only-except-matches? (clause context)
  "Whether CLAUSE (:refs :variables) holds for CONTEXT, as `only' means it."
  (let ((refs (getf clause :refs)) (variables (getf clause :variables)))
    (and (or (null refs)
             (some (lambda (ref)
                     (cond ((string= ref "branches") (and (getf context :branch) t))
                           ((string= ref "tags") (and (getf context :tag) t))
                           ((member ref '("pushes" "web" "api" "schedules" "triggers" "pipelines" "merge_requests" "external") :test #'string=)
                            (string-equal (name-string (getf context :source)) (string-right-trim "es" ref)))
                           (t (ref-matches? ref (getf context :ref)))))
                   refs))
         (or (null variables)
             (some (lambda (v) (eval-gitlab-expression v context)) variables)))))

(defun job-decision (job context)
  "Two values: :when for JOB in CONTEXT (nil when the job is left out of
the pipeline), and whether failure is allowed."
  (let ((rules (getf job :rules)))
    (cond
      (rules
       (dolist (rule rules (values nil nil))
         (when (and (eval-rule-if (getf rule :if) context)
                    (or (null (getf rule :exists))
                        (some (lambda (pattern) (path-exists? pattern context)) (getf rule :exists)))
                    (or (null (getf rule :changes))
                        (some (lambda (pattern) (path-changed? pattern context)) (getf rule :changes))))
           (let ((when (or (getf rule :when) (getf job :when))))
             (return (if (eq when :never)
                         (values nil nil)
                         (values when (if (member :allow-failure rule) (getf rule :allow-failure) (getf job :allow-failure)))))))))
      (t
       (let ((only (getf job :only)) (except (getf job :except)))
         (if (and (or (null only) (only-except-matches? only context))
                  (or (null except) (not (only-except-matches? except context))))
             (if (eq (getf job :when) :never) (values nil nil) (values (getf job :when) (getf job :allow-failure)))
             (values nil nil)))))))

(defun glob-to-regexp (pattern)
  (with-output-to-string (out)
    (write-string "^" out)
    (loop with i = 0
          while (< i (length pattern))
          do (let ((c (char pattern i)))
               (cond ((and (char= c #\*) (< (1+ i) (length pattern)) (char= (char pattern (1+ i)) #\*))
                      (write-string ".*" out) (incf i 2)
                      (when (and (< i (length pattern)) (char= (char pattern i) #\/)) (incf i)))
                     ((char= c #\*) (write-string "[^/]*" out) (incf i))
                     ((char= c #\?) (write-string "[^/]" out) (incf i))
                     ((find c ".+()[]{}^$|\\") (write-char #\\ out) (write-char c out) (incf i))
                     (t (write-char c out) (incf i)))))
    (write-string "$" out)))

(defun path-exists? (pattern context)
  (let ((paths (getf context :paths)) (re (glob-to-regexp pattern)))
    (some (lambda (path) (ppcre:scan re path)) paths)))

(defun path-changed? (pattern context)
  (let ((changed (getf context :changed-paths)) (re (glob-to-regexp pattern)))
    (if (eq changed :unknown)
        t
        (some (lambda (path) (ppcre:scan re path)) changed))))

(defun workflow-allows? (spec context)
  "Whether the pipeline runs at all, by the workflow rules (none: yes)."
  (let ((rules (getf spec :workflow-rules)))
    (if (null rules)
        t
        (dolist (rule rules nil)
          (when (eval-rule-if (getf rule :if) context)
            (return (not (eq (getf rule :when) :never))))))))

(defun jobs-for-context (spec context)
  "The jobs of SPEC that run in CONTEXT, each with its final :when and
:allow-failure, in stage order then file order."
  (let ((stages (getf spec :stages))
        (chosen nil))
    (dolist (job (getf spec :jobs))
      (multiple-value-bind (when allow-failure?) (job-decision job context)
        (when when
          (push (append (list :when when :allow-failure allow-failure?) job) chosen))))
    (setq chosen (nreverse chosen))
    (stable-sort chosen #'< :key (lambda (job) (or (position (getf job :stage) stages :test #'string=) 99)))))
