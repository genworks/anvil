;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :anvil)

;;
;; Pipelines are made here and run here.
;;
;; `run-pipeline!' reads the specification at a commit, chooses the jobs
;; for the push and writes the pipeline and job records.  The runner
;; thread wakes every few seconds, finds the jobs that may start, and
;; runs each in a thread of its own: a job's working tree is a tar made
;; from the commit, put into a fresh container through the Docker
;; Engine API; its output is followed into a trace file; its artifacts
;; are fetched out as a tar.  A :deploy job runs no container: it is
;; handed to deploy.lisp.
;;

(defvar *runner-thread* nil)
(defvar *running-jobs* (make-hash-table) "job id -> thread")
(defvar *running-lock* (bt:make-lock "anvil running jobs"))
(defvar *runner-stopped?* nil)

;;; Making a pipeline

(defun pipeline-context (project ref sha source variables &key paths)
  "The context rules are evaluated in: :branch :tag :ref :source
:default-branch :variables (an alist, the predefined ones among them)
:paths (the tree's files, for rules: exists)."
  (let* ((branch (and (starts-with? "refs/heads/" ref) (subseq ref 11)))
         (tag (and (starts-with? "refs/tags/" ref) (subseq ref 10)))
         (short (or branch tag ref)))
    (list :branch branch :tag tag :ref short :source source :sha sha
          :default-branch (getf project :default-branch)
          :paths paths
          :changed-paths :unknown
          :variables (append variables
                             (list (cons "CI_COMMIT_REF_NAME" short)
                                   (cons "CI_COMMIT_BRANCH" (or branch ""))
                                   (cons "CI_COMMIT_TAG" (or tag ""))
                                   (cons "CI_COMMIT_SHA" sha)
                                   (cons "CI_PIPELINE_SOURCE" (string-downcase (symbol-name source)))
                                   (cons "CI_DEFAULT_BRANCH" (getf project :default-branch))
                                   (cons "CI_PROJECT_PATH" (getf project :path-with-namespace))
                                   (cons "CI_PROJECT_NAME" (getf project :path)))))))

(defun run-pipeline! (project revision &key (ref nil) (source :web) user variables quiet?)
  "Make and start a pipeline for REVISION (a sha or a ref name) of
PROJECT.  REF is the full ref name when known.  Answers the pipeline
record, or nil (quietly, when QUIET? and the tree has no specification).
Signals ci-spec-error for a specification that does not compile, which
is also recorded as a failed pipeline when not QUIET?."
  (let* ((repository (project-repository project))
         (ref (or ref (cond ((ingot:ref-value* repository (format nil "refs/heads/~a" revision)) (format nil "refs/heads/~a" revision))
                            ((ingot:ref-value* repository (format nil "refs/tags/~a" revision)) (format nil "refs/tags/~a" revision))
                            (t (format nil "refs/heads/~a" (getf project :default-branch))))))
         (sha (or (ingot:resolve-commit repository revision) (bad-request "No commit ~a." revision)))
         (commit (ingot:read-commit repository sha))
         (project-variables (mapcar (lambda (v) (cons (getf v :key) (getf v :value))) (project-variables project)))
         (paths (mapcar #'car (ingot:tree-paths repository (ingot:commit-tree commit)))))
    (multiple-value-bind (spec file)
        (handler-case (read-spec-from-tree repository sha)
          (ci-spec-error (e)
            (if quiet?
                (progn (record-failed-pipeline! project sha ref source user (ci-spec-error-text e))
                       (return-from run-pipeline! nil))
                (error e))))
      (when (null spec)
        (if quiet? (return-from run-pipeline! nil) (bad-request "The commit has no ci.sexp and no .gitlab-ci.yml.")))
      (let* ((context (pipeline-context project ref sha source
                                        (append (mapcar (lambda (v) (cons (car v) (cdr v))) variables)
                                                project-variables
                                                (getf spec :variables))
                                        :paths paths))
             (jobs (and (workflow-allows? spec context) (jobs-for-context spec context))))
        (when (null jobs)
          (if quiet? (return-from run-pipeline! nil) (bad-request "No job runs for this ref: the rules leave the pipeline empty.")))
        (let* ((pipeline (save-record! (new-record :pipeline :project-id (getf project :id)
                                                             :sha sha :ref (getf context :ref) :full-ref ref
                                                             :branch (getf context :branch) :tag (getf context :tag)
                                                             :source source :status :created
                                                             :stages (remove-if-not (lambda (s) (find s jobs :key (lambda (j) (getf j :stage)) :test #'string=))
                                                                                    (getf spec :stages))
                                                             :spec-file file
                                                             :variables (append variables project-variables (getf spec :variables))
                                                             :user-id (and user (getf user :id))
                                                             :title (ingot:commit-subject commit)
                                                             :jobs nil)))
               (job-records (loop for job in jobs
                                  collect (save-record! (new-record :job :pipeline-id (getf pipeline :id)
                                                                         :project-id (getf project :id)
                                                                         :name (getf job :name) :stage (getf job :stage)
                                                                         :spec job
                                                                         :status (if (eq (getf job :when) :manual) :manual :created)
                                                                         :when (getf job :when)
                                                                         :allow-failure (getf job :allow-failure)
                                                                         :job-kind (getf job :kind)
                                                                         :started nil :finished nil :exit-code nil
                                                                         :trace-file nil :artifacts-file nil)))))
          (setq pipeline (update-record! pipeline :jobs (mapcar (lambda (j) (getf j :id)) job-records)
                                                  :status (the-object (pipeline-object (update-record! pipeline :jobs (mapcar (lambda (j) (getf j :id)) job-records))) status)))
          (touch-project! project)
          (wake-runner!)
          pipeline)))))

(defun record-failed-pipeline! (project sha ref source user text)
  "A pipeline that could not be made (the specification does not
compile): kept, failed, with the reason, so the push shows it."
  (let ((branch (and (starts-with? "refs/heads/" ref) (subseq ref 11)))
        (tag (and (starts-with? "refs/tags/" ref) (subseq ref 10))))
    (save-record! (new-record :pipeline :project-id (getf project :id) :sha sha
                                        :ref (or branch tag ref) :full-ref ref :branch branch :tag tag
                                        :source source :status :failed :stages nil :jobs nil
                                        :user-id (and user (getf user :id)) :error text
                                        :finished (get-universal-time)))))

(defun cancel-pipeline! (pipeline)
  (dolist (job (pipeline-jobs pipeline))
    (when (member (getf job :status) '(:created :pending :manual))
      (set-job-status! job :canceled))
    (when (eq (getf job :status) :running)
      (let ((thread (bt:with-lock-held (*running-lock*) (gethash (getf job :id) *running-jobs*))))
        (set-job-status! job :canceled)
        (when (and thread (bt:thread-alive-p thread)) (ignore-errors (bt:destroy-thread thread)))
        (let ((container (getf job :container)))
          (when container (docker-kill container) (docker-remove container))))))
  (record :pipeline (getf pipeline :id)))

(defun retry-job! (job)
  "A fresh job record for JOB's specification, in its pipeline, and
the runner woken.  Answers the new job."
  (let* ((pipeline (job-pipeline job))
         (new (save-record! (new-record :job :pipeline-id (getf pipeline :id) :project-id (getf job :project-id)
                                             :name (getf job :name) :stage (getf job :stage)
                                             :spec (getf job :spec) :status :created
                                             :when (getf job :when) :allow-failure (getf job :allow-failure)
                                             :job-kind (job-kind job) :retry-of (getf job :id)))))
    (update-record! job :retried? t)
    (with-pipelines ()
      (remhash (getf pipeline :id) *pipeline-objects*)
      (update-record! pipeline :jobs (substitute (getf new :id) (getf job :id) (getf pipeline :jobs))
                               :status :running :finished nil)
      (pipeline-object (record :pipeline (getf pipeline :id))))
    (wake-runner!)
    new))

(defun play-job! (job)
  "Start a manual job."
  (when (eq (getf job :status) :manual)
    (set-job-status! job :created)
    (let ((pipeline (job-pipeline job)))
      (update-record! pipeline :finished nil :status (the-object (pipeline-object pipeline) status)))
    (wake-runner!)
    (record :job (getf job :id))))

;;; The runner loop

(defvar *runner-wake* (bt:make-condition-variable))
(defvar *runner-wake-lock* (bt:make-lock "anvil runner wake"))

(defun wake-runner! ()
  (bt:with-lock-held (*runner-wake-lock*) (bt:condition-notify *runner-wake*)))

(defun start-runner ()
  (setq *runner-stopped?* nil)
  (setq *runner-thread*
        (if (and *runner-thread* (bt:thread-alive-p *runner-thread*))
            *runner-thread*
            (bt:make-thread (lambda ()
                              (loop until *runner-stopped?*
                                    do (handler-case (runner-tick!)
                                         (error (e) (log-line "runner: ~a" e)))
                                       (bt:with-lock-held (*runner-wake-lock*)
                                         (bt:condition-wait *runner-wake* *runner-wake-lock* :timeout 5))))
                            :name "anvil runner"))))

(defun stop-runner ()
  (setq *runner-stopped?* t)
  (wake-runner!))

(defun running-count ()
  (bt:with-lock-held (*running-lock*)
    (let ((n 0))
      (maphash (lambda (id thread) (declare (ignore id)) (when (bt:thread-alive-p thread) (incf n))) *running-jobs*)
      n)))

(defun job-tags-served? (job)
  "Whether this runner takes the job: every tag of the job is among
*runner-tags* (a job with none runs anywhere)."
  (let ((tags (getf (getf job :spec) :tags)))
    (or (null tags) (subsetp tags *runner-tags* :test #'string-equal))))

(defun runner-tick! ()
  "Skip what cannot run any more, start what may, up to the parallelism."
  (dolist (pipeline (records :pipeline (lambda (p) (member (getf p :status) '(:created :pending :running)))))
    (let ((object (pipeline-object pipeline)))
      (dolist (doomed (the-object object doomed-jobs))
        (let ((job (record :job (the-object doomed record-id))))
          (when job (set-job-status! job :skipped))))
      (dolist (ready (the-object object ready-jobs))
        (when (< (running-count) *runner-parallelism*)
          (let ((job (record :job (the-object ready record-id))))
            (when (and job (eq (getf job :status) :created))
              (cond ((not (job-tags-served? job))
                     (set-job-status! job :failed :failure-reason
                                      (format nil "No runner here serves the tags ~{~a~^, ~} (this one has ~{~a~^, ~})."
                                              (getf (getf job :spec) :tags) *runner-tags*)))
                    ((and (member (job-kind job) '(:script :lisp)) (not (docker-available?)) (not *shell-executor?*))
                     (set-job-status! job :failed :failure-reason "No executor: the Docker socket is not reachable from the forge."))
                    (t (start-job! job)))))))
      ;; a pipeline whose object says it ended but whose record lags
      (let ((current (record :pipeline (getf pipeline :id))))
        (when (and current (the-object object finished?) (not (member (getf current :status) '(:success :failed :canceled :skipped :manual))))
          (update-record! current :status (the-object object status) :finished (get-universal-time))
          (pipeline-finished! (record :pipeline (getf pipeline :id))))))))

(defun start-job! (job)
  (let ((job (set-job-status! job :pending)))
    (let ((thread (bt:make-thread (lambda ()
                                    (unwind-protect
                                         (handler-case (execute-job! job)
                                           (error (e)
                                             (log-line "job ~d: ~a" (getf job :id) e)
                                             (ignore-errors (trace-line! job "~&ERROR: ~a~%" e))
                                             (ignore-errors (set-job-status! (record :job (getf job :id)) :failed
                                                                             :failure-reason (princ-to-string e)))))
                                      (bt:with-lock-held (*running-lock*) (remhash (getf job :id) *running-jobs*))
                                      (wake-runner!)))
                                  :name (format nil "anvil job ~d" (getf job :id)))))
      (bt:with-lock-held (*running-lock*) (setf (gethash (getf job :id) *running-jobs*) thread)))))

;;; Traces

(defun trace-file (job)
  (data-path (format nil "ci/traces/~d.log" (getf job :id))))

(defvar *trace-locks* (make-hash-table))
(defvar *trace-locks-lock* (bt:make-lock "anvil traces"))

(defun trace-lock (job)
  (bt:with-lock-held (*trace-locks-lock*)
    (or (gethash (getf job :id) *trace-locks*)
        (setf (gethash (getf job :id) *trace-locks*) (bt:make-lock (format nil "trace ~d" (getf job :id)))))))

(defun trace-write! (job text)
  (let ((file (trace-file job)))
    (bt:with-lock-held ((trace-lock job))
      (ensure-directories-exist file)
      (with-open-file (out file :direction :output :if-exists :append :if-does-not-exist :create
                                :element-type '(unsigned-byte 8))
        (write-sequence (ingot:string-to-octets text) out)))))

(defun trace-line! (job control &rest args)
  (trace-write! job (format nil "~?~%" control args)))

(defun read-trace (job &key (start 0) (limit nil))
  "The trace's text from byte START (at most LIMIT bytes), and the
size of the file."
  (let ((file (probe-file (trace-file job))))
    (if (null file)
        (values "" 0)
        (with-open-file (in file :element-type '(unsigned-byte 8))
          (let* ((size (file-length in))
                 (from (min start size))
                 (to (if limit (min size (+ from limit)) size))
                 (octets (ingot:make-octets (- to from))))
            (file-position in from)
            (read-sequence octets in)
            (values (ingot:octets-to-string octets) size))))))

;;; Executing a job

(defun job-image (job)
  (or (getf (getf job :spec) :image) "genworks/gendl:devo-ccl"))

(defun lisp-of-image (image)
  "ccl or sbcl, from a Gendl image's tag; what a :lisp job's script
needs as ANVIL_LISP."
  (cond ((search "sbcl" image) "sbcl")
        ((search "ccl" image) "ccl")
        (t "ccl")))

(defun job-variables (job pipeline project)
  "An alist of every variable the job sees: the predefined ones, the
project's, the pipeline's, the job's own."
  (let* ((spec (getf job :spec))
         (project-dir (project-build-dir project)))
    (append (getf spec :variables)
            (getf pipeline :variables)
            (list (cons "CI" "true") (cons "GITLAB_CI" "true") (cons "ANVIL_CI" "true")
                  (cons "CI_SERVER_URL" *site-url*) (cons "CI_SERVER_HOST" (or (url-host *site-url*) "anvil"))
                  (cons "CI_SERVER_NAME" *site-name*) (cons "CI_API_V4_URL" (strcat *site-url* "/api/v4"))
                  (cons "CI_PROJECT_DIR" project-dir)
                  (cons "CI_PROJECT_ID" (princ-to-string (getf project :id)))
                  (cons "CI_PROJECT_PATH" (getf project :path-with-namespace))
                  (cons "CI_PROJECT_NAME" (getf project :path))
                  (cons "CI_PROJECT_NAMESPACE" (project-namespace-path project))
                  (cons "CI_PROJECT_URL" (project-web-url project))
                  (cons "CI_PROJECT_VISIBILITY" (string-downcase (symbol-name (getf project :visibility))))
                  (cons "CI_REPOSITORY_URL" (project-clone-url project))
                  (cons "CI_COMMIT_SHA" (getf pipeline :sha))
                  (cons "CI_COMMIT_SHORT_SHA" (subseq (getf pipeline :sha) 0 8))
                  (cons "CI_COMMIT_REF_NAME" (getf pipeline :ref))
                  (cons "CI_COMMIT_REF_SLUG" (ref-slug (getf pipeline :ref)))
                  (cons "CI_COMMIT_BRANCH" (or (getf pipeline :branch) ""))
                  (cons "CI_COMMIT_TAG" (or (getf pipeline :tag) ""))
                  (cons "CI_COMMIT_TITLE" (or (getf pipeline :title) ""))
                  (cons "CI_DEFAULT_BRANCH" (getf project :default-branch))
                  (cons "CI_PIPELINE_ID" (princ-to-string (getf pipeline :id)))
                  (cons "CI_PIPELINE_IID" (princ-to-string (getf pipeline :id)))
                  (cons "CI_PIPELINE_SOURCE" (string-downcase (symbol-name (getf pipeline :source))))
                  (cons "CI_PIPELINE_URL" (strcat *site-url* (pipeline-url project pipeline)))
                  (cons "CI_JOB_ID" (princ-to-string (getf job :id)))
                  (cons "CI_JOB_NAME" (getf job :name))
                  (cons "CI_JOB_STAGE" (getf job :stage))
                  (cons "CI_JOB_URL" (strcat *site-url* (job-url project job)))
                  (cons "CI_REGISTRY" "")
                  (cons "ANVIL_LISP" (lisp-of-image (job-image job)))))))

(defun ref-slug (ref)
  (string-trim "-" (subseq (ppcre:regex-replace-all "[^a-z0-9]+" (string-downcase ref) "-") 0 (min 63 (length ref)))))

(defun unique-variables (alist)
  "ALIST with the first binding of each name kept."
  (let ((seen (make-hash-table :test #'equal)) (out nil))
    (dolist (pair alist (nreverse out))
      (unless (gethash (car pair) seen)
        (setf (gethash (car pair) seen) t)
        (push pair out)))))

(defun expand-variable-references (alist)
  "ALIST with every $NAME and ${NAME} in a value replaced by that
variable's value from ALIST, as GitLab expands them; a name not in
ALIST is left for the shell."
  (flet ((expand (text)
           (ppcre:regex-replace-all "\\$(?:\\{([A-Za-z_][A-Za-z0-9_]*)\\}|([A-Za-z_][A-Za-z0-9_]*))" text
                                    (lambda (match name1 name2)
                                      (let ((found (assoc (or name1 name2) alist :test #'string=)))
                                        (if found (cdr found) match)))
                                    :simple-calls t)))
    (loop for (name . value) in alist
          collect (cons name (if (stringp value) (expand value) value)))))

(defun job-environment (job pipeline project)
  "The variables a job's container gets: each name once, references
between them expanded."
  (expand-variable-references (unique-variables (job-variables job pipeline project))))

(defparameter *builds-root* "/builds")

(defun project-build-dir (project)
  (format nil "~a/~a" *builds-root* (getf project :path-with-namespace)))

(defun shell-quote (text)
  (format nil "'~a'" (ppcre:regex-replace-all "'" text "'\\\\''")))

(defun job-shell-script (spec)
  "One sh script: before_script and script in a subshell under set -e,
each line echoed as GitLab does, so that a failing line ends the main
part and after_script still runs; the main part's status is the job's."
  (flet ((lines (out lines)
           (dolist (line lines)
             (format out "printf '%s\\n' ~a~%~a~%" (shell-quote (strcat "$ " line)) line))))
    (with-output-to-string (out)
      (format out "(~%set -e~%")
      (lines out (append (getf spec :before-script) (getf spec :script)))
      (format out ")~%status=$?~%")
      (lines out (getf spec :after-script))
      (format out "exit $status~%"))))

(defun job-kind (job)
  "What kind of job the record is: :script, :lisp or :deploy."
  (or (getf job :job-kind) (getf (getf job :spec) :kind) :script))

(defun execute-job! (job)
  (let* ((pipeline (job-pipeline job))
         (project (record :project (getf job :project-id)))
         (spec (getf job :spec)))
    (setq job (set-job-status! job :running :trace-file (namestring (trace-file job))))
    (trace-line! job "Running on ~a ~a (~a ~a)" *site-name* *version* (lisp-implementation-type) (lisp-implementation-version))
    (ecase (job-kind job)
      (:deploy (execute-deploy-job! job pipeline project))
      ((:script :lisp)
       (if (and (member "shell" (getf spec :tags) :test #'string-equal) *shell-executor?*)
           (execute-shell-job! job pipeline project)
           (execute-docker-job! job pipeline project))))))

(defun execute-docker-job! (job pipeline project)
  (let* ((spec (getf job :spec))
         (image (job-image job))
         (repository (project-repository project))
         (env (mapcar (lambda (pair) (format nil "~a=~a" (car pair) (cdr pair))) (job-environment job pipeline project)))
         (project-dir (project-build-dir project))
         (network nil) (services nil) (container nil) (watchdog nil))
    (unwind-protect
         (progn
           ;; the image
           (unless (docker-image-exists? image)
             (trace-line! job "Pulling ~a ..." image)
             (docker-pull image :on-progress (lambda (text) (trace-line! job "  ~a" text))))
           (trace-line! job "Using image ~a" image)
           ;; services
           (when (getf spec :services)
             (setq network (docker-create-network (format nil "anvil-job-~d" (getf job :id))))
             (dolist (service (getf spec :services))
               (let ((name (getf service :name)))
                 (unless (docker-image-exists? name)
                   (trace-line! job "Pulling service ~a ..." name)
                   (docker-pull name))
                 (let ((id (docker-create-container
                            :image name :command (getf service :command)
                            :env (mapcar (lambda (pair) (format nil "~a=~a" (car pair) (cdr pair))) (getf service :variables))
                            :network-mode network :name (format nil "anvil-job-~d-~a" (getf job :id) (getf service :alias))
                            :labels (list (cons "anvil.job" (princ-to-string (getf job :id)))))))
                   (docker-start id)
                   (push id services)
                   (trace-line! job "Started service ~a as ~a" name (getf service :alias))))))
           ;; the job's container
           (setq container (docker-create-container
                            :image image
                            :entrypoint (getf spec :entrypoint)
                            :command (list "sh" "-c" (job-shell-script spec))
                            :env env
                            :working-dir project-dir
                            :labels (list (cons "anvil.job" (princ-to-string (getf job :id)))
                                          (cons "anvil.project" (getf project :path-with-namespace)))
                            :memory *runner-memory-bytes*
                            :network-mode (or network "bridge")
                            :name (format nil "anvil-job-~d" (getf job :id))))
           (update-record! job :container container)
           ;; the working tree, and the artifacts of earlier jobs
           (unless (eq (getf spec :git-strategy) :none)
             ;; the tree goes in under the builds root, its directories and
             ;; files open to everyone: the tar is root's, and the job runs
             ;; as the image's user, which must be able to write in it
             (let* ((commit (ingot:read-commit repository (getf pipeline :sha)))
                    (tar (ingot::buffer-octets (ingot:tar-tree repository (ingot:commit-tree commit)
                                                               :prefix (subseq (strcat project-dir "/") (length (strcat *builds-root* "/")))
                                                               :mtime (ingot:commit-committer-time commit)
                                                               :directory-mode #o777 :file-mode #o666 :executable-mode #o777))))
               (docker-put-archive container *builds-root* tar)
               (trace-line! job "Checked out ~a (~a) into ~a: ~a"
                            (subseq (getf pipeline :sha) 0 8) (getf pipeline :ref) project-dir
                            (plural (length (ingot:tree-paths repository (ingot:commit-tree commit))) "file")))
             (dolist (earlier (artifact-sources job pipeline))
               (let ((tar (ingot::file-octets-if-exists (getf earlier :artifacts-file))))
                 (when tar
                   (docker-put-archive container project-dir tar)
                   (trace-line! job "Downloaded artifacts of ~a" (getf earlier :name))))))
           ;; run it, watched
           (docker-start container)
           (let ((timeout (or (getf spec :timeout) *job-timeout-seconds*))
                 (id container))
             (setq watchdog (bt:make-thread (lambda ()
                                              (sleep timeout)
                                              (ignore-errors (trace-line! job "~&ERROR: the job ran past its ~a and was stopped." (duration-text timeout)))
                                              (docker-kill id))
                                            :name (format nil "anvil watchdog ~d" (getf job :id)))))
           (docker-logs container (lambda (text) (trace-write! job text)))
           (let ((exit-code (docker-wait container)))
             (when (and watchdog (bt:thread-alive-p watchdog)) (ignore-errors (bt:destroy-thread watchdog)) (setq watchdog nil))
             ;; artifacts
             (let ((artifacts (getf spec :artifacts)))
               (when (and artifacts (getf artifacts :paths)
                          (or (eq (getf artifacts :when) :always)
                              (and (eq (getf artifacts :when) :on-success) (zerop exit-code))
                              (and (eq (getf artifacts :when) :on-failure) (not (zerop exit-code)))))
                 (collect-artifacts! job container project-dir artifacts)))
             (trace-line! job "~%Job ~:[failed with exit code ~d~;succeeded~*~]" (zerop exit-code) exit-code)
             (set-job-status! (record :job (getf job :id)) (if (zerop exit-code) :success :failed)
                              :exit-code exit-code
                              :failure-reason (if (zerop exit-code) nil (format nil "exit code ~d" exit-code)))))
      (when (and watchdog (bt:thread-alive-p watchdog)) (ignore-errors (bt:destroy-thread watchdog)))
      (when container (docker-remove container))
      (dolist (id services) (docker-remove id))
      (when network (docker-remove-network network)))))

(defun artifact-sources (job pipeline)
  "The earlier jobs whose artifacts this job gets: those named in
`dependencies' or `needs', else every finished job of an earlier
stage."
  (let* ((spec (getf job :spec))
         (jobs (pipeline-jobs pipeline))
         (stages (getf pipeline :stages))
         (my-stage (position (getf job :stage) stages :test #'string=))
         (named (cond ((getf spec :dependencies-given?) (getf spec :dependencies))
                      ((getf spec :needs-given?) (getf spec :needs))
                      (t nil))))
    (remove-if-not (lambda (other)
                     (and (getf other :artifacts-file)
                          (not (eql (getf other :id) (getf job :id)))
                          (if (or (getf spec :dependencies-given?) (getf spec :needs-given?))
                              (member (getf other :name) named :test #'string=)
                              (let ((stage (position (getf other :stage) stages :test #'string=)))
                                (and stage my-stage (< stage my-stage))))))
                   jobs)))

(defun artifacts-file (job)
  (data-path (format nil "ci/artifacts/~d.tar" (getf job :id))))

(defun collect-artifacts! (job container project-dir artifacts)
  "Fetch each declared path out of the container and keep them as one
tar whose entries are relative to the project directory."
  (let ((buffer (ingot::make-buffer (* 256 1024)))
        (count 0))
    (dolist (path (getf artifacts :paths))
      (let* ((clean (string-right-trim "/" (if (starts-with? "./" path) (subseq path 2) path)))
             (full (format nil "~a/~a" project-dir clean))
             (tar (handler-case (docker-get-archive container full)
                    (error () nil))))
        (cond ((null tar) (trace-line! job "WARNING: no ~a to keep as an artifact" clean))
              (t (let ((prefix (let ((slash (position #\/ clean :from-end t))) (if slash (subseq clean 0 (1+ slash)) ""))))
                   (dolist (entry (ingot:tar-entries tar))
                     (ingot:tar-append-entry buffer (strcat prefix (getf entry :name)) entry)
                     (incf count)))))))
    (when (plusp count)
      (ingot::buffer-write buffer (ingot:make-octets 1024))
      (let ((file (artifacts-file job)))
        (ingot:write-file-octets file (ingot::buffer-octets buffer))
        (update-record! job :artifacts-file (namestring file) :artifacts-size (fill-pointer buffer)
                            :artifacts-expire (let ((in (parse-duration (getf artifacts :expire-in))))
                                                (and in (+ (get-universal-time) in))))
        (trace-line! job "Uploaded artifacts: ~a (~:d bytes)" (plural count "entry" "entries") (fill-pointer buffer))))))

(defun reap-artifacts! ()
  (dolist (job (records :job (lambda (j) (and (getf j :artifacts-file) (getf j :artifacts-expire)
                                              (< (getf j :artifacts-expire) (get-universal-time))))))
    (let ((file (probe-file (getf job :artifacts-file))))
      (when file (delete-file file)))
    (update-record! job :artifacts-file nil :artifacts-expired? t)))

;;; The shell executor: for a site that trusts its projects

(defun execute-shell-job! (job pipeline project)
  (let* ((spec (getf job :spec))
         (repository (project-repository project))
         (root (data-path (format nil "ci/builds/~d/" (getf job :id))))
         (variables (job-environment job pipeline project)))
    (ensure-directories-exist root)
    (let* ((commit (ingot:read-commit repository (getf pipeline :sha)))
           (tar (ingot::buffer-octets (ingot:tar-tree repository (ingot:commit-tree commit)))))
      (ingot:untar tar root))
    (trace-line! job "Checked out ~a into ~a (shell executor)" (subseq (getf pipeline :sha) 0 8) root)
    (let ((script-file (merge-pathnames "anvil-job.sh" root)))
      (write-text-file script-file (job-shell-script spec))
      (let ((exit-code
              (handler-case
                  (nth-value 2 (uiop:run-program (list "sh" (namestring script-file))
                                                 :directory root
                                                 :environment (append (mapcar (lambda (p) (format nil "~a=~a" (car p) (cdr p)))
                                                                              (acons "CI_PROJECT_DIR" (namestring root) variables))
                                                                      (list (format nil "PATH=~a" (or (uiop:getenv "PATH") "/usr/bin:/bin"))))
                                                 :output (lambda (stream) (loop for line = (read-line stream nil nil) while line do (trace-line! job "~a" line)))
                                                 :error-output :output
                                                 :ignore-error-status t))
                (error (e) (trace-line! job "ERROR: ~a" e) 1))))
        (set-job-status! (record :job (getf job :id)) (if (zerop exit-code) :success :failed) :exit-code exit-code)))))

;;; Housekeeping at start: jobs left running by a previous image

(defun fail-orphaned-jobs! ()
  (dolist (job (records :job (lambda (j) (member (getf j :status) '(:pending :running)))))
    (ignore-errors (trace-line! job "~&ERROR: the forge restarted while this job ran."))
    (set-job-status! job :failed :failure-reason "runner restarted")))
