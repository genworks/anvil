;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :anvil)

;;
;; A pipeline as a Gendl object: its jobs are children, each with a
;; settable status; the pipeline's own status, the jobs that may start
;; now, and each stage's state are computed from the children and
;; recomputed when a status is set.  The records on disk mirror the
;; object (pipeline-record, job-record), written after every change, so
;; a restart finds every pipeline where it was.
;;
;; Statuses, as GitLab names them: created, pending, running, success,
;; failed, canceled, skipped, manual.
;;

(define-object ci-job (base-object)

  :input-slots
  (("Plist. The compiled job (ci-spec.lisp) with its :when and
:allow-failure decided for this pipeline." spec)
   ("Integer. The job record's id." record-id)
   ("Keyword. Where the job stands." status :created :settable)
   ("List of strings: the stages before this job's, in order." earlier-stages)
   (pipeline-jobs))

  :computed-slots
  ((name (getf (the spec) :name))
   (stage (getf (the spec) :stage))
   (when-to-run (getf (the spec) :when))
   (allow-failure? (getf (the spec) :allow-failure))
   (needs (getf (the spec) :needs))
   (needs-given? (getf (the spec) :needs-given?))
   (kind (getf (the spec) :kind))
   (tags (getf (the spec) :tags))

   ("Boolean. The job has ended, one way or another."
    done? (and (member (the status) '(:success :failed :canceled :skipped)) t))

   ("Boolean. The job counts as succeeded for what depends on it: it
succeeded, or failed and was allowed to, or was skipped."
    succeeded? (or (eq (the status) :success)
                   (eq (the status) :skipped)
                   (and (eq (the status) :failed) (the allow-failure?))))

   ("List of ci-job: what this job waits for -- its needs, or every job
of an earlier stage."
    prerequisites
    (if (the needs-given?)
        (remove-if-not (lambda (job) (member (the-object job name) (the needs) :test #'string=)) (the pipeline-jobs))
        (remove-if-not (lambda (job) (member (the-object job stage) (the earlier-stages) :test #'string=)) (the pipeline-jobs))))

   ("Boolean. Every prerequisite has succeeded."
    prerequisites-met? (every (lambda (job) (the-object job succeeded?)) (the prerequisites)))

   ("Boolean. A prerequisite failed for good, so this job never runs
(unless it is to run on failure or always)."
    prerequisites-failed?
    (and (some (lambda (job) (and (the-object job done?) (not (the-object job succeeded?)))) (the prerequisites))
         (not (member (the when-to-run) '(:always :on-failure)))))

   ("Boolean. The job may start now: created, its prerequisites met (or
it runs always), and not waiting for a hand."
    ready?
    (and (eq (the status) :created)
         (not (eq (the when-to-run) :manual))
         (or (the prerequisites-met?)
             (and (member (the when-to-run) '(:always :on-failure))
                  (every (lambda (job) (the-object job done?)) (the prerequisites))))
         (or (not (eq (the when-to-run) :on-failure))
             (some (lambda (job) (and (the-object job done?) (not (the-object job succeeded?)))) (the prerequisites)))))

   ("Boolean. A manual job whose prerequisites are met: a Play button."
    playable? (and (eq (the status) :manual) (the prerequisites-met?)))))

(define-object ci-pipeline (base-object)

  :input-slots
  (("Integer. The pipeline record's id." record-id)
   ("List of strings: the stages, in order." stages)
   ("List of plists: the jobs chosen for the pipeline, each with a
:record-id and :status to start from." job-specs))

  :computed-slots
  ((status
    (let ((jobs (list-elements (the jobs))))
      (cond ((null jobs) :skipped)
            ((some (lambda (j) (eq (the-object j status) :running)) jobs) :running)
            ((some (lambda (j) (and (eq (the-object j status) :failed) (not (the-object j allow-failure?)))) jobs)
             (if (every (lambda (j) (the-object j done?)) jobs) :failed :running))
            ((some (lambda (j) (eq (the-object j status) :canceled)) jobs)
             (if (every (lambda (j) (the-object j done?)) jobs) :canceled :running))
            ((every (lambda (j) (the-object j done?)) jobs) :success)
            ((some (lambda (j) (member (the-object j status) '(:pending :running))) jobs) :running)
            ((and (some (lambda (j) (eq (the-object j status) :manual)) jobs)
                  (every (lambda (j) (or (the-object j done?) (eq (the-object j status) :manual))) jobs))
             :manual)
            ((some (lambda (j) (the-object j ready?)) jobs) :pending)
            ((every (lambda (j) (or (the-object j done?) (eq (the-object j status) :manual) (eq (the-object j status) :created))) jobs)
             ;; created jobs that can never run (prerequisites failed) are skipped by the runner
             :pending)
            (t :pending))))

   ("Boolean. Nothing is left to do."
    finished? (and (member (the status) '(:success :failed :canceled :skipped :manual)) t))

   ("List of ci-job: the jobs that may start now."
    ready-jobs (remove-if-not (lambda (j) (the-object j ready?)) (list-elements (the jobs))))

   ("List of ci-job: created jobs that will never run, because what
they wait for failed: the runner marks them skipped."
    doomed-jobs (remove-if-not (lambda (j) (and (eq (the-object j status) :created) (the-object j prerequisites-failed?)))
                               (list-elements (the jobs))))

   ("List of (stage . status)."
    stage-statuses
    (loop for stage in (the stages)
          for jobs = (remove-if-not (lambda (j) (string= (the-object j stage) stage)) (list-elements (the jobs)))
          when jobs
            collect (cons stage
                          (cond ((some (lambda (j) (eq (the-object j status) :running)) jobs) :running)
                                ((some (lambda (j) (and (eq (the-object j status) :failed) (not (the-object j allow-failure?)))) jobs) :failed)
                                ((every (lambda (j) (the-object j done?)) jobs) :success)
                                ((some (lambda (j) (eq (the-object j status) :manual)) jobs) :manual)
                                ((some (lambda (j) (eq (the-object j status) :pending)) jobs) :pending)
                                (t :created))))))

  :objects
  ((jobs :type 'ci-job
         :sequence (:size (length (the job-specs)))
         :spec (nth (the-child index) (the job-specs))
         :record-id (getf (nth (the-child index) (the job-specs)) :record-id)
         :status (or (getf (nth (the-child index) (the job-specs)) :status) :created)
         :earlier-stages (subseq (the stages) 0 (or (position (getf (nth (the-child index) (the job-specs)) :stage)
                                                               (the stages) :test #'string=)
                                                     0))
         :pipeline-jobs (list-elements (the jobs))))

  :functions
  (("Set the status of the job named NAME." set-job-status! (name status)
    (let ((job (find name (list-elements (the jobs)) :key (lambda (j) (the-object j name)) :test #'string=)))
      (when job (the-object job (set-slot! :status status)))
      job))
   ("The ci-job named NAME." job-named (name)
    (find name (list-elements (the jobs)) :key (lambda (j) (the-object j name)) :test #'string=))))

;;; Pipelines and their objects

(defvar *pipeline-objects* (make-hash-table) "pipeline record id -> ci-pipeline")
(defvar *pipeline-lock* (bt:make-recursive-lock "anvil pipelines"))

(defmacro with-pipelines (() &body body)
  `(bt:with-recursive-lock-held (*pipeline-lock*) ,@body))

(defun pipeline-object (pipeline)
  "The ci-pipeline object for the pipeline record, made from the records
when the image has none yet."
  (with-pipelines ()
    (or (gethash (getf pipeline :id) *pipeline-objects*)
        (setf (gethash (getf pipeline :id) *pipeline-objects*)
              (make-object 'ci-pipeline
                           :record-id (getf pipeline :id)
                           :stages (getf pipeline :stages)
                           :job-specs (loop for job-id in (getf pipeline :jobs)
                                            for job = (record :job job-id)
                                            when job collect (append (list :record-id job-id :status (getf job :status))
                                                                     (getf job :spec))))))))

(defun pipeline-jobs (pipeline)
  (remove nil (mapcar (lambda (id) (record :job id)) (getf pipeline :jobs))))

(defun job-pipeline (job) (record :pipeline (getf job :pipeline-id)))

(defun set-job-status! (job status &rest more)
  "Move JOB (a record) to STATUS, in the object and on disk, with MORE
fields (:started :finished :exit-code ...); the pipeline's own status
follows.  Answers the updated job record."
  (with-pipelines ()
    (let* ((pipeline (job-pipeline job))
           (object (and pipeline (pipeline-object pipeline)))
           (now (get-universal-time))
           (updated (apply #'update-record! job :status status
                           (append (when (and (eq status :running) (null (getf job :started))) (list :started now))
                                   (when (member status '(:success :failed :canceled :skipped)) (list :finished now))
                                   more))))
      (when object
        (the-object object (set-job-status! (getf job :name) status))
        (let ((pipeline-status (the-object object status)))
          (update-record! pipeline :status pipeline-status
                          :started (or (getf pipeline :started) (and (eq pipeline-status :running) now))
                          :finished (and (the-object object finished?) now))
          (when (the-object object finished?)
            (pipeline-finished! (record :pipeline (getf pipeline :id))))))
      updated)))

(defvar *pipeline-finished-hooks* nil
  "Functions of a pipeline record, called when it ends.")

(defun pipeline-finished! (pipeline)
  (dolist (hook *pipeline-finished-hooks*)
    (handler-case (funcall hook pipeline)
      (error (e) (log-line "pipeline hook: ~a" e)))))

(defun pipeline-url (project pipeline)
  (format nil "~a/-/pipelines/~d" (project-url project) (getf pipeline :id)))

(defun job-url (project job)
  (format nil "~a/-/jobs/~d" (project-url project) (getf job :id)))

(defun pipeline-duration (pipeline)
  (let ((started (getf pipeline :started)) (finished (getf pipeline :finished)))
    (and started (- (or finished (get-universal-time)) started))))

(defun job-duration (job)
  (let ((started (getf job :started)) (finished (getf job :finished)))
    (and started (- (or finished (get-universal-time)) started))))

(defun project-pipelines (project &key ref (limit nil))
  (let ((all (records :pipeline (lambda (p) (and (eql (getf p :project-id) (getf project :id))
                                                 (or (null ref) (equal (getf p :ref) ref)))))))
    (if limit (subseq all 0 (min limit (length all))) all)))

(defun latest-pipeline (project &key ref sha)
  (first (records :pipeline (lambda (p) (and (eql (getf p :project-id) (getf project :id))
                                             (or (null ref) (equal (getf p :ref) ref))
                                             (or (null sha) (equal (getf p :sha) sha)))))))

(defun status-text (status)
  (string-downcase (symbol-name (or status :created))))

