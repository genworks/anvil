;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :anvil)

;;
;; CI pages: the pipelines, a pipeline with its stages, a job with its
;; trace following itself, Run pipeline; and the Deployments page with
;; the Deploy button.
;;

(defun pipelines-page (req ent project)
  (let* ((site (site)) (url (project-url project))
         (pipelines (project-pipelines project))
         (view (project-view project)))
    (respond-html req ent
                  (project-page project "Pipelines"
                                (html (:div :class "filters"
                                        (:h2 :style "margin:0" "Pipelines")
                                        (when (can? :run-pipeline project)
                                          (htm (:form :method "post" :action (format nil "~a/-/pipelines" url) :class "ref-pick" :style "margin:0 0 0 auto"
                                                 (:select :name "ref"
                                                   (dolist (entry (the-object view branches)) (htm (:option :value (first entry) (hs (first entry)))))
                                                   (dolist (entry (the-object view tags)) (htm (:option :value (first entry) (hs (first entry))))))
                                                 (str (the-object site (button "Run pipeline" :class "btn btn-small")))))))
                                      (unless (the-object view ci-file)
                                        (htm (:p :class "hint" "No " (:code "ci.sexp") " or " (:code ".gitlab-ci.yml") " on the default branch; see " (:a :href "/help" "Help") ".")))
                                      (str (the-object site (table '("Status" "Pipeline" "Commit" "Stages" "Duration" "")
                                                                   (mapcar (lambda (p) (pipeline-row site project p)) pipelines)
                                                                   :empty "No pipelines yet.")))
                                      (when (some (lambda (p) (member (getf p :status) '(:created :pending :running))) pipelines)
                                        (htm (:span :data-poll "1"))))
                                :active :pipelines :notice (flash-notice req)))))

(defun pipeline-row (site project pipeline)
  (let ((url (project-url project)))
    (list (the-object site (status-badge (getf pipeline :status) :href (pipeline-url project pipeline)))
          (html (:a :href (pipeline-url project pipeline) "#" (hs (princ-to-string (getf pipeline :id))))
                " " (:span :class "quiet" (hs (status-text (getf pipeline :source)))) " "
                (:span :class "badge" (hs (getf pipeline :ref)))
                (when (getf pipeline :error) (htm (:div :class "profile-bad" (hs (getf pipeline :error))))))
          (html (:a :href (format nil "~a/-/commit/~a" url (getf pipeline :sha)) (:code (hs (subseq (getf pipeline :sha) 0 8))))
                " " (:span :class "quiet" (hs (or (getf pipeline :title) ""))))
          (html (dolist (job (pipeline-jobs pipeline))
                  (htm (:a :href (job-url project job) :title (getf job :name) (str (the-object site (status-badge (getf job :status))))) " ")))
          (hs (duration-text (pipeline-duration pipeline)))
          (the-object site (time-html (getf pipeline :created))))))

(defun pipeline-run-post (req ent project)
  (require-can :run-pipeline project)
  (handler-case
      (let ((pipeline (run-pipeline! project (param req "ref") :source :web :user *current-user*)))
        (redirect req ent (pipeline-url project pipeline)))
    (http-error (e) (redirect req ent (with-notice (format nil "~a/-/pipelines" (project-url project)) (http-error-text e))))
    (ci-spec-error (e) (redirect req ent (with-notice (format nil "~a/-/pipelines" (project-url project)) (ci-spec-error-text e))))))

(defun pipeline-page (req ent project pipeline)
  (let* ((site (site)) (url (project-url project))
         (jobs (pipeline-jobs pipeline))
         (live? (member (getf pipeline :status) '(:created :pending :running))))
    (respond-html req ent
                  (project-page project (format nil "Pipeline #~d" (getf pipeline :id))
                                (html (:div :class "thing-head"
                                        (str (the-object site (status-badge (getf pipeline :status))))
                                        (:h2 "Pipeline #" (hs (princ-to-string (getf pipeline :id))))
                                        (:span :class "quiet" "for " (:a :href (format nil "~a/-/commit/~a" url (getf pipeline :sha)) (:code (hs (subseq (getf pipeline :sha) 0 8))))
                                               " on " (:span :class "badge" (hs (getf pipeline :ref)))
                                               " · " (hs (status-text (getf pipeline :source))) " · " (str (the-object site (time-html (getf pipeline :created))))
                                               (when (getf pipeline :spec-file) (htm " · " (:a :href (format nil "~a/-/blob/~a/~a" url (getf pipeline :sha) (getf pipeline :spec-file)) (hs (getf pipeline :spec-file))))))
                                        (when (can? :run-pipeline project)
                                          (htm (:span :style "margin-left:auto"
                                                 (when live? (htm (:form :method "post" :action (format nil "~a/cancel" (pipeline-url project pipeline)) :style "display:inline" (str (the-object site (button "Cancel" :class "btn btn-small btn-quiet"))))))
                                                 (when (member (getf pipeline :status) '(:failed :canceled)) (htm (:form :method "post" :action (format nil "~a/retry" (pipeline-url project pipeline)) :style "display:inline" (str (the-object site (button "Retry failed" :class "btn btn-small btn-quiet"))))))))))
                                      (when (getf pipeline :error) (htm (:p :class "profile-bad" (hs (getf pipeline :error)))))
                                      (when (getf pipeline :title) (htm (:p :class "quiet" (hs (getf pipeline :title)))))
                                      (:div :class "stages"
                                        (dolist (stage (getf pipeline :stages))
                                          (let ((stage-jobs (remove-if-not (lambda (j) (string= (getf j :stage) stage)) jobs)))
                                            (when stage-jobs
                                              (htm (:div :class "stage"
                                                     (:h3 (hs stage))
                                                     (dolist (job stage-jobs)
                                                       (htm (:div :class "job-row"
                                                              (:a :href (job-url project job) (hs (getf job :name)))
                                                              (:span (str (the-object site (status-badge (getf job :status))))
                                                                     (when (and (eq (getf job :status) :manual) (can? :run-pipeline project))
                                                                       (htm (:form :method "post" :action (format nil "~a/play" (job-url project job)) :style "display:inline" (str (the-object site (button "▶" :class "btn btn-small"))))))))))))))))
                                      (when live? (htm (:span :data-poll "1")))
                                      (let ((variables (remove-if (lambda (v) (find (car v) (project-variables project) :key (lambda (pv) (getf pv :key)) :test #'string=))
                                                                  (getf pipeline :variables))))
                                        (when variables
                                          (htm (:details (:summary "Variables") (:pre (hs (format nil "~{~a=~a~%~}" (loop for (k . v) in variables append (list k v))))))))))
                                :active :pipelines :notice (flash-notice req)))))

(defun pipeline-action-post (req ent project pipeline action)
  (require-can :run-pipeline project)
  (ecase action
    (:cancel (cancel-pipeline! pipeline))
    (:retry (dolist (job (pipeline-jobs pipeline))
              (when (member (getf job :status) '(:failed :canceled)) (retry-job! job *current-user*)))))
  (redirect req ent (pipeline-url project pipeline)))

(defun job-page (req ent project job)
  (let* ((site (site)) (url (project-url project))
         (pipeline (job-pipeline job))
         (live? (member (getf job :status) '(:pending :running))))
    (multiple-value-bind (text size) (read-trace job :start (max 0 (- (nth-value 1 (read-trace job :start 0 :limit 0)) *trace-tail-bytes*)))
      (respond-html req ent
                    (project-page project (format nil "~a (#~d)" (getf job :name) (getf job :id))
                                  (html (:div :class "thing-head"
                                          (str (the-object site (status-badge (getf job :status))))
                                          (:h2 (hs (getf job :name)) " " (:span :class "quiet" "#" (hs (princ-to-string (getf job :id)))))
                                          (:span :class "quiet" "in " (:a :href (pipeline-url project pipeline) "pipeline #" (hs (princ-to-string (getf pipeline :id))))
                                                 " · stage " (hs (getf job :stage))
                                                 " · " (:a :href (format nil "~a/-/commit/~a" url (getf pipeline :sha)) (:code (hs (subseq (getf pipeline :sha) 0 8))))
                                                 (when (job-duration job) (htm " · " (hs (duration-text (job-duration job)))))
                                                 (when (getf job :failure-reason) (htm " · " (:span :class "profile-bad" (hs (getf job :failure-reason))))))
                                          (:span :style "margin-left:auto"
                                            (when (getf job :artifacts-file)
                                              (htm (:a :class "btn btn-small btn-quiet" :href (format nil "~a/artifacts" (job-url project job)) "Download artifacts")))
                                            (when (can? :run-pipeline project)
                                              (cond ((eq (getf job :status) :manual)
                                                     (htm (:form :method "post" :action (format nil "~a/play" (job-url project job)) :style "display:inline" (str (the-object site (button "Play" :class "btn btn-small"))))))
                                                    ((member (getf job :status) '(:failed :canceled :success))
                                                     (htm (:form :method "post" :action (format nil "~a/retry" (job-url project job)) :style "display:inline" (str (the-object site (button "Retry" :class "btn btn-small btn-quiet"))))))
                                                    (live? (htm (:form :method "post" :action (format nil "~a/cancel" (job-url project job)) :style "display:inline" (str (the-object site (button "Cancel" :class "btn btn-small btn-quiet"))))))))))
                                        (:pre :class "trace" :data-trace (format nil "~a/trace.json" (job-url project job)) :data-live (if live? "1" "0") :data-size size
                                          (hs (if (string= text "") (if live? "Waiting for the runner ..." "No trace.") text)))
                                        (let ((spec (getf job :spec)))
                                          (htm (:details (:summary "The job's specification")
                                                 (:pre (hs (let ((*print-case* :downcase) (*print-right-margin* 100))
                                                              (prin1-to-string (loop for (k v) on spec by #'cddr unless (member k '(:name)) append (list k v))))))))))
                                  :active :pipelines :wide? t)))))

(defun job-trace-json (req ent project job)
  (declare (ignore project))
  (let ((start (max 0 (or (ignore-errors (parse-integer (url-query-value req "start" "0"))) 0))))
    (multiple-value-bind (text size) (read-trace job :start start)
      (respond-json req ent (json-table "text" text "size" size
                                        "live" (json-boolean (member (getf (record :job (getf job :id)) :status) '(:pending :running)))
                                        "status" (status-text (getf (record :job (getf job :id)) :status)))))))

(defun job-action-post (req ent project job action)
  (require-can :run-pipeline project)
  (let ((target (ecase action
                  (:retry (retry-job! job *current-user*))
                  (:play (or (play-job! job *current-user*) job))
                  (:cancel (set-job-status! job :canceled)))))
    (redirect req ent (job-url project target))))

(defun job-artifacts-page (req ent project job path)
  (declare (ignore project))
  (if (blank? path)
      (serve-artifacts req ent job)
      (serve-artifact-file req ent job path)))

;;; Deployments

(defun deployments-page (req ent project)
  (let* ((site (site)) (url (project-url project))
         (view (project-view project))
         (deployments (project-deployments project))
         (report (the-object view profile))
         (settings (getf project :deploy)))
    (dolist (d deployments)
      (when (and (not (deployment-final? d)) (> (- (get-universal-time) (or (getf d :checked) 0)) 10))
        (ignore-errors (refresh-deployment! d))))
    (setq deployments (project-deployments project))
    (respond-html req ent
                  (project-page project "Deployments"
                                (html (:div :class "grid"
                                        (:div
                                          (:h2 "Deployments")
                                          (str (the-object site (table '("State" "Commit" "House" "Asked" "Address" "")
                                                                       (mapcar (lambda (d)
                                                                                 (list (html (:span :class (format nil "badge status-~a" (getf d :state)) (hs (deployment-state-text (getf d :state)))))
                                                                                       (html (:a :href (format nil "~a/-/commit/~a" url (getf d :sha)) (:code (hs (subseq (getf d :sha) 0 8)))))
                                                                                       (hs (let ((h (deploy-house (getf d :house)))) (or (and h (getf h :name)) (princ-to-string (getf d :house)))))
                                                                                       (the-object site (time-html (getf d :created)))
                                                                                       (if (getf d :url) (html (:a :href (getf d :url) :rel "noopener" (hs (getf d :url)))) "")
                                                                                       (html (:details (:summary "history")
                                                                                               (:ul (dolist (h (getf d :history)) (htm (:li (hs (getf h :state)) (when (getf h :note) (htm ": " (hs (getf h :note))))))))
                                                                                               (dolist (r (getf d :reasons)) (htm (:p :class "profile-bad" (hs r))))))))
                                                                               deployments)
                                                                       :empty "Nothing has been deployed yet.")))
                                          (when (some (lambda (d) (not (deployment-final? d))) deployments) (htm (:span :data-poll "1"))))
                                        (:aside
                                          (str (profile-card site project report))
                                          (when (can? :deploy project)
                                            (htm (:form :method "post" :action (format nil "~a/-/deployments" url) :class "card"
                                                   (:h2 :class "card-title" "Deploy")
                                                   (str (the-object site (field "house" "House" :options (mapcar (lambda (h) (cons (string-downcase (symbol-name (getf h :key))) (getf h :name))) *deploy-houses*)
                                                                                :value (and (getf settings :house) (string-downcase (symbol-name (getf settings :house)))))))
                                                   (:p :class "hint" "The default branch at its tip, " (:code (hs (if (the-object view head-oid) (subseq (the-object view head-oid) 0 8) "none")))
                                                       ", is handed to the house, which builds it, checks it against the profile, reads it, and puts it up. The project must be public.")
                                                   (str (the-object site (button "Deploy now" :confirm "Hand the default branch to the house?")))
                                                   (:hr)
                                                   (:label (:input :type "checkbox" :name "on_green" :value "1" :checked (and (getf settings :on-green?) "checked"))
                                                           " deploy on every green pipeline of the default branch")
                                                   (:p (str (the-object site (button "Save settings" :class "btn btn-small btn-quiet" :name "save" :value "1"))))))))))
                                :active :deployments :notice (flash-notice req)))))

(defun deployments-post (req ent project)
  (require-can :deploy project)
  (let ((house (and (param req "house") (intern (string-upcase (param req "house")) :keyword))))
    (if (param req "save")
        (progn (update-record! project :deploy (list :house house :on-green? (boolean-param req "on_green")))
               (redirect req ent (with-notice (format nil "~a/-/deployments" (project-url project)) "Saved.")))
        (handler-case
            (progn (update-record! project :deploy (list :house house :on-green? (getf (getf project :deploy) :on-green?)))
                   (let ((report (profile-report project)))
                     (when (getf report :faults)
                       (redirect req ent (with-notice (format nil "~a/-/deployments" (project-url project))
                                                      (format nil "Not sent: ~a" (first (getf report :faults)))))
                       (return-from deployments-post nil)))
                   (request-hosting! project *current-user* :house-key house)
                   (redirect req ent (with-notice (format nil "~a/-/deployments" (project-url project)) "The house took the request.")))
          (http-error (e) (redirect req ent (with-notice (format nil "~a/-/deployments" (project-url project)) (http-error-text e))))))))
