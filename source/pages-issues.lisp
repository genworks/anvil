;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :anvil)

;;
;; Issues and merge requests: the lists, the new forms, a thing's page
;; with its notes, close and reopen, and for a merge request the
;; commits, the changes and the Merge button.
;;

(defun things-list-page (req ent project kind)
  (let* ((url (project-url project))
         (issue? (eq kind :issue))
         (segment (if issue? "issues" "merge_requests"))
         (state (let ((s (url-query-value req "state"))) (cond ((equal s "closed") :closed) ((equal s "merged") :merged) ((equal s "all") :all) (t :opened))))
         (label (url-query-value req "label"))
         (milestone (url-query-value req "milestone"))
         (things (remove-if-not (lambda (thing) (or (null milestone) (equal (getf thing :milestone) milestone)))
                                (if issue?
                                    (project-issues project :state state :labels (and label (list label)))
                                    (project-merge-requests project :state state)))))
    (respond-html req ent
                  (project-page project (if issue? "Issues" "Merge requests")
                                (html (:div :class "filters"
                                        (loop for (key name) in (if issue? '((:opened "Open") (:closed "Closed") (:all "All")) '((:opened "Open") (:merged "Merged") (:closed "Closed") (:all "All")))
                                              do (htm (:a :class (if (eq key state) "active" "") :href (format nil "~a/-/~a?state=~(~a~)" url segment key) (hs name))))
                                        (when label (htm (:span :class "label" (hs label)) (:a :href (format nil "~a/-/~a" url segment) "clear")))
                                        (when milestone (htm (:span :class "badge" "milestone: " (hs milestone)) (:a :href (format nil "~a/-/~a" url segment) "clear")))
                                        (when issue? (htm (:a :href (format nil "~a/-/milestones" url) "Milestones")))
                                        (when (if issue? (can? :create-issue project) (can? :create-merge-request project))
                                          (htm (str (mutating-link (format nil "~a/-/~a/new" url segment) (if issue? "New issue" "New merge request")
                                                                   :class "btn btn-small" :style "margin-left:auto")))))
                                      (str (html-table '("" "Title" "Author" "Opened" "Notes")
                                                                   (mapcar (lambda (thing)
                                                                             (list (status-badge-html (getf thing :state))
                                                                                   (html (:span :class "title" (:a :href (thing-url project thing) (hs (getf thing :title))))
                                                                                         (unless issue? (htm " " (:code (hs (getf thing :source-branch))) " → " (:code (hs (getf thing :target-branch)))))
                                                                                         (dolist (l (getf thing :labels)) (htm " " (:a :class "label" :href (format nil "~a/-/~a?label=~a" url segment (url-encode l)) (hs l))))
                                                                                         (:div :class "meta" (if issue? "#" "!") (hs (princ-to-string (getf thing :iid)))))
                                                                                   (user-link-html (record :user (getf thing :author-id)))
                                                                                   (time-html (getf thing :created))
                                                                                   (hs (princ-to-string (count-if-not (lambda (n) (getf n :system?)) (getf thing :notes))))))
                                                                           things)
                                                                   :class "thing-list"
                                                                   :empty (if issue? "No issues here." "No merge requests here."))))
                                :active (if issue? :issues :merge-requests) :notice (flash-notice req)))))

(defun new-thing-page (req ent project kind &key error)
  (let* ((url (project-url project)) (issue? (eq kind :issue))
         (view (project-view project)))
    (if issue? (require-can :create-issue project) (require-can :create-merge-request project))
    (respond-html req ent
                  (project-page project (if issue? "New issue" "New merge request")
                                (html (:div :class "narrow" :style "margin-top:0"
                                        (:h2 (if issue? "New issue" "New merge request"))
                                        (when error (htm (:p :class "profile-bad" (hs error))))
                                        (:form :method "post" :action (format nil "~a/-/~a/new" url (if issue? "issues" "merge_requests"))
                                          (unless issue?
                                            (let ((branches (mapcar (lambda (b) (cons (first b) (first b))) (the-object view branches))))
                                              (str (form-field "source" "Source branch" :options branches :value (or (param req "source") (url-query-value req "source"))))
                                              (str (form-field "target" "Target branch" :options branches :value (or (param req "target") (url-query-value req "target") (getf project :default-branch))))))
                                          (str (form-field "title" "Title" :required t :value (or (param req "title") (and (not issue?) (url-query-value req "source")))))
                                          (str (form-field "description" "Description (Markdown)" :rows 10 :value (param req "description")))
                                          (str (form-field "labels" "Labels (comma separated)" :value (param req "labels")
                                                                       :hint (hs (format nil "~{~a~^, ~}" (project-labels project)))))
                                          (let ((milestones (project-milestones project)))
                                            (when milestones
                                              (str (form-field "milestone" "Milestone" :value (param req "milestone")
                                                                           :options (cons (cons "" "none") (mapcar (lambda (m) (cons (getf m :title) (getf m :title))) milestones))))))
                                          (unless issue?
                                            (str (form-field "remove_source" "Delete the source branch when merged" :type "checkbox" :checked t)))
                                          (str (form-button (if issue? "Open issue" "Open merge request"))))))
                                :active (if issue? :issues :merge-requests)))))

(defun new-thing-post (req ent project kind)
  (handler-case
      (let* ((labels (remove "" (mapcar #'trim (split-on (or (param req "labels") "") #\,)) :test #'string=))
             (milestone (let ((m (param req "milestone"))) (and (not (blank? m)) (find-milestone-by-title project m) (trim m))))
             (thing (if (eq kind :issue)
                        (progn (require-can :create-issue project)
                               (make-issue! project *current-user* (param req "title") :description (param req "description") :labels labels :milestone milestone))
                        (progn (require-can :create-merge-request project)
                               (let ((mr (make-merge-request! project *current-user* (param req "title") (param req "source") (param req "target")
                                                              :description (param req "description") :labels labels
                                                              :remove-source-branch? (boolean-param req "remove_source"))))
                                 (if milestone (update-record! mr :milestone milestone) mr))))))
        (deliver-thing-hooks! project thing *current-user* "open")
        (redirect req ent (thing-url project thing)))
    (http-error (e) (new-thing-page req ent project kind :error (http-error-text e)))))

(defun thing-page (req ent project thing)
  (let* ((url (project-url project))
         (issue? (eq (getf thing :kind) :issue))
         (segment (if issue? "issues" "merge_requests"))
         (base (format nil "~a/-/~a/~d" url segment (getf thing :iid)))
         (author (record :user (getf thing :author-id)))
         (open? (eq (getf thing :state) :opened)))
    (respond-html req ent
                  (project-page project (getf thing :title)
                                (html (:div :class "thing-head"
                                        (str (status-badge-html (getf thing :state)))
                                        (when (and (not issue?) (merge-request-draft? thing)) (htm (:span :class "badge" "draft")))
                                        (:h2 (hs (getf thing :title)) " " (:span :class "quiet" (if issue? "#" "!") (hs (princ-to-string (getf thing :iid)))))
                                        (:span :class "quiet" "opened " (str (time-html (getf thing :created))) " by " (str (user-link-html author)))
                                        (dolist (l (getf thing :labels)) (htm (:a :class "label" :href (format nil "~a/-/~a?label=~a" url segment (url-encode l)) (hs l)))))
                                      (:div :class "grid"
                                        (:div
                                          (:div :class "note"
                                            (:div :class "note-head" (str (avatar-html author :size "avatar-small")) (str (user-link-html author)) " wrote")
                                            (:div :class "note-body" (str (prose-html (if (blank? (getf thing :description)) "*No description.*" (getf thing :description))))))
                                          (unless issue? (str (merge-request-panel project thing)))
                                          (dolist (note (getf thing :notes))
                                            (let ((who (record :user (getf note :author-id))))
                                              (if (getf note :system?)
                                                  (htm (:div :class "note system" (str (user-link-html who)) " " (hs (getf note :body)) " " (str (time-html (getf note :created)))))
                                                  (htm (:div :class "note"
                                                         (:div :class "note-head" (str (avatar-html who :size "avatar-small")) (str (user-link-html who)) " " (str (time-html (getf note :created))))
                                                         (:div :class "note-body" (str (prose-html (getf note :body)))))))))
                                          (when (can? :create-note project)
                                            (htm (:form :method "post" :action (format nil "~a/notes" base) :class "card"
                                                   (str (form-field "body" "Comment (Markdown)" :rows 5 :required t))
                                                   (:div :class "actions"
                                                     (str (form-button "Comment"))
                                                     (when (can? :update-issue project)
                                                       (htm (str (form-button (if open? (if issue? "Close issue" "Close merge request") "Reopen") :class "btn btn-quiet"
                                                                                          :formaction (format nil "~a/~a" base (if open? "close" "reopen"))))))))))
                                          (when (and (not (can? :create-note project)) (not *current-user*))
                                            (htm (:p :class "hint" (str (mutating-link (format nil "/users/sign_in?return_to=~a" (url-encode base)) "Sign in")) " to comment."))))
                                        (:aside
                                          (str (card-html (html (:dl :class "kv"
                                                                              (:dt "State") (:dd (hs (status-text (getf thing :state))))
                                                                              (:dt "Author") (:dd (str (user-link-html author)))
                                                                              (:dt "Assignee") (:dd (let ((a (record :user (getf thing :assignee-id)))) (if a (str (user-link-html a)) (htm (:span :class "quiet" "nobody")))))
                                                                              (when (getf thing :milestone)
                                                                                (htm (:dt "Milestone") (:dd (let ((m (find-milestone-by-title project (getf thing :milestone))))
                                                                                                            (if m (htm (:a :href (format nil "~a/-/milestones/~d" url (getf m :iid)) (hs (getf thing :milestone)))) (htm (hs (getf thing :milestone))))))))
                                                                              (unless issue?
                                                                                (htm (:dt "Branches") (:dd (:code (hs (getf thing :source-branch))) " → " (:code (hs (getf thing :target-branch))))))
                                                                              (when (getf thing :merge-commit)
                                                                                (htm (:dt "Merged as") (:dd (:a :href (format nil "~a/-/commit/~a" url (getf thing :merge-commit)) (:code (hs (subseq (getf thing :merge-commit) 0 8))))))))
                                                                            (when (can? :update-issue project)
                                                                              (htm (:form :method "post" :action (format nil "~a/edit" base) :style "margin-top:1rem"
                                                                                     (str (form-field "title" "Title" :value (getf thing :title)))
                                                                                     (str (form-field "labels" "Labels" :value (format nil "~{~a~^, ~}" (getf thing :labels))))
                                                                                     (str (form-field "assignee" "Assignee (username)" :value (let ((a (record :user (getf thing :assignee-id)))) (and a (getf a :username)))))
                                                                                     (str (form-field "milestone" "Milestone" :value (or (getf thing :milestone) "")
                                                                                                                  :options (cons (cons "" "none")
                                                                                                                                 (mapcar (lambda (m) (cons (getf m :title) (getf m :title)))
                                                                                                                                         (let ((active (project-milestones project)) (current (find-milestone-by-title project (getf thing :milestone))))
                                                                                                                                           (if (and current (not (member current active))) (cons current active) active))))))
                                                                                     (str (form-button "Update" :class "btn btn-small"))))))
                                                                      :title "Details")))))
                                :active (if issue? :issues :merge-requests) :notice (flash-notice req)))))

(defun merge-request-panel (project mr)
  (let* ((url (project-url project)) (base (format nil "~a/-/merge_requests/~d" url (getf mr :iid))))
    (multiple-value-bind (repository source target) (merge-request-base project mr)
      (let* ((changes (merge-request-changes project mr))
             (commits (merge-request-commits project mr))
             (open? (eq (getf mr :state) :opened))
             (status (and source target open? (ingot:can-merge? repository source target)))
             (pipeline (and source (latest-pipeline project :sha source)))
             (statuses (and source (commit-statuses project source)))
             (approvals (current-approvals project mr))
             (required (approvals-required project))
             (blockers (and open? (merge-blockers project mr)))
             (mine? (and *current-user* (find (getf *current-user* :id) approvals :key (lambda (a) (getf a :user-id))))))
        (html (:div :class "card"
                (:h2 :class "card-title" (hs (plural (length commits) "commit")) ", " (hs (plural (length changes) "file")) " changed"
                     (when pipeline (htm " · pipeline " (str (status-badge-html (getf pipeline :status) :href (pipeline-url project pipeline)))))
                     (when statuses (htm " · checks " (str (statuses-html project statuses)))))
                (when (and open? (or (plusp required) approvals (can? :push project)))
                  (htm (:p :class "approvals"
                         (cond ((and (plusp required) (zerop (approvals-left project mr))) (htm (:span :class "badge status-success" "approved")))
                               ((plusp required) (htm (:span :class "badge" (hs (format nil "~d of ~d approvals" (length approvals) required)))))
                               (approvals (htm (:span :class "badge status-success" "approved"))))
                         (dolist (a approvals) (let ((who (record :user (getf a :user-id)))) (when who (htm " " (str (user-link-html who))))))
                         (when (can? :push project)
                           (htm (:form :method "post" :action (format nil "~a/~a" base (if mine? "unapprove" "approve")) :style "display:inline;margin-left:.5rem"
                                  (str (form-button (if mine? "Revoke approval" "Approve") :class "btn btn-small btn-quiet"))))))))
                (cond ((not open?))
                      ((null source) (htm (:p :class "profile-bad" "The source branch is gone.")))
                      ((null target) (htm (:p :class "profile-bad" "The target branch is gone.")))
                      ((eq status :already) (htm (:p :class "quiet" "Everything in the source is in the target already.")))
                      ((eq status :conflict) (htm (:p :class "profile-bad" "Cannot be merged automatically: the branches conflict. Merge the target into the source locally and push.")))
                      (blockers (htm (:ul :class "faults" (dolist (b blockers) (htm (:li (hs b)))))))
                      ((can? :merge project)
                       (htm (:form :method "post" :action (format nil "~a/merge" base) :class "actions"
                              (str (form-button (if (eq status :fast-forward) "Merge (fast-forward)" "Merge") :confirm "Merge this request?"))
                              (:label (:input :type "checkbox" :name "remove_source" :value "1" :checked (and (getf mr :remove-source-branch?) "checked")) " delete the source branch"))))
                      (t (htm (:p :class "quiet" (if (eq status :fast-forward) "Can be merged (fast-forward) by a Maintainer." "Can be merged by a Maintainer.")))))
                (when commits
                  (htm (:details (:summary "Commits") (str (commit-list-html project commits)))))
                (when changes
                  (htm (:details :open "open" (:summary "Changes") (str (changes-html repository changes :url url :ref source)))))))))))

(defun approval-post (req ent project mr action)
  (require-can :push project)
  (handler-case
      (progn (ecase action
               (:approve (approve-merge-request! project mr *current-user*))
               (:unapprove (unapprove-merge-request! project mr *current-user*)))
             (redirect req ent (thing-url project mr)))
    (http-error (e) (redirect req ent (with-notice (thing-url project mr) (http-error-text e))))))

(defun note-post (req ent project thing)
  (require-can :create-note project)
  (let ((updated (add-note! thing *current-user* (param req "body"))))
    (deliver-thing-hooks! project updated *current-user* "note")
    (redirect req ent (thing-url project thing))))

(defun thing-state-post (req ent project thing action)
  (require-can :update-issue project)
  (let ((updated (ecase action
                   (:close (close-thing! thing *current-user*))
                   (:reopen (reopen-thing! thing *current-user*)))))
    (deliver-thing-hooks! project updated *current-user* (string-downcase (symbol-name action)))
    (redirect req ent (thing-url project thing))))

(defun thing-edit-post (req ent project thing)
  (require-can :update-issue project)
  (let* ((assignee (let ((name (param req "assignee"))) (and (not (blank? name)) (find-user (trim name)))))
         (updated (update-record! thing :title (if (blank? (param req "title")) (getf thing :title) (trim (param req "title")))
                                        :labels (remove "" (mapcar #'trim (split-on (or (param req "labels") "") #\,)) :test #'string=)
                                        :assignee-id (and assignee (getf assignee :id))
                                        :milestone (let ((m (param req "milestone"))) (and (not (blank? m)) (trim m))))))
    (deliver-thing-hooks! project updated *current-user* "update")
    (redirect req ent (thing-url project thing))))

(defun merge-post (req ent project mr)
  (require-can :merge project)
  (handler-case
      (progn (merge-merge-request! project mr *current-user* :remove-source? (boolean-param req "remove_source"))
             (redirect req ent (with-notice (thing-url project mr) "Merged.")))
    (http-error (e) (redirect req ent (with-notice (thing-url project mr) (http-error-text e))))))

;;; Milestones

(defun milestone-progress-html (project milestone)
  "The count of things done and open under MILESTONE, and a bar."
  (multiple-value-bind (issues mrs) (milestone-things project milestone)
    (let* ((things (append issues mrs))
           (done (count-if (lambda (thing) (not (eq (getf thing :state) :opened))) things))
           (total (length things))
           (percent (if (zerop total) 0 (round (* 100 done) total))))
      (html (:div :class "progress" :title (format nil "~d of ~d done" done total)
              (:span :style (format nil "width:~d%" percent)))
            (:span :class "quiet" (hs (format nil "~d open · ~d closed · ~d%" (- total done) done percent)))))))

(defun milestones-page (req ent project)
  (let* ((url (project-url project))
         (state (let ((s (url-query-value req "state"))) (cond ((equal s "closed") :closed) ((equal s "all") :all) (t :active))))
         (milestones (project-milestones project :state state)))
    (respond-html req ent
                  (project-page project "Milestones"
                                (html (:div :class "filters"
                                        (loop for (key name) in '((:active "Open") (:closed "Closed") (:all "All"))
                                              do (htm (:a :class (if (eq key state) "active" "") :href (format nil "~a/-/milestones?state=~(~a~)" url key) (hs name))))
                                        (:a :href (format nil "~a/-/issues" url) "Issues")
                                        (when (can? :update-issue project)
                                          (htm (str (mutating-link (format nil "~a/-/milestones/new" url) "New milestone" :class "btn btn-small" :style "margin-left:auto")))))
                                      (str (html-table '("" "Milestone" "Due" "Progress")
                                                       (mapcar (lambda (m)
                                                                 (list (status-badge-html (if (eq (getf m :state) :active) :opened :closed))
                                                                       (html (:span :class "title" (:a :href (format nil "~a/-/milestones/~d" url (getf m :iid)) (hs (getf m :title))))
                                                                             (unless (blank? (getf m :description)) (htm (:div :class "meta" (hs (subseq (getf m :description) 0 (min 120 (length (getf m :description)))))))))
                                                                       (hs (or (getf m :due-date) ""))
                                                                       (milestone-progress-html project m)))
                                                               milestones)
                                                       :class "thing-list" :empty "No milestones here.")))
                                :active :issues :notice (flash-notice req)))))

(defun milestone-page (req ent project milestone)
  (let ((url (project-url project)) (base (format nil "~a/-/milestones/~d" (project-url project) (getf milestone :iid))))
    (multiple-value-bind (issues mrs) (milestone-things project milestone)
      (respond-html req ent
                    (project-page project (getf milestone :title)
                                  (html (:div :class "thing-head"
                                          (str (status-badge-html (if (eq (getf milestone :state) :active) :opened :closed)))
                                          (:h2 (hs (getf milestone :title)) " " (:span :class "quiet" "%" (hs (princ-to-string (getf milestone :iid)))))
                                          (when (getf milestone :due-date) (htm (:span :class "quiet" "due " (hs (getf milestone :due-date)))))
                                          (when (getf milestone :start-date) (htm (:span :class "quiet" "from " (hs (getf milestone :start-date))))))
                                        (unless (blank? (getf milestone :description))
                                          (htm (:div :class "card prose" (str (markdown-html (getf milestone :description))))))
                                        (:div :class "card" (str (milestone-progress-html project milestone)))
                                        (:div :class "grid"
                                          (:div (:h3 (hs (plural (length issues) "issue")))
                                                (:ul :class "thing-list"
                                                  (dolist (issue issues)
                                                    (htm (:li (str (status-badge-html (getf issue :state))) " " (:span :class "title" (:a :href (thing-url project issue) (hs (getf issue :title))))))))
                                                (:h3 (hs (plural (length mrs) "merge request")))
                                                (:ul :class "thing-list"
                                                  (dolist (mr mrs)
                                                    (htm (:li (str (status-badge-html (getf mr :state))) " " (:span :class "title" (:a :href (thing-url project mr) (hs (getf mr :title)))))))))
                                          (:aside
                                            (when (can? :update-issue project)
                                              (str (card-html (html (:form :method "post" :action (format nil "~a/~a" base (if (eq (getf milestone :state) :active) "close" "reopen"))
                                                                      (str (form-button (if (eq (getf milestone :state) :active) "Close milestone" "Reopen milestone") :class "btn btn-small btn-quiet")))
                                                                    (:form :method "post" :action (format nil "~a/delete" base) :style "margin-top:.5rem"
                                                                      (str (form-button "Delete" :class "btn btn-small btn-quiet" :confirm "Delete the milestone? Its issues stay, unassigned.")))
                                                                    (:p :class "hint" (:a :href (format nil "~a/-/issues?milestone=~a" url (url-encode (getf milestone :title))) "The open issues, as a list")))
                                                              :title "Actions"))))))
                                  :active :issues :notice (flash-notice req))))))

(defun new-milestone-page (req ent project &key error)
  (require-can :update-issue project)
  (respond-html req ent
                (project-page project "New milestone"
                              (html (:div :class "narrow" :style "margin-top:0"
                                      (:h2 "New milestone")
                                      (when error (htm (:p :class "profile-bad" (hs error))))
                                      (:form :method "post" :action (format nil "~a/-/milestones/new" (project-url project))
                                        (str (form-field "title" "Title" :required t :value (param req "title")))
                                        (str (form-field "description" "Description (Markdown)" :rows 6 :value (param req "description")))
                                        (str (form-field "start_date" "Start date" :type "date" :value (param req "start_date")))
                                        (str (form-field "due_date" "Due date" :type "date" :value (param req "due_date")))
                                        (str (form-button "Create milestone")))))
                              :active :issues)))

(defun new-milestone-post (req ent project)
  (require-can :update-issue project)
  (handler-case
      (let ((milestone (make-milestone! project (param req "title") :description (param req "description")
                                        :due-date (param req "due_date") :start-date (param req "start_date"))))
        (redirect req ent (with-notice (format nil "~a/-/milestones/~d" (project-url project) (getf milestone :iid)) "Milestone made.")))
    (http-error (e) (new-milestone-page req ent project :error (http-error-text e)))))

(defun milestone-state-post (req ent project milestone action)
  (require-can :update-issue project)
  (set-milestone-state! project milestone (ecase action (:close :closed) (:reopen :active)))
  (redirect req ent (format nil "~a/-/milestones/~d" (project-url project) (getf milestone :iid))))

(defun milestone-delete-post (req ent project milestone)
  (require-can :update-issue project)
  (delete-milestone! project milestone)
  (redirect req ent (with-notice (format nil "~a/-/milestones" (project-url project)) "Milestone deleted.")))
