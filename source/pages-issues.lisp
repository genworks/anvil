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
  (let* ((site (site)) (url (project-url project))
         (issue? (eq kind :issue))
         (segment (if issue? "issues" "merge_requests"))
         (state (let ((s (url-query-value req "state"))) (cond ((equal s "closed") :closed) ((equal s "merged") :merged) ((equal s "all") :all) (t :opened))))
         (label (url-query-value req "label"))
         (things (if issue?
                     (project-issues project :state state :labels (and label (list label)))
                     (project-merge-requests project :state state))))
    (respond-html req ent
                  (project-page project (if issue? "Issues" "Merge requests")
                                (html (:div :class "filters"
                                        (loop for (key name) in (if issue? '((:opened "Open") (:closed "Closed") (:all "All")) '((:opened "Open") (:merged "Merged") (:closed "Closed") (:all "All")))
                                              do (htm (:a :class (if (eq key state) "active" "") :href (format nil "~a/-/~a?state=~(~a~)" url segment key) (hs name))))
                                        (when label (htm (:span :class "label" (hs label)) (:a :href (format nil "~a/-/~a" url segment) "clear")))
                                        (when (if issue? (can? :create-issue project) (can? :create-merge-request project))
                                          (htm (str (mutating-link (format nil "~a/-/~a/new" url segment) (if issue? "New issue" "New merge request")
                                                                   :class "btn btn-small" :style "margin-left:auto")))))
                                      (str (the-object site (table '("" "Title" "Author" "Opened" "Notes")
                                                                   (mapcar (lambda (thing)
                                                                             (list (the-object site (status-badge (getf thing :state)))
                                                                                   (html (:span :class "title" (:a :href (thing-url project thing) (hs (getf thing :title))))
                                                                                         (unless issue? (htm " " (:code (hs (getf thing :source-branch))) " → " (:code (hs (getf thing :target-branch)))))
                                                                                         (dolist (l (getf thing :labels)) (htm " " (:a :class "label" :href (format nil "~a/-/~a?label=~a" url segment (url-encode l)) (hs l))))
                                                                                         (:div :class "meta" (if issue? "#" "!") (hs (princ-to-string (getf thing :iid)))))
                                                                                   (the-object site (user-link (record :user (getf thing :author-id))))
                                                                                   (the-object site (time-html (getf thing :created)))
                                                                                   (hs (princ-to-string (count-if-not (lambda (n) (getf n :system?)) (getf thing :notes))))))
                                                                           things)
                                                                   :class "thing-list"
                                                                   :empty (if issue? "No issues here." "No merge requests here.")))))
                                :active (if issue? :issues :merge-requests) :notice (flash-notice req)))))

(defun new-thing-page (req ent project kind &key error)
  (let* ((site (site)) (url (project-url project)) (issue? (eq kind :issue))
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
                                              (str (the-object site (field "source" "Source branch" :options branches :value (or (param req "source") (url-query-value req "source")))))
                                              (str (the-object site (field "target" "Target branch" :options branches :value (or (param req "target") (url-query-value req "target") (getf project :default-branch)))))))
                                          (str (the-object site (field "title" "Title" :required t :value (or (param req "title") (and (not issue?) (url-query-value req "source"))))))
                                          (str (the-object site (field "description" "Description (Markdown)" :rows 10 :value (param req "description"))))
                                          (str (the-object site (field "labels" "Labels (comma separated)" :value (param req "labels")
                                                                       :hint (hs (format nil "~{~a~^, ~}" (project-labels project))))))
                                          (unless issue?
                                            (str (the-object site (field "remove_source" "Delete the source branch when merged" :type "checkbox" :checked t))))
                                          (str (the-object site (button (if issue? "Open issue" "Open merge request")))))))
                                :active (if issue? :issues :merge-requests)))))

(defun new-thing-post (req ent project kind)
  (handler-case
      (let* ((labels (remove "" (mapcar #'trim (split-on (or (param req "labels") "") #\,)) :test #'string=))
             (thing (if (eq kind :issue)
                        (progn (require-can :create-issue project)
                               (make-issue! project *current-user* (param req "title") :description (param req "description") :labels labels))
                        (progn (require-can :create-merge-request project)
                               (make-merge-request! project *current-user* (param req "title") (param req "source") (param req "target")
                                                    :description (param req "description") :labels labels
                                                    :remove-source-branch? (boolean-param req "remove_source"))))))
        (deliver-thing-hooks! project thing *current-user* "open")
        (redirect req ent (thing-url project thing)))
    (http-error (e) (new-thing-page req ent project kind :error (http-error-text e)))))

(defun thing-page (req ent project thing)
  (let* ((site (site)) (url (project-url project))
         (issue? (eq (getf thing :kind) :issue))
         (segment (if issue? "issues" "merge_requests"))
         (base (format nil "~a/-/~a/~d" url segment (getf thing :iid)))
         (author (record :user (getf thing :author-id)))
         (open? (eq (getf thing :state) :opened)))
    (respond-html req ent
                  (project-page project (getf thing :title)
                                (html (:div :class "thing-head"
                                        (str (the-object site (status-badge (getf thing :state))))
                                        (:h2 (hs (getf thing :title)) " " (:span :class "quiet" (if issue? "#" "!") (hs (princ-to-string (getf thing :iid)))))
                                        (:span :class "quiet" "opened " (str (the-object site (time-html (getf thing :created)))) " by " (str (the-object site (user-link author))))
                                        (dolist (l (getf thing :labels)) (htm (:a :class "label" :href (format nil "~a/-/~a?label=~a" url segment (url-encode l)) (hs l)))))
                                      (:div :class "grid"
                                        (:div
                                          (:div :class "note"
                                            (:div :class "note-head" (str (the-object site (avatar author :size "avatar-small"))) (str (the-object site (user-link author))) " wrote")
                                            (:div :class "note-body" (str (the-object site (markdown (if (blank? (getf thing :description)) "*No description.*" (getf thing :description)))))))
                                          (unless issue? (str (merge-request-panel site project thing)))
                                          (dolist (note (getf thing :notes))
                                            (let ((who (record :user (getf note :author-id))))
                                              (if (getf note :system?)
                                                  (htm (:div :class "note system" (str (the-object site (user-link who))) " " (hs (getf note :body)) " " (str (the-object site (time-html (getf note :created))))))
                                                  (htm (:div :class "note"
                                                         (:div :class "note-head" (str (the-object site (avatar who :size "avatar-small"))) (str (the-object site (user-link who))) " " (str (the-object site (time-html (getf note :created)))))
                                                         (:div :class "note-body" (str (the-object site (markdown (getf note :body))))))))))
                                          (when (can? :create-note project)
                                            (htm (:form :method "post" :action (format nil "~a/notes" base) :class "card"
                                                   (str (the-object site (field "body" "Comment (Markdown)" :rows 5 :required t)))
                                                   (:div :class "actions"
                                                     (str (the-object site (button "Comment")))
                                                     (when (can? :update-issue project)
                                                       (htm (str (the-object site (button (if open? (if issue? "Close issue" "Close merge request") "Reopen") :class "btn btn-quiet"
                                                                                          :formaction (format nil "~a/~a" base (if open? "close" "reopen")))))))))))
                                          (when (and (not (can? :create-note project)) (not *current-user*))
                                            (htm (:p :class "hint" (str (mutating-link (format nil "/users/sign_in?return_to=~a" (url-encode base)) "Sign in")) " to comment."))))
                                        (:aside
                                          (str (the-object site (card (html (:dl :class "kv"
                                                                              (:dt "State") (:dd (hs (status-text (getf thing :state))))
                                                                              (:dt "Author") (:dd (str (the-object site (user-link author))))
                                                                              (:dt "Assignee") (:dd (let ((a (record :user (getf thing :assignee-id)))) (if a (str (the-object site (user-link a))) (htm (:span :class "quiet" "nobody")))))
                                                                              (when (getf thing :milestone) (htm (:dt "Milestone") (:dd (hs (getf thing :milestone)))))
                                                                              (unless issue?
                                                                                (htm (:dt "Branches") (:dd (:code (hs (getf thing :source-branch))) " → " (:code (hs (getf thing :target-branch))))))
                                                                              (when (getf thing :merge-commit)
                                                                                (htm (:dt "Merged as") (:dd (:a :href (format nil "~a/-/commit/~a" url (getf thing :merge-commit)) (:code (hs (subseq (getf thing :merge-commit) 0 8))))))))
                                                                            (when (can? :update-issue project)
                                                                              (htm (:form :method "post" :action (format nil "~a/edit" base) :style "margin-top:1rem"
                                                                                     (str (the-object site (field "title" "Title" :value (getf thing :title))))
                                                                                     (str (the-object site (field "labels" "Labels" :value (format nil "~{~a~^, ~}" (getf thing :labels)))))
                                                                                     (str (the-object site (field "assignee" "Assignee (username)" :value (let ((a (record :user (getf thing :assignee-id)))) (and a (getf a :username))))))
                                                                                     (str (the-object site (field "milestone" "Milestone" :value (getf thing :milestone))))
                                                                                     (str (the-object site (button "Update" :class "btn btn-small")))))))
                                                                      :title "Details"))))))
                                :active (if issue? :issues :merge-requests) :notice (flash-notice req)))))

(defun merge-request-panel (site project mr)
  (let* ((url (project-url project)) (base (format nil "~a/-/merge_requests/~d" url (getf mr :iid))))
    (multiple-value-bind (repository source target) (merge-request-base project mr)
      (let* ((changes (merge-request-changes project mr))
             (commits (merge-request-commits project mr))
             (status (and source target (eq (getf mr :state) :opened) (ingot:can-merge? repository source target)))
             (pipeline (and source (latest-pipeline project :sha source))))
        (html (:div :class "card"
                (:h2 :class "card-title" (hs (plural (length commits) "commit")) ", " (hs (plural (length changes) "file")) " changed"
                     (when pipeline (htm " · pipeline " (str (the-object site (status-badge (getf pipeline :status) :href (pipeline-url project pipeline)))))))
                (cond ((not (eq (getf mr :state) :opened)))
                      ((null source) (htm (:p :class "profile-bad" "The source branch is gone.")))
                      ((null target) (htm (:p :class "profile-bad" "The target branch is gone.")))
                      ((eq status :already) (htm (:p :class "quiet" "Everything in the source is in the target already.")))
                      ((eq status :conflict) (htm (:p :class "profile-bad" "Cannot be merged automatically: the branches conflict. Merge the target into the source locally and push.")))
                      ((can? :merge project)
                       (htm (:form :method "post" :action (format nil "~a/merge" base) :class "actions"
                              (str (the-object site (button (if (eq status :fast-forward) "Merge (fast-forward)" "Merge") :confirm "Merge this request?")))
                              (:label (:input :type "checkbox" :name "remove_source" :value "1" :checked (and (getf mr :remove-source-branch?) "checked")) " delete the source branch"))))
                      (t (htm (:p :class "quiet" (if (eq status :fast-forward) "Can be merged (fast-forward) by a Maintainer." "Can be merged by a Maintainer.")))))
                (when commits
                  (htm (:details (:summary "Commits") (str (commit-list-html site project commits)))))
                (when changes
                  (htm (:details :open "open" (:summary "Changes") (str (changes-html repository changes :url url :ref source)))))))))))

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
