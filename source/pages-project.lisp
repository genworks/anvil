;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :anvil)

;;
;; A project's frame (its header and tabs) and its home page: the
;; README, the tree of the default branch, the clone address, the last
;; pipeline, the profile and the Deploy button.
;;

(defun project-header-html (project view active)
  (let* ((site (site))
         (level (project-access-level project))
         (latest (latest-pipeline project :ref (getf project :default-branch)))
         (url (project-url project)))
    (html
      (:div :class "project-head"
        (:div (:h1 (:a :href (format nil "/~a" (project-namespace-path project)) (hs (project-namespace-path project)))
                   " / " (:a :href url (hs (getf project :name)))
                   " " (:span :class "visibility" (hs (string-downcase (symbol-name (getf project :visibility))))))
              (unless (blank? (getf project :description)) (htm (:p :class "desc" (hs (getf project :description))))))
        (:div :class "actions"
          (when latest (str (the-object site (status-badge (getf latest :status) :href (pipeline-url project latest)))))
          (when *current-user*
            (htm (:form :method "post" :action (format nil "~a/-/star" url) :style "display:inline"
                   (:button :class "btn btn-small btn-quiet" :type "submit"
                     (if (member (getf *current-user* :id) (getf project :stars)) "★ Unstar" "☆ Star")
                     " " (hs (princ-to-string (or (getf project :star-count) 0)))))))
          (when (and level (>= level 40) *deploy-houses*)
            (htm (str (mutating-link (format nil "~a/-/deployments" url) "Deploy" :class "btn btn-small"))))
          (when (and level (>= level 40))
            (htm (:a :class "btn btn-small btn-quiet" :href (format nil "~a/-/settings" url) "Settings")))))
      (str (the-object site (tabs (remove nil
                                          (list (list :code "Code" url)
                                                (and (getf project :issues-enabled? t)
                                                     (list :issues "Issues" (format nil "~a/-/issues" url) (princ-to-string (length (project-issues project)))))
                                                (and (getf project :merge-requests-enabled? t)
                                                     (list :merge-requests "Merge requests" (format nil "~a/-/merge_requests" url) (princ-to-string (length (project-merge-requests project)))))
                                                (and (getf project :ci-enabled? t)
                                                     (list :pipelines "Pipelines" (format nil "~a/-/pipelines" url)))
                                                (and *deploy-houses* (list :deployments "Deployments" (format nil "~a/-/deployments" url)))
                                                (list :commits "Commits" (format nil "~a/-/commits/~a" url (url-encode (getf project :default-branch))))
                                                (list :branches "Branches" (format nil "~a/-/branches" url) (princ-to-string (length (the-object view branches))))
                                                (list :tags "Tags" (format nil "~a/-/tags" url) (princ-to-string (length (the-object view tags))))))
                                  active))))))

(defun project-page (project title body &key (active :code) notice wide?)
  "A whole page in the project's frame."
  (let ((view (project-view project)))
    (the-object (site) (page (format nil "~a · ~a" title (getf project :path-with-namespace))
                             body
                             :header (project-header-html project view active)
                             :notice notice :wide? wide?))))

(defun project-home-page (req ent project)
  (let* ((site (site))
         (view (project-view project))
         (url (project-url project)))
    (respond-html req ent
                  (project-page project "Code"
                                (if (the-object view empty?)
                                    (empty-repository-html project)
                                    (html
                                      (:div :class "grid"
                                        (:div
                                          (str (ref-picker-html project (the-object view default-branch) "tree" ""))
                                          (str (tree-table-html project (the-object view default-branch) (the-object view head-oid) "" (the-object view root-entries)))
                                          (when (the-object view readme-html)
                                            (htm (:div :class "card" :style "margin-top:1rem"
                                                   (:h2 :class "card-title" (hs (ingot:tree-entry-name (the-object view readme-entry))))
                                                   (:div :class "prose" (str (the-object view readme-html)))))))
                                        (:aside
                                          (str (the-object site (card (html (:div :class "clone-box"
                                                                              (:input :type "text" :readonly "readonly" :value (project-clone-url project) :onclick "this.select()")
                                                                              (:button :type "button" :class "btn btn-small btn-quiet" :data-copy (project-clone-url project) "copy"))
                                                                            (:p :class "hint" (:a :href (format nil "~a/-/archive/~a/~a-~a.tar.gz" url (url-encode (the-object view default-branch)) (getf project :path) (ref-slug (the-object view default-branch))) "Download tar.gz")
                                                                                " · " (:a :href (format nil "~a/-/archive/~a/~a-~a.zip" url (url-encode (the-object view default-branch)) (getf project :path) (ref-slug (the-object view default-branch))) "zip")))
                                                                      :title "Clone")))
                                          (str (the-object site (card (html (:dl :class "kv"
                                                                              (:dt "Commits") (:dd (:a :href (format nil "~a/-/commits/~a" url (url-encode (the-object view default-branch))) (hs (princ-to-string (the-object view commit-count)))))
                                                                              (:dt "Files") (:dd (hs (princ-to-string (the-object view file-count))))
                                                                              (:dt "Default branch") (:dd (:code (hs (the-object view default-branch))))
                                                                              (:dt "Last commit") (:dd (str (the-object site (time-html (git-time-universal (ingot:commit-committer (the-object view head-commit)))))))
                                                                              (when (the-object view ci-file)
                                                                                (htm (:dt "CI") (:dd (:a :href (format nil "~a/-/blob/~a/~a" url (url-encode (the-object view default-branch)) (the-object view ci-file)) (hs (the-object view ci-file))))))))
                                                                      :title "About")))
                                          (when *deploy-houses*
                                            (str (profile-card site project (the-object view profile))))))))
                                :notice (flash-notice req)))))

(defun profile-card (site project report)
  (let ((faults (getf report :faults)) (url (project-url project)))
    (the-object site (card (html (cond ((null report) (htm (:p :class "quiet" "No commits yet.")))
                                       ((null faults)
                                        (htm (:p :class "profile-ok" "✓ Meets the hosting profile"
                                                 (when (getf report :name) (htm " as " (:code (hs (getf report :name))))))
                                             (when (can? :deploy project)
                                               (htm (:p (str (mutating-link (format nil "~a/-/deployments" url) "Deploy" :class "btn btn-small")))))))
                                       (t (htm (:p :class "profile-bad" "✗ Not yet in the hosting profile")
                                               (:ul :class "faults" (dolist (fault faults) (htm (:li (hs fault)))))
                                               (:p :class "hint" "An " (:code "image.sexp") " and a " (:code "monetize.sexp") " at the root; the house's profile says the rest.")))))
                           :title "Hosting"))))

(defun empty-repository-html (project)
  (html (:div :class "card"
          (:h2 "An empty repository")
          (:p "Push something, or start with a README.")
          (:pre (hs (format nil "git clone ~a~%cd ~a~%echo '# ~a' > README.md~%git add README.md~%git commit -m 'First'~%git push -u origin ~a"
                             (project-clone-url project) (getf project :path) (getf project :name) (getf project :default-branch))))
          (:p "Or, with a repository you have:")
          (:pre (hs (format nil "git remote add origin ~a~%git push -u origin --all~%git push origin --tags" (project-clone-url project))))
          (when (can? :push project)
            (htm (:form :method "post" :action (format nil "~a/-/new/~a/README.md" (project-url project) (url-encode (getf project :default-branch)))
                   (:input :type "hidden" :name "content" :value (format nil "# ~a~%" (getf project :name)))
                   (:input :type "hidden" :name "message" :value "Add README")
                   (:button :class "btn" :type "submit" "Add a README here")))))))

(defun ref-picker-html (project ref kind path)
  "The branch and tag menu that keeps KIND (tree, blob, commits) and PATH."
  (let ((view (project-view project)) (url (project-url project)))
    (html (:form :class "ref-pick" :method "get" :action (format nil "~a/-/switch" url)
            (:input :type "hidden" :name "kind" :value kind)
            (:input :type "hidden" :name "path" :value path)
            (:select :name "ref" :onchange "this.form.submit()" :aria-label "Branch or tag"
              (:optgroup :label "Branches"
                (dolist (entry (the-object view branches))
                  (htm (:option :value (first entry) :selected (and (string= (first entry) ref) "selected") (hs (first entry))))))
              (when (the-object view tags)
                (htm (:optgroup :label "Tags"
                       (dolist (entry (the-object view tags))
                         (htm (:option :value (first entry) :selected (and (string= (first entry) ref) "selected") (hs (first entry)))))))))
            (when (and (ingot:oid? ref) (not (find ref (the-object view branches) :key #'first :test #'string=)))
              (htm (:code (hs (subseq ref 0 8)))))
            (:noscript (:button :type "submit" :class "btn btn-small btn-quiet" "Go"))
            (when (plusp (length path))
              (htm (:span :class "quiet" (:a :href (format nil "~a/-/tree/~a" url (url-encode ref)) (hs (getf project :path)))
                          (let ((parts (split-on path #\/)) (so-far ""))
                            (dolist (part parts)
                              (setq so-far (if (string= so-far "") part (format nil "~a/~a" so-far part)))
                              (htm " / " (:a :href (format nil "~a/-/tree/~a/~a" url (url-encode ref) (path-encode so-far)) (hs part))))))))))))

(defun tree-table-html (project ref commit-oid path entries)
  "The table of ENTRIES at PATH in the tree of COMMIT-OID, each with the
last commit that touched it (the twenty newest commits are walked for
that, which answers most trees; what they do not reach says nothing)."
  (let* ((site (site)) (url (project-url project))
         (repository (project-repository project))
         (touches (last-touches repository commit-oid path entries)))
    (html (:div :class "table-wrap"
            (:table :class "tree-table"
              (:tbody
                (when (plusp (length path))
                  (let ((parent (let ((slash (position #\/ path :from-end t))) (if slash (subseq path 0 slash) ""))))
                    (htm (:tr (:td "") (:td :class "name" (:a :href (format nil "~a/-/tree/~a~@[/~a~]" url (url-encode ref) (and (plusp (length parent)) (path-encode parent))) "..")) (:td) (:td)))))
                (dolist (entry (append (remove-if-not #'ingot::tree-entry-tree? entries)
                                       (remove-if #'ingot::tree-entry-tree? entries)))
                  (let* ((name (ingot:tree-entry-name entry))
                         (entry-path (if (string= path "") name (format nil "~a/~a" path name)))
                         (kind (ingot:tree-entry-kind entry))
                         (touch (cdr (assoc name touches :test #'string=))))
                    (htm (:tr (:td (str (case kind (:tree "📁") (:submodule "📦") (:symlink "🔗") (t "📄"))))
                              (:td :class "name"
                                   (case kind
                                     (:tree (htm (:a :href (format nil "~a/-/tree/~a/~a" url (url-encode ref) (path-encode entry-path)) (hs name))))
                                     (:submodule (htm (hs name) " @ " (:code (hs (subseq (ingot:tree-entry-oid entry) 0 8)))))
                                     (t (htm (:a :href (format nil "~a/-/blob/~a/~a" url (url-encode ref) (path-encode entry-path)) (hs name))))))
                              (:td :class "msg" (when touch (htm (:a :href (format nil "~a/-/commit/~a" url (ingot:commit-oid touch)) :class "quiet" (hs (ingot:commit-subject touch))))))
                              (:td :class "when" (when touch (str (the-object site (time-html (git-time-universal (ingot:commit-committer touch)))))))))))))))))

(defun last-touches (repository commit-oid path entries &key (depth 30))
  "An alist of entry name to the newest commit (among the DEPTH newest)
that changed it."
  (let ((pending (make-hash-table :test #'equal)) (found nil))
    (dolist (entry entries) (setf (gethash (ingot:tree-entry-name entry) pending) (ingot:tree-entry-oid entry)))
    (let ((commits (ignore-errors (ingot:log-commits repository commit-oid :limit depth))))
      (loop for (commit next) on commits
            while (plusp (hash-table-count pending))
            do (let* ((parent (first (ingot:commit-parents commit)))
                      (parent-commit (or (and next (string= (ingot:commit-oid next) (or parent "")) next)
                                         (and parent (ignore-errors (ingot:read-commit repository parent)))))
                      (parent-entries (and parent-commit
                                           (let ((e (ingot:tree-entry-at repository (ingot:commit-tree parent-commit) path)))
                                             (and e (ingot::tree-entry-tree? e) (ingot:read-tree repository (ingot:tree-entry-oid e))))))
                      (this-entries (let ((e (ingot:tree-entry-at repository (ingot:commit-tree commit) path)))
                                      (and e (ingot::tree-entry-tree? e) (ingot:read-tree repository (ingot:tree-entry-oid e))))))
                 (maphash (lambda (name oid)
                            (let ((here (find name this-entries :key #'ingot:tree-entry-name :test #'string=))
                                  (there (find name parent-entries :key #'ingot:tree-entry-name :test #'string=)))
                              (when (and here (string= (ingot:tree-entry-oid here) oid)
                                         (not (and there (string= (ingot:tree-entry-oid there) oid))))
                                (push (cons name commit) found)
                                (remhash name pending))))
                          pending))))
    found))

;;; Branches, tags

(defun branches-page (req ent project)
  (let* ((site (site)) (view (project-view project)) (url (project-url project))
         (default (getf project :default-branch)))
    (respond-html req ent
                  (project-page project "Branches"
                                (html (:h2 "Branches")
                                      (str (the-object site (table '("Branch" "Last commit" "" "")
                                                                   (mapcar (lambda (entry)
                                                                             (destructuring-bind (name oid commit) entry
                                                                               (list (html (:a :href (format nil "~a/-/tree/~a" url (url-encode name)) (:strong (hs name)))
                                                                                           (when (string= name default) (htm " " (:span :class "badge" "default")))
                                                                                           (when (protected-branch? project name) (htm " " (:span :class "badge" "protected"))))
                                                                                     (if commit
                                                                                         (html (:a :href (format nil "~a/-/commit/~a" url oid) (hs (ingot:commit-subject commit))) " "
                                                                                               (str (the-object site (time-html (git-time-universal (ingot:commit-committer commit))))))
                                                                                         "")
                                                                                     (if (string= name default) ""
                                                                                         (html (:a :href (format nil "~a/-/compare?from=~a&to=~a" url (url-encode default) (url-encode name)) "compare")
                                                                                               (when (can? :create-merge-request project)
                                                                                                 (htm " · " (str (mutating-link (format nil "~a/-/merge_requests/new?source=~a" url (url-encode name)) "merge request"))))))
                                                                                     (if (and (not (string= name default)) (can-push-branch? project name))
                                                                                         (html (:form :method "post" :action (format nil "~a/-/branches/~a/delete" url (url-encode name))
                                                                                                 (str (the-object site (button "Delete" :class "btn btn-small btn-quiet" :confirm (format nil "Delete the branch ~a?" name))))))
                                                                                         ""))))
                                                                           (the-object view branches))
                                                                   :empty "No branches.")))
                                      (when (can? :push project)
                                        (htm (:form :method "post" :action (format nil "~a/-/branches" url) :class "actions"
                                               (:input :type "text" :name "name" :placeholder "new-branch" :required t)
                                               (:span "from")
                                               (:input :type "text" :name "from" :value default)
                                               (str (the-object site (button "Create branch" :class "btn btn-small")))))))
                                :active :branches :notice (flash-notice req)))))

(defun branch-create-post (req ent project)
  (require-can :push project)
  (let* ((repository (project-repository project))
         (name (trim (param req "name")))
         (from (ingot:resolve-commit repository (or (param req "from") (getf project :default-branch)))))
    (unless (ingot::valid-ref-name? (format nil "refs/heads/~a" name)) (bad-request "~s is not a branch name." name))
    (unless from (bad-request "No such starting point."))
    (ingot:update-ref! repository (format nil "refs/heads/~a" name) from :old nil)
    (project-changed! project)
    (redirect req ent (with-notice (format nil "~a/-/branches" (project-url project)) (format nil "Branch ~a created." name)))))

(defun branch-delete-post (req ent project name)
  (unless (can-push-branch? project name) (forbidden))
  (when (string= name (getf project :default-branch)) (bad-request "The default branch stays."))
  (ingot:delete-ref! (project-repository project) (format nil "refs/heads/~a" name))
  (project-changed! project)
  (redirect req ent (with-notice (format nil "~a/-/branches" (project-url project)) (format nil "Branch ~a deleted." name))))

(defun tags-page (req ent project)
  (let* ((site (site)) (view (project-view project)) (url (project-url project)))
    (respond-html req ent
                  (project-page project "Tags"
                                (html (:h2 "Tags")
                                      (str (the-object site (table '("Tag" "Commit" "Download")
                                                                   (mapcar (lambda (entry)
                                                                             (destructuring-bind (name oid commit) entry
                                                                               (declare (ignore oid))
                                                                               (list (html (:a :href (format nil "~a/-/tree/~a" url (url-encode name)) (:strong (hs name))))
                                                                                     (if commit (html (:a :href (format nil "~a/-/commit/~a" url (ingot:commit-oid commit)) (hs (ingot:commit-subject commit)))
                                                                                                      " " (str (the-object site (time-html (git-time-universal (ingot:commit-committer commit)))))) "")
                                                                                     (html (:a :href (format nil "~a/-/archive/~a/~a-~a.tar.gz" url (url-encode name) (getf project :path) (ref-slug name)) "tar.gz")
                                                                                           " · " (:a :href (format nil "~a/-/archive/~a/~a-~a.zip" url (url-encode name) (getf project :path) (ref-slug name)) "zip")))))
                                                                           (the-object view tags))
                                                                   :empty "No tags. Push one: git push origin v1.0"))))
                                :active :tags))))

;;; Commits

(defun commits-page (req ent project ref path)
  (let* ((site (site)) (url (project-url project))
         (repository (project-repository project))
         (start (or (ingot:resolve-commit repository ref) (not-found "No such ref.")))
         (page (max 1 (or (ignore-errors (parse-integer (url-query-value req "page" "1"))) 1)))
         (per 30)
         (commits (ingot:log-commits repository start :limit per :skip (* per (1- page)) :path (and (plusp (length path)) path))))
    (respond-html req ent
                  (project-page project "Commits"
                                (html (str (ref-picker-html project ref "commits" path))
                                      (:h2 "Commits" (when (plusp (length path)) (htm " to " (:code (hs path)))))
                                      (str (commit-list-html site project commits))
                                      (:div :class "pager"
                                        (when (> page 1) (htm (:a :class "btn btn-small btn-quiet" :href (format nil "~a/-/commits/~a~@[/~a~]?page=~d" url (url-encode ref) (and (plusp (length path)) (path-encode path)) (1- page)) "Newer")))
                                        (when (= (length commits) per) (htm (:a :class "btn btn-small btn-quiet" :href (format nil "~a/-/commits/~a~@[/~a~]?page=~d" url (url-encode ref) (and (plusp (length path)) (path-encode path)) (1+ page)) "Older")))))
                                :active :commits))))

(defun commit-list-html (site project commits)
  (let ((url (project-url project)))
    (the-object site (table '("Commit" "Author" "When")
                            (mapcar (lambda (commit)
                                      (list (html (:span :class "title" (:a :href (format nil "~a/-/commit/~a" url (ingot:commit-oid commit)) (hs (ingot:commit-subject commit))))
                                                  " " (:code :class "sha" (hs (subseq (ingot:commit-oid commit) 0 8)))
                                                  (let ((pipeline (latest-pipeline project :sha (ingot:commit-oid commit))))
                                                    (when pipeline (htm " " (str (the-object site (status-badge (getf pipeline :status) :href (pipeline-url project pipeline))))))))
                                            (hs (ingot:commit-author-name commit))
                                            (the-object site (time-html (git-time-universal (ingot:commit-committer commit))))))
                                    commits)
                            :class "commit-list" :empty "No commits."))))

(defun commit-page (req ent project sha)
  (let* ((site (site)) (url (project-url project))
         (repository (project-repository project))
         (oid (or (ingot:resolve-commit repository sha) (not-found "No such commit.")))
         (commit (ingot:read-commit repository oid))
         (changes (ingot::commit-diff repository oid))
         (pipeline (latest-pipeline project :sha oid)))
    (respond-html req ent
                  (project-page project (ingot:commit-subject commit)
                                (html (:div :class "card"
                                        (:h2 (hs (ingot:commit-subject commit)))
                                        (:pre :style "white-space:pre-wrap" (hs (string-trim '(#\Newline) (subseq (ingot:commit-message commit) (min (length (ingot:commit-message commit)) (length (ingot:commit-subject commit)))))))
                                        (:dl :class "kv"
                                          (:dt "Commit") (:dd (:code (hs oid)) " · " (:a :href (format nil "~a/-/tree/~a" url oid) "browse the tree"))
                                          (:dt "Author") (:dd (hs (ingot:commit-author-name commit)) " <" (hs (ingot:commit-author-email commit)) "> "
                                                              (str (the-object site (time-html (git-time-universal (ingot:commit-author commit))))))
                                          (when (ingot:commit-parents commit)
                                            (htm (:dt "Parents") (:dd (dolist (p (ingot:commit-parents commit)) (htm (:a :href (format nil "~a/-/commit/~a" url p) (:code (hs (subseq p 0 8)))) " ")))))
                                          (when pipeline (htm (:dt "Pipeline") (:dd (str (the-object site (status-badge (getf pipeline :status) :href (pipeline-url project pipeline)))))))))
                                      (:h3 (hs (plural (length changes) "file")) " changed")
                                      (str (changes-html repository changes :url url :ref oid)))
                                :active :commits :wide? t))))

(defun changes-html (repository changes &key url ref (limit 60))
  "The diffs of CHANGES, as rendered unified diffs."
  (html
    (:ul :class "quiet"
      (dolist (change changes)
        (htm (:li (:a :href (format nil "#diff-~a" (ingot::change-path change)) (hs (ingot::change-path change)))
                  " " (:span :class "badge" (hs (string-downcase (symbol-name (ingot::change-status change)))))))))
    (loop for change in changes
          for n from 0
          do (if (>= n limit)
                 (progn (htm (:p :class "quiet" "... and " (hs (plural (- (length changes) limit) "more file")) ".")) (return))
                 (let* ((old (and (ingot::change-old-oid change) (ignore-errors (ingot:read-blob repository (ingot::change-old-oid change)))))
                        (new (and (ingot::change-new-oid change) (ignore-errors (ingot:read-blob repository (ingot::change-new-oid change)))))
                        (binary? (or (and old (ingot:binary-octets? old)) (and new (ingot:binary-octets? new))))
                        (big? (or (> (length (or old #())) (* 512 1024)) (> (length (or new #())) (* 512 1024)))))
                   (htm (:div :class "diff" :id (format nil "diff-~a" (ingot::change-path change))
                          (:div :class "diff-head"
                            (:span (hs (ingot::change-path change)))
                            (:span :class "quiet" (hs (string-downcase (symbol-name (ingot::change-status change)))))
                            (when (and url ref (ingot::change-new-oid change))
                              (htm (:a :href (format nil "~a/-/blob/~a/~a" url (url-encode ref) (path-encode (ingot::change-path change))) "view"))))
                          (cond (binary? (htm (:pre "Binary file")))
                                (big? (htm (:pre "A large file; the diff is not shown.")))
                                (t (str (diff-pre-html (ingot:unified-diff (if old (ingot:octets-to-string old) "") (if new (ingot:octets-to-string new) "")
                                                                           :old-name (ingot::change-path change) :new-name (ingot::change-path change)))))))))))))

(defun diff-pre-html (text)
  (html (:pre
          (dolist (line (ingot::split-lines text))
            (cond ((or (starts-with? "+++" line) (starts-with? "---" line)))
                  ((starts-with? "@@" line) (htm (:span :class "hunk" (hs line))))
                  ((starts-with? "+" line) (htm (:span :class "add" (hs line))))
                  ((starts-with? "-" line) (htm (:span :class "del" (hs line))))
                  (t (htm (:span (hs line)))))
            (str (string #\Newline))))))

(defun compare-page (req ent project)
  (let* ((site (site)) (url (project-url project))
         (repository (project-repository project))
         (from-ref (or (url-query-value req "from") (getf project :default-branch)))
         (to-ref (or (url-query-value req "to") (getf project :default-branch)))
         (from (ingot:resolve-commit repository from-ref)) (to (ingot:resolve-commit repository to-ref)))
    (unless (and from to) (not-found "No such ref."))
    (let* ((base (ingot:merge-base repository from to))
           (changes (ingot:tree-diff repository (ingot:commit-tree (ingot:read-commit repository (or base from)))
                                     (ingot:commit-tree (ingot:read-commit repository to))))
           (commits (remove-if (lambda (c) (ingot:ancestor? repository (ingot:commit-oid c) from)) (ingot:log-commits repository to :limit 100))))
      (respond-html req ent
                    (project-page project "Compare"
                                  (html (:form :class "ref-pick" :method "get" :action (format nil "~a/-/compare" url)
                                          (:span "Compare") (:input :type "text" :name "from" :value from-ref) (:span "with") (:input :type "text" :name "to" :value to-ref)
                                          (str (the-object site (button "Compare" :class "btn btn-small"))))
                                        (when (and (can? :create-merge-request project) commits)
                                          (htm (:p (str (mutating-link (format nil "~a/-/merge_requests/new?source=~a&target=~a" url (url-encode to-ref) (url-encode from-ref)) "Open a merge request"
                                                                       :class "btn btn-small")))))
                                        (:h3 (hs (plural (length commits) "commit")))
                                        (str (commit-list-html site project commits))
                                        (:h3 (hs (plural (length changes) "file")) " changed")
                                        (str (changes-html repository changes :url url :ref to)))
                                  :active :commits :wide? t)))))
