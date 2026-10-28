;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :anvil)

;;
;; The model: users, groups, projects, members, issues, merge requests,
;; notes, labels.  Records as the store keeps them (plists), with the
;; functions that make and find them.  Permissions are the next file.
;;

;;; Names

(defun valid-path-name? (name)
  "A namespace or project path: letters, digits, dot, hyphen and
underscore, starting with a letter or digit, not ending in .git."
  (and (stringp name) (<= 1 (length name) 64)
       (alphanumericp (char name 0))
       (every (lambda (c) (or (alphanumericp c) (find c "._-"))) name)
       (not (ends-with? ".git" name))
       (not (member (string-downcase name) '("-" "api" "oauth" "users" "admin" "static" "explore" "help" "groups" "projects" "dashboard" "search" "sign_in" "sign_out" "sign_up") :test #'string=))))

(defun valid-email? (text)
  (and (stringp text) (< 2 (length text) 200) (position #\@ text) (position #\. text :start (position #\@ text))))

;;; Users

(define-index :user :username (lambda (user) (string-downcase (getf user :username))))
(define-index :user :email (lambda (user) (let ((email (getf user :email))) (and email (string-downcase email)))))

(defun make-user! (username name email password &key admin?)
  "A new user; answers the record.  Signals when the name or the email
is taken or malformed."
  (unless (valid-path-name? username) (bad-request "~s is not a user name: letters, digits, dot, hyphen, underscore." username))
  (unless (valid-email? email) (bad-request "~s is not an email address." email))
  (when (find-user username) (bad-request "The name ~a is taken." username))
  (when (lookup :user :email (string-downcase email)) (bad-request "The address ~a is taken." email))
  (when (< (length password) 8) (bad-request "A password of 8 characters at least."))
  (save-record! (new-record :user :username username :name (if (blank? name) username name)
                                  :email email :password (hash-password password)
                                  :admin? (or admin? (and (member email *admin-emails* :test #'string-equal) t)
                                              (zerop (count-records :user)))
                                  :state :active :ssh-keys nil :bio "")))

(defun find-user (name-or-email)
  (and (stringp name-or-email)
       (or (lookup :user :username (string-downcase name-or-email))
           (lookup :user :email (string-downcase name-or-email)))))

(defun user-display (user) (or (getf user :name) (getf user :username)))

(defun user-url (user) (format nil "/~a" (getf user :username)))

(defun user-avatar-letter (user)
  (string-upcase (subseq (user-display user) 0 1)))

;;; Groups

(define-index :group :path (lambda (group) (string-downcase (getf group :full-path))))

(defun make-group! (path name &key description (visibility *default-visibility*) owner parent)
  "A new group (a namespace) at PATH (under PARENT, a group record, for
a subgroup); OWNER, a user, becomes its Owner.  Answers the record."
  (unless (valid-path-name? path) (bad-request "~s is not a group path." path))
  (let ((full-path (if parent (format nil "~a/~a" (getf parent :full-path) path) path)))
    (when (or (find-namespace full-path)) (bad-request "The path ~a is taken." full-path))
    (save-record! (new-record :group :path path :full-path full-path :name (if (blank? name) path name)
                                     :description (or description "") :visibility visibility
                                     :parent-id (and parent (getf parent :id))
                                     :members (if owner (list (list :user-id (getf owner :id) :access 50)) nil)))))

(defun find-group (full-path)
  (and (stringp full-path) (lookup :group :path (string-downcase full-path))))

(defun find-namespace (path)
  "Two values: the group or user record at PATH, and :group or :user."
  (let ((group (find-group path)))
    (if group
        (values group :group)
        (let ((user (and (not (position #\/ path)) (lookup :user :username (string-downcase path)))))
          (if user (values user :user) (values nil nil))))))

(defun namespace-path (namespace kind)
  (ecase kind (:group (getf namespace :full-path)) (:user (getf namespace :username))))

(defun namespace-name (namespace kind)
  (ecase kind (:group (getf namespace :name)) (:user (user-display namespace))))

(defun group-url (group) (format nil "/~a" (getf group :full-path)))

(defun group-projects (group)
  (records :project (lambda (p) (and (eq (getf p :namespace-kind) :group)
                                     (eql (getf p :namespace-id) (getf group :id))))))

(defun user-projects (user)
  "The projects in USER's own namespace."
  (records :project (lambda (p) (and (eq (getf p :namespace-kind) :user)
                                     (eql (getf p :namespace-id) (getf user :id))))))

(defun subgroups (group)
  (records :group (lambda (g) (eql (getf g :parent-id) (getf group :id)))))

;;; Projects

(define-index :project :path (lambda (project) (string-downcase (getf project :path-with-namespace))))

(defun repositories-directory () (data-path "repositories/"))

(defun project-repository-path (path-with-namespace)
  (merge-pathnames (format nil "~a.git/" path-with-namespace) (repositories-directory)))

(defun make-project! (namespace kind path &key name description (visibility *default-visibility*)
                                               (default-branch *default-branch-name*) creator
                                               initialize-with-readme?)
  "A new project PATH in NAMESPACE (a group or user record, KIND :group
or :user) with an empty bare repository.  CREATOR becomes a Maintainer
of a project in a group (an Owner of one in their own namespace is the
namespace itself).  Answers the record."
  (unless (valid-path-name? path) (bad-request "~s is not a project path." path))
  (let ((full (format nil "~a/~a" (namespace-path namespace kind) path)))
    (when (find-project full) (bad-request "The project ~a exists." full))
    (let* ((repository (ingot:init-repository (project-repository-path full) :default-branch default-branch
                                                                             :description (or name path)))
           (project (save-record! (new-record :project
                                              :path path :name (if (blank? name) path name)
                                              :path-with-namespace full
                                              :namespace-id (getf namespace :id) :namespace-kind kind
                                              :description (or description "")
                                              :visibility visibility
                                              :default-branch default-branch
                                              :creator-id (and creator (getf creator :id))
                                              :members (if (and creator (eq kind :group))
                                                           (list (list :user-id (getf creator :id) :access 40))
                                                           nil)
                                              :protected-branches (list default-branch)
                                              :variables nil :hooks nil :mirrors nil
                                              :deploy nil
                                              :issues-enabled? t :merge-requests-enabled? t :ci-enabled? t
                                              :last-activity (get-universal-time)
                                              :star-count 0 :archived? nil))))
      (when initialize-with-readme?
        (ingot:make-commit! repository :branch default-branch
                                      :author (project-committer creator)
                                      :message "Initial commit"
                                      :actions (list (list :action :create :path "README.md"
                                                           :content (format nil "# ~a~%~%~a~%" (getf project :name)
                                                                            (getf project :description))))))
      project)))

(defun project-committer (user)
  (ingot:make-identity (if user (user-display user) *site-name*)
                      (if user (getf user :email) "noreply@anvil.invalid")))

(defun find-project (path-or-id)
  "The project at PATH (group/project, with or without .git) or with the
integer id (or its decimal string), or nil."
  (cond ((integerp path-or-id) (record :project path-or-id))
        ((not (stringp path-or-id)) nil)
        ((every #'digit-char-p path-or-id) (and (plusp (length path-or-id)) (record :project (parse-integer path-or-id))))
        (t (let ((path (string-right-trim "/" path-or-id)))
             (when (ends-with? ".git" path) (setq path (subseq path 0 (- (length path) 4))))
             (lookup :project :path (string-downcase path))))))

(defun project-repository (project)
  (ingot:open-repository (project-repository-path (getf project :path-with-namespace))))

(defun project-url (project) (format nil "/~a" (getf project :path-with-namespace)))

(defun project-web-url (project) (strcat *site-url* (project-url project)))

(defun project-clone-url (project) (format nil "~a/~a.git" *site-url* (getf project :path-with-namespace)))

(defun project-namespace (project)
  "Two values: the namespace record and its kind."
  (values (record (getf project :namespace-kind) (getf project :namespace-id)) (getf project :namespace-kind)))

(defun project-namespace-path (project)
  (let ((full (getf project :path-with-namespace)))
    (subseq full 0 (position #\/ full :from-end t))))

(defun all-projects () (records :project))

(defun touch-project! (project)
  (update-record! project :last-activity (get-universal-time)))

(defvar *project-deleted-hooks* nil "Functions of a project record, called after its deletion.")

(defun delete-project! (project)
  "Remove the project, its issues, merge requests, pipelines, and its
repository."
  (dolist (kind '(:issue :merge-request :pipeline :job :deployment))
    (dolist (r (records kind (lambda (r) (eql (getf r :project-id) (getf project :id)))))
      (delete-record! r)))
  (delete-record! project)
  (let ((dir (project-repository-path (getf project :path-with-namespace))))
    (when (uiop:directory-exists-p dir)
      (uiop:delete-directory-tree dir :validate t)))
  (dolist (hook *project-deleted-hooks*) (funcall hook project))
  t)

(defun rename-project! (project new-path)
  (unless (valid-path-name? new-path) (bad-request "~s is not a project path." new-path))
  (let* ((full (format nil "~a/~a" (project-namespace-path project) new-path)))
    (when (find-project full) (bad-request "The project ~a exists." full))
    (let ((old-dir (project-repository-path (getf project :path-with-namespace)))
          (new-dir (project-repository-path full)))
      (ingot:ensure-directory new-dir)
      (rename-file old-dir new-dir))
    (update-record! project :path new-path :path-with-namespace full)))

;;; Members

(defparameter *access-levels*
  '((10 . "Guest") (20 . "Reporter") (30 . "Developer") (40 . "Maintainer") (50 . "Owner")))

(defun access-name (level)
  (or (cdr (assoc level *access-levels*)) (princ-to-string level)))

(defun member-access (members user-id)
  (getf (find user-id members :key (lambda (m) (getf m :user-id))) :access))

(defun set-member! (record user access)
  "Give USER ACCESS (an integer level, or nil to remove) on RECORD, a
project or a group; answers the updated record."
  (let* ((members (remove (getf user :id) (getf record :members) :key (lambda (m) (getf m :user-id))))
         (members (if access (cons (list :user-id (getf user :id) :access access :since (get-universal-time)) members) members)))
    (update-record! record :members members)))

(defun members-with-users (record)
  "A list of (user . access) for RECORD's members, by access then name."
  (sort (loop for m in (getf record :members)
              for user = (record :user (getf m :user-id))
              when user collect (cons user (getf m :access)))
        (lambda (a b) (or (> (cdr a) (cdr b))
                          (and (= (cdr a) (cdr b)) (string< (getf (car a) :username) (getf (car b) :username)))))))

;;; Issues and merge requests share their shape: a title, a description,
;;; an author, a state, labels, notes, a per-project iid.

(defun project-counter (project what)
  (next-id (format nil "~(~a~)-~d" what (getf project :id))))

(defun make-issue! (project author title &key description labels assignee-id milestone)
  (when (blank? title) (bad-request "An issue needs a title."))
  (let ((issue (save-record! (new-record :issue :project-id (getf project :id)
                                                :iid (project-counter project :issue)
                                                :title (trim title) :description (or description "")
                                                :author-id (getf author :id) :assignee-id assignee-id
                                                :labels (or labels nil) :milestone milestone
                                                :state :opened :notes nil :closed-at nil))))
    (touch-project! project)
    issue))

(defun project-issues (project &key (state :opened) labels)
  (records :issue (lambda (issue)
                    (and (eql (getf issue :project-id) (getf project :id))
                         (or (eq state :all) (eq (getf issue :state) state))
                         (or (null labels) (subsetp labels (getf issue :labels) :test #'string-equal))))))

(defun find-issue (project iid)
  (find-record :issue (lambda (issue) (and (eql (getf issue :project-id) (getf project :id))
                                            (eql (getf issue :iid) iid)))))

(defun make-merge-request! (project author title source-branch target-branch &key description labels assignee-id
                                                                                  (remove-source-branch? nil))
  (when (blank? title) (bad-request "A merge request needs a title."))
  (let ((repository (project-repository project)))
    (unless (ingot:resolve repository source-branch) (bad-request "No branch ~a." source-branch))
    (unless (ingot:resolve repository target-branch) (bad-request "No branch ~a." target-branch))
    (when (string= source-branch target-branch) (bad-request "The two branches are one."))
    (let ((mr (save-record! (new-record :merge-request :project-id (getf project :id)
                                                       :iid (project-counter project :merge-request)
                                                       :title (trim title) :description (or description "")
                                                       :author-id (getf author :id) :assignee-id assignee-id
                                                       :labels (or labels nil)
                                                       :source-branch source-branch :target-branch target-branch
                                                       :remove-source-branch? remove-source-branch?
                                                       :state :opened :notes nil
                                                       :merge-commit nil :merged-by-id nil :merged-at nil :closed-at nil))))
      (touch-project! project)
      mr)))

(defun project-merge-requests (project &key (state :opened) source-branch)
  (records :merge-request
           (lambda (mr) (and (eql (getf mr :project-id) (getf project :id))
                             (or (eq state :all) (eq (getf mr :state) state))
                             (or (null source-branch) (string= (getf mr :source-branch) source-branch))))))

(defun find-merge-request (project iid)
  (find-record :merge-request (lambda (mr) (and (eql (getf mr :project-id) (getf project :id))
                                                 (eql (getf mr :iid) iid)))))

(defun add-note! (thing author body &key system?)
  "Append a note (a comment) to THING, an issue or merge request;
answers the updated record."
  (when (and (not system?) (blank? body)) (bad-request "An empty comment."))
  (let ((note (list :id (next-id :note) :author-id (and author (getf author :id)) :body (trim body)
                    :created (get-universal-time) :system? system?)))
    (update-record! thing :notes (append (getf thing :notes) (list note)))))

(defun close-thing! (thing user)
  (add-note! (update-record! thing :state :closed :closed-at (get-universal-time)) user "closed" :system? t))

(defun reopen-thing! (thing user)
  (add-note! (update-record! thing :state :opened :closed-at nil) user "reopened" :system? t))

(defun thing-url (project thing)
  (format nil "~a/-/~a/~d" (project-url project)
          (if (eq (getf thing :kind) :issue) "issues" "merge_requests") (getf thing :iid)))

(defun project-labels (project)
  "Every label used in the project's issues and merge requests, with
the defaults."
  (let ((labels (copy-list '("bug" "enhancement" "question" "documentation"))))
    (dolist (thing (append (project-issues project :state :all) (project-merge-requests project :state :all)))
      (dolist (label (getf thing :labels)) (pushnew label labels :test #'string-equal)))
    (sort labels #'string-lessp)))

;;; CI variables, hooks

(defun project-variables (project)
  "The project's CI variables, the group's under them: a list of
(:key ... :value ... :masked? ... :protected?)."
  (let ((own (getf project :variables))
        (group (multiple-value-bind (ns kind) (project-namespace project)
                 (and (eq kind :group) (getf ns :variables)))))
    (append own (remove-if (lambda (v) (find (getf v :key) own :key (lambda (o) (getf o :key)) :test #'string=)) group))))

(defun set-variable! (record key value &key masked? protected?)
  (let ((variables (remove key (getf record :variables) :key (lambda (v) (getf v :key)) :test #'string=)))
    (update-record! record :variables (if value
                                          (cons (list :key key :value value :masked? masked? :protected? protected?) variables)
                                          variables))))

(defun add-hook! (project url &key token (events '("push")))
  (update-record! project :hooks (append (getf project :hooks)
                                         (list (list :id (next-id :hook) :url url :token token :events events
                                                     :created (get-universal-time) :last-status nil)))))

(defun remove-hook! (project id)
  (update-record! project :hooks (remove id (getf project :hooks) :key (lambda (h) (getf h :id)))))

;;; Stars, for the explore page's order

(defun star-project! (project user)
  (let ((stars (getf project :stars)))
    (unless (member (getf user :id) stars)
      (update-record! project :stars (cons (getf user :id) stars) :star-count (1+ (length stars))))))

(defun unstar-project! (project user)
  (let ((stars (remove (getf user :id) (getf project :stars))))
    (update-record! project :stars stars :star-count (length stars))))

;;; Releases: a tag with a name and notes, GitLab's way

(defun project-releases (project)
  "The project's releases, newest first."
  (sort (records :release (lambda (r) (eql (getf r :project-id) (getf project :id))))
        #'> :key (lambda (r) (or (getf r :released-at) (getf r :created)))))

(defun find-release (project tag-name)
  (find-record :release (lambda (r) (and (eql (getf r :project-id) (getf project :id))
                                          (string= (getf r :tag-name) tag-name)))))

(defun release-notes (project tag-oid)
  "Markdown release notes for the commit TAG-OID: the subjects of the
commits since the tag before it (every commit, when there is none)."
  (let* ((repository (project-repository project))
         (commit (ingot:read-commit repository tag-oid))
         (previous (let ((parent (first (ingot:commit-parents commit))))
                     (and parent (ingot:nearest-tag repository parent))))
         (commits (remove-if (lambda (c) (> (length (ingot:commit-parents c)) 1))
                             (ingot:commits-between repository (and previous (ingot:resolve-commit repository previous)) tag-oid))))
    (when commits
      (format nil "~:[### Changes~;### Changes since ~:*~a~]~%~%~{- ~a~%~}"
              previous
              (mapcar (lambda (c) (format nil "~a (~a)" (ingot:commit-subject c) (subseq (ingot:commit-oid c) 0 8))) commits)))))

(defun make-release! (project author tag-name &key name description ref)
  "A release of TAG-NAME, made at REF (a branch, a commit) when the tag
does not exist yet; blank notes are written from the commits since the
tag before.  Answers the record."
  (when (blank? tag-name) (bad-request "A release needs a tag."))
  (when (find-release project tag-name) (bad-request "A release of ~a exists." tag-name))
  (let* ((repository (project-repository project))
         (tag-ref (format nil "refs/tags/~a" tag-name))
         (oid (or (ingot:ref-value* repository tag-ref)
                  (let ((target (ingot:resolve-commit repository (if (blank? ref) (getf project :default-branch) ref))))
                    (unless target (bad-request "No such ref to tag: ~a." ref))
                    (unless (ingot:valid-ref-name? tag-ref) (bad-request "~s is not a tag name." tag-name))
                    (ingot:update-ref! repository tag-ref target :old nil)
                    (project-changed! project)
                    target)))
         (commit-oid (or (ingot:peel-to-commit repository oid) (bad-request "The tag ~a is not a commit." tag-name)))
         (release (save-record! (new-record :release :project-id (getf project :id) :tag-name tag-name
                                                    :name (if (blank? name) tag-name (trim name))
                                                    :description (if (blank? description) (or (release-notes project commit-oid) "") description)
                                                    :author-id (getf author :id)
                                                    :released-at (get-universal-time)))))
    (touch-project! project)
    release))

(defun delete-release! (project release)
  (delete-record! release)
  (touch-project! project))

;;; Release assets: files kept beside the release, and links to elsewhere

(defun release-asset-directory (project release)
  (data-path (format nil "releases/~d/~a/" (getf project :id) (ref-slug (getf release :tag-name)))))

(defun safe-file-name (name)
  "NAME's last path part with only letters, digits, dot, dash and
underscore, not starting with a dot; a bad request when nothing is left."
  (let* ((base (ingot:last-path-part (substitute #\/ #\\ (or name ""))))
         (clean (string-left-trim "." (remove-if-not (lambda (c) (or (alphanumericp c) (find c "._-"))) base))))
    (when (zerop (length clean)) (bad-request "~s is not a file name." name))
    clean))

(defun add-release-asset! (project release name octets &key content-type)
  "Keep OCTETS as the file NAME of RELEASE; answers the updated release."
  (let* ((name (safe-file-name name))
         (directory (release-asset-directory project release)))
    (when (find name (getf release :assets) :key (lambda (a) (getf a :name)) :test #'string=)
      (bad-request "An asset named ~a exists." name))
    (when (zerop (length octets)) (bad-request "An empty file."))
    (ingot:ensure-directory directory)
    (ingot:write-file-octets (merge-pathnames name directory) octets)
    (update-record! release :assets (append (getf release :assets)
                                            (list (list :name name :size (length octets)
                                                        :content-type (if (blank? content-type) (content-type-for name) content-type)
                                                        :downloads 0 :created (get-universal-time)))))))

(defun release-asset (release name)
  (find name (getf release :assets) :key (lambda (a) (getf a :name)) :test #'string=))

(defun release-asset-octets (project release asset)
  "The file's octets, or nil when it is gone from disk."
  (ingot::file-octets-if-exists (merge-pathnames (getf asset :name) (release-asset-directory project release))))

(defun count-asset-download! (release asset)
  (update-record! release :assets (mapcar (lambda (a) (if (string= (getf a :name) (getf asset :name))
                                                          (list* :downloads (1+ (or (getf a :downloads) 0)) a)
                                                          a))
                                          (getf release :assets))))

(defun remove-release-asset! (project release name)
  (let ((file (probe-file (merge-pathnames name (release-asset-directory project release)))))
    (when file (delete-file file)))
  (update-record! release :assets (remove name (getf release :assets) :key (lambda (a) (getf a :name)) :test #'string=)))

(defun add-release-link! (release name url &key (link-type "other"))
  (when (or (blank? name) (blank? url)) (bad-request "A link needs a name and a URL."))
  (unless (or (starts-with? "https://" url) (starts-with? "http://" url)) (bad-request "A link is http(s)."))
  (update-record! release :links (append (getf release :links)
                                         (list (list :id (next-id :release-link) :name (trim name) :url (trim url)
                                                     :link-type (if (member link-type '("other" "runbook" "image" "package") :test #'equal) link-type "other"))))))

(defun remove-release-link! (release id)
  (update-record! release :links (remove id (getf release :links) :key (lambda (l) (getf l :id)))))

;;; Milestones: a title with a due date that issues and merge requests
;;; point at by title

(defun valid-date? (text)
  (and (stringp text) (= (length text) 10) (char= (char text 4) #\-) (char= (char text 7) #\-)
       (every (lambda (i) (digit-char-p (char text i))) '(0 1 2 3 5 6 8 9))))

(defun make-milestone! (project title &key description due-date start-date)
  (when (blank? title) (bad-request "A milestone needs a title."))
  (when (find-milestone-by-title project title) (bad-request "A milestone ~a exists." (trim title)))
  (dolist (date (list due-date start-date))
    (when (and (not (blank? date)) (not (valid-date? date))) (bad-request "A date is YYYY-MM-DD, not ~s." date)))
  (let ((milestone (save-record! (new-record :milestone :project-id (getf project :id)
                                                        :iid (project-counter project :milestone)
                                                        :title (trim title) :description (or description "")
                                                        :state :active
                                                        :due-date (and (not (blank? due-date)) due-date)
                                                        :start-date (and (not (blank? start-date)) start-date)))))
    (touch-project! project)
    milestone))

(defun project-milestones (project &key (state :active))
  "The project's milestones in STATE (:active, :closed or :all), the
earliest due first, undated last."
  (sort (records :milestone (lambda (m) (and (eql (getf m :project-id) (getf project :id))
                                             (or (eq state :all) (eq (getf m :state) state)))))
        (lambda (a b) (let ((da (or (getf a :due-date) "9999")) (db (or (getf b :due-date) "9999")))
                        (or (string< da db) (and (string= da db) (< (getf a :iid) (getf b :iid))))))))

(defun find-milestone (project iid)
  (find-record :milestone (lambda (m) (and (eql (getf m :project-id) (getf project :id)) (eql (getf m :iid) iid)))))

(defun find-milestone-by-title (project title)
  (and (stringp title)
       (find-record :milestone (lambda (m) (and (eql (getf m :project-id) (getf project :id)) (string-equal (getf m :title) (trim title)))))))

(defun milestone-things (project milestone)
  "Two values: the issues and the merge requests of MILESTONE, every state."
  (flet ((of (things) (remove-if-not (lambda (thing) (equal (getf thing :milestone) (getf milestone :title))) things)))
    (values (of (project-issues project :state :all)) (of (project-merge-requests project :state :all)))))

(defun set-milestone-state! (project milestone state)
  (touch-project! project)
  (update-record! milestone :state state :updated (get-universal-time)))

(defun delete-milestone! (project milestone)
  "Delete MILESTONE; the things that pointed at it point at nothing."
  (multiple-value-bind (issues mrs) (milestone-things project milestone)
    (dolist (thing (append issues mrs)) (update-record! thing :milestone nil)))
  (delete-record! milestone)
  (touch-project! project))

;;; Merge requests: drafts, approvals, and what stands between one and
;;; its merge

(defun draft-title? (title)
  "Whether TITLE marks a draft, as GitLab reads it: Draft: or WIP: in
front, or [Draft]."
  (let ((t2 (string-downcase (trim title))))
    (or (starts-with? "draft:" t2) (starts-with? "wip:" t2) (starts-with? "[draft]" t2) (starts-with? "(draft)" t2))))

(defun merge-request-draft? (mr) (draft-title? (getf mr :title)))

(defun approvals-required (project)
  (or (getf project :approvals-before-merge) 0))

(defun merge-request-head (project mr)
  (ingot:resolve-commit (project-repository project) (getf mr :source-branch)))

(defun current-approvals (project mr)
  "The approvals given to the source branch as it is now; a push resets them."
  (let ((head (merge-request-head project mr)))
    (remove-if-not (lambda (a) (equal (getf a :sha) head)) (getf mr :approvals))))

(defun approvals-left (project mr)
  (max 0 (- (approvals-required project) (length (current-approvals project mr)))))

(defun approve-merge-request! (project mr user &key sha)
  "USER approves MR as its source stands; SHA, when given, must be that head."
  (let ((head (merge-request-head project mr)))
    (unless head (bad-request "The source branch is gone."))
    (when (and sha (not (string= sha head))) (http-error 409 "The source branch has moved to ~a." head))
    (if (find (getf user :id) (current-approvals project mr) :key (lambda (a) (getf a :user-id)))
        mr
        (add-note! (update-record! mr :approvals (append (remove (getf user :id) (getf mr :approvals) :key (lambda (a) (getf a :user-id)))
                                                         (list (list :user-id (getf user :id) :sha head :at (get-universal-time)))))
                   user "approved" :system? t))))

(defun unapprove-merge-request! (project mr user)
  (declare (ignore project))
  (if (find (getf user :id) (getf mr :approvals) :key (lambda (a) (getf a :user-id)))
      (add-note! (update-record! mr :approvals (remove (getf user :id) (getf mr :approvals) :key (lambda (a) (getf a :user-id))))
                 user "revoked their approval" :system? t)
      mr))

(defun merge-blockers (project mr)
  "Why MR may not be merged yet, as a list of sentences; nil when it may."
  (let ((blockers nil))
    (when (merge-request-draft? mr) (push "It is a draft; take Draft: off the title first." blockers))
    (let ((left (approvals-left project mr)))
      (when (plusp left) (push (format nil "~a still needed." (plural left "approval")) blockers)))
    (when (getf project :only-merge-if-pipeline-succeeds?)
      (let* ((head (merge-request-head project mr))
             (pipeline (and head (latest-pipeline project :sha head)))
             (statuses (and head (commit-statuses project head)))
             (summary (commit-statuses-summary statuses)))
        (when (and pipeline (not (eq (getf pipeline :status) :success)))
          (push (format nil "The pipeline is ~a; it has to succeed." (status-text (getf pipeline :status))) blockers))
        (when (and summary (not (string= summary "success")))
          (push (format nil "A check is ~a; all have to succeed." summary) blockers))))
    (nreverse blockers)))
