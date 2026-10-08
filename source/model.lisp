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
  t)

(defun rename-project! (project new-path)
  (unless (valid-path-name? new-path) (bad-request "~s is not a project path." new-path))
  (let* ((full (format nil "~a/~a" (project-namespace-path project) new-path)))
    (when (find-project full) (bad-request "The project ~a exists." full))
    (let ((old-dir (project-repository-path (getf project :path-with-namespace)))
          (new-dir (project-repository-path full)))
      (ensure-directories-exist new-dir)
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
