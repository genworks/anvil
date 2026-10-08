;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :anvil)

;;
;; The site's own pages: home, explore, search, sign in and up, the
;; account's settings and tokens, new project and group, user and
;; group pages, the admin area, help.
;;

(defun project-card-html (project)
  (let ((site (site)))
    (html (:div :class "project-card"
            (:h3 (:a :href (project-url project) (hs (getf project :path-with-namespace)))
                 " " (:span :class "visibility" (hs (string-downcase (symbol-name (getf project :visibility))))))
            (:p (hs (if (blank? (getf project :description)) "No description." (getf project :description))))
            (:p :class "quiet" "Updated " (str (the-object site (time-html (or (getf project :last-activity) (getf project :created))))))))))

(defun project-cards (projects &key empty)
  (if projects
      (html (:div :class "project-cards" (dolist (p projects) (str (project-card-html p)))))
      (html (:p :class "empty" (hs (or empty "No projects to show."))))))

(defun home-page (req ent)
  (if *current-user*
      (dashboard-page req ent)
      (explore-page req ent)))

(defun explore-page (req ent)
  (let* ((q (url-query-value req "q"))
         (projects (visible-projects))
         (projects (if q
                       (remove-if-not (lambda (p) (or (search (string-downcase q) (string-downcase (getf p :path-with-namespace)))
                                                      (search (string-downcase q) (string-downcase (or (getf p :description) "")))))
                                      projects)
                       projects)))
    (respond-html req ent
                  (the-object (site) (page (if q (format nil "Search: ~a" q) "Explore")
                                           (html (:h1 (if q (htm "Projects matching " (:em (hs q))) (htm "Explore projects")))
                                                 (:p :class "quiet" (hs (plural (length projects) "project")))
                                                 (str (project-cards projects :empty "Nothing matches."))))))))

(defun explore-groups-page (req ent)
  (respond-html req ent
                (the-object (site) (page "Groups"
                                         (html (:h1 "Groups")
                                               (when *current-user* (htm (:p (:a :class "btn btn-small" :href "/groups/new" "New group"))))
                                               (str (the-object (site) (table '("Group" "Projects" "Description")
                                                                              (mapcar (lambda (g) (list (html (:a :href (group-url g) (hs (getf g :full-path))))
                                                                                                        (princ-to-string (length (remove-if-not #'can-read-project? (group-projects g))))
                                                                                                        (hs (getf g :description))))
                                                                                      (visible-groups))
                                                                              :empty "No groups yet."))))))))

(defun dashboard-page (req ent)
  (let ((mine (sort (member-projects *current-user*) #'> :key (lambda (p) (or (getf p :last-activity) 0)))))
    (respond-html req ent
                  (the-object (site) (page "Your projects"
                                           (html (:h1 "Your projects")
                                                 (str (project-cards mine :empty "You are not a member of any project yet."))
                                                 (:p :class "actions" (:a :class "btn" :href "/projects/new" "New project")
                                                     (:a :class "btn btn-quiet" :href "/explore" "Explore"))))))))

;;; Sign in, sign up, sign out

(defun sign-in-page (req ent &key error)
  (let ((return-to (or (param req "return_to") "/")))
    (respond-html req ent
                  (the-object (site) (page "Sign in"
                                           (html (:div :class "narrow"
                                                   (:h1 "Sign in")
                                                   (when error (htm (:p :class "profile-bad" (hs error))))
                                                   (:form :method "post" :action "/users/sign_in"
                                                     (:input :type "hidden" :name "return_to" :value return-to)
                                                     (str (the-object (site) (field "login" "Username or email" :required t :autocomplete "username")))
                                                     (str (the-object (site) (field "password" "Password" :type "password" :required t :autocomplete "current-password")))
                                                     (str (the-object (site) (button "Sign in"))))
                                                   (when *registration-open?*
                                                     (htm (:p :class "hint" "No account? " (:a :href "/users/sign_up" "Register") "."))))))))
                                     :status (if error 401 200)))

(defun sign-in-post (req ent)
  (let* ((login (param req "login")) (password (param req "password"))
         (user (find-user login)))
    (if (and user (user-active? user) (check-password password (getf user :password)))
        (let ((session (make-session! user)))
          (redirect req ent (let ((to (param req "return_to"))) (if (and to (starts-with? "/" to)) to "/"))
                    :headers (list :set-cookie (session-cookie-header session))))
        (sign-in-page req ent :error "That is not a name and password we know."))))

(defun sign-out-post (req ent)
  (when *current-session* (delete-record! *current-session*))
  (redirect req ent "/" :headers (list :set-cookie (session-cookie-header nil :clear? t))))

(defun sign-up-page (req ent &key error values)
  (unless *registration-open?* (forbidden "Registration is closed; an administrator makes accounts."))
  (respond-html req ent
                (the-object (site) (page "Register"
                                         (html (:div :class "narrow"
                                                 (:h1 "Register")
                                                 (when error (htm (:p :class "profile-bad" (hs error))))
                                                 (:form :method "post" :action "/users/sign_up"
                                                   (str (the-object (site) (field "username" "Username" :required t :value (getf values :username)
                                                                                  :hint "Letters, digits, dot, hyphen, underscore; your projects live under it.")))
                                                   (str (the-object (site) (field "name" "Name" :value (getf values :name))))
                                                   (str (the-object (site) (field "email" "Email" :type "email" :required t :value (getf values :email))))
                                                   (str (the-object (site) (field "password" "Password" :type "password" :required t :hint "Eight characters at least.")))
                                                   (str (the-object (site) (button "Register")))))))))
                                   :status (if error 400 200))

(defun sign-up-post (req ent)
  (handler-case
      (let* ((user (make-user! (param req "username") (param req "name") (param req "email") (param req "password")))
             (session (make-session! user)))
        (redirect req ent "/" :headers (list :set-cookie (session-cookie-header session))))
    (http-error (e)
      (sign-up-page req ent :error (http-error-text e)
                            :values (list :username (param req "username") :name (param req "name") :email (param req "email"))))))

;;; The account

(defun profile-page (req ent &key notice)
  (let ((user (require-user)) (site (site)))
    (respond-html req ent
                  (the-object site (page "Settings"
                                         (html (:div :class "narrow"
                                                 (:h1 "Your settings")
                                                 (:form :method "post" :action "/-/profile" :class "card"
                                                   (str (the-object site (field "name" "Name" :value (getf user :name))))
                                                   (str (the-object site (field "email" "Email" :type "email" :value (getf user :email))))
                                                   (str (the-object site (field "bio" "About you" :rows 3 :value (getf user :bio))))
                                                   (str (the-object site (button "Save"))))
                                                 (:form :method "post" :action "/-/profile/password" :class "card"
                                                   (:h2 :class "card-title" "Password")
                                                   (str (the-object site (field "current" "Current password" :type "password" :required t)))
                                                   (str (the-object site (field "password" "New password" :type "password" :required t)))
                                                   (str (the-object site (button "Change password"))))
                                                 (:p (:a :href "/-/profile/tokens" "Personal access tokens") " · " (:a :href "/-/profile/keys" "SSH keys"))))
                                         :notice (or notice (flash-notice req)))))))

(defun profile-post (req ent)
  (let ((user (require-user)))
    (update-record! user :name (param req "name") :email (param req "email") :bio (param req "bio"))
    (redirect req ent (with-notice "/-/profile" "Saved."))))

(defun password-post (req ent)
  (let ((user (require-user)))
    (cond ((not (check-password (param req "current") (getf user :password)))
           (profile-page req ent :notice "The current password is wrong."))
          ((< (length (or (param req "password") "")) 8)
           (profile-page req ent :notice "Eight characters at least."))
          (t (update-record! user :password (hash-password (param req "password")))
             (redirect req ent (with-notice "/-/profile" "The password is changed."))))))

(defun tokens-page (req ent &key new-token)
  (let ((user (require-user)) (site (site)))
    (respond-html req ent
                  (the-object site (page "Access tokens"
                                         (html (:div :class "narrow"
                                                 (:h1 "Personal access tokens")
                                                 (:p "A token stands in for your password with git over HTTPS and with the API. It is shown once.")
                                                 (when new-token
                                                   (htm (:div :class "card"
                                                          (:p :class "profile-ok" "Your new token. Copy it now; it will not be shown again.")
                                                          (:pre (hs new-token)))))
                                                 (:form :method "post" :action "/-/profile/tokens" :class "card"
                                                   (str (the-object site (field "name" "Name" :required t :placeholder "laptop, CI, the lab")))
                                                   (:div :class "field" (:label "Scopes")
                                                     (dolist (scope '("api" "read_api" "read_user" "read_repository" "write_repository"))
                                                       (htm (:label :style "display:block;font-weight:normal"
                                                              (:input :type "checkbox" :name "scopes" :value scope :checked (and (member scope '("api") :test #'string=) "checked"))
                                                              (hs scope)))))
                                                   (str (the-object site (field "expires" "Expires (days, empty for never)" :type "number" :value "")))
                                                   (str (the-object site (button "Create token"))))
                                                 (str (the-object site (table '("Name" "Scopes" "Created" "Last used" "")
                                                                              (mapcar (lambda (token)
                                                                                        (list (hs (getf token :name))
                                                                                              (hs (format nil "~{~a~^, ~}" (getf token :scopes)))
                                                                                              (the-object site (time-html (getf token :created)))
                                                                                              (if (getf token :last-used) (hs (princ-to-string (getf token :last-used))) "never")
                                                                                              (html (:form :method "post" :action (format nil "/-/profile/tokens/~d/revoke" (getf token :id))
                                                                                                      (str (the-object site (button "Revoke" :class "btn btn-small btn-quiet")))))))
                                                                                      (user-tokens user))
                                                                              :empty "No tokens yet.")))))
                                         :notice (flash-notice req))))))

(defun tokens-post (req ent)
  (let* ((user (require-user))
         (scopes (or (query-values req "scopes") '("api")))
         (days (integer-param req "expires")))
    (multiple-value-bind (text) (make-token! user (or (param req "name") "token") scopes
                                             :expires (and days (plusp days) (+ (get-universal-time) (* days 86400))))
      (tokens-page req ent :new-token text))))

(defun token-revoke-post (req ent id)
  (let* ((user (require-user))
         (token (record :token id)))
    (when (and token (eql (getf token :user-id) (getf user :id))) (revoke-token! token))
    (redirect req ent (with-notice "/-/profile/tokens" "Revoked."))))

(defun keys-page (req ent)
  (let ((user (require-user)) (site (site)))
    (respond-html req ent
                  (the-object site (page "SSH keys"
                                         (html (:div :class "narrow"
                                                 (:h1 "SSH keys")
                                                 (:p :class "hint" "Kept for the day this forge answers over ssh; today git goes over HTTPS with a token.")
                                                 (:form :method "post" :action "/-/profile/keys" :class "card"
                                                   (str (the-object site (field "title" "Title" :required t)))
                                                   (str (the-object site (field "key" "Key" :rows 3 :required t :placeholder "ssh-ed25519 AAAA...")))
                                                   (str (the-object site (button "Add key"))))
                                                 (str (the-object site (table '("Title" "Key" "")
                                                                              (mapcar (lambda (key)
                                                                                        (list (hs (getf key :title))
                                                                                              (html (:code (hs (subseq (getf key :key) 0 (min 40 (length (getf key :key)))))) "…")
                                                                                              (html (:form :method "post" :action (format nil "/-/profile/keys/~d/delete" (getf key :id))
                                                                                                      (str (the-object site (button "Remove" :class "btn btn-small btn-quiet")))))))
                                                                                      (getf user :ssh-keys))
                                                                              :empty "No keys."))))))))))

(defun keys-post (req ent)
  (let ((user (require-user)) (key (trim (param req "key"))))
    (unless (ppcre:scan "^(ssh-ed25519|ssh-rsa|ecdsa-sha2-nistp[0-9]+|sk-ssh-ed25519@openssh.com) [A-Za-z0-9+/=]+" key)
      (bad-request "That is not a public key."))
    (update-record! user :ssh-keys (append (getf user :ssh-keys) (list (list :id (next-id :ssh-key) :title (param req "title") :key key :created (get-universal-time)))))
    (redirect req ent "/-/profile/keys")))

(defun key-delete-post (req ent id)
  (let ((user (require-user)))
    (update-record! user :ssh-keys (remove id (getf user :ssh-keys) :key (lambda (k) (getf k :id))))
    (redirect req ent "/-/profile/keys")))

;;; New project, new group

(defun namespace-choices (user)
  (append (list (cons (format nil "user:~d" (getf user :id)) (getf user :username)))
          (loop for group in (records :group)
                when (>= (or (group-access-level group user) 0) 40)
                  collect (cons (format nil "group:~d" (getf group :id)) (getf group :full-path)))))

(defun new-project-page (req ent &key error)
  (let ((user (require-user)) (site (site)))
    (respond-html req ent
                  (the-object site (page "New project"
                                         (html (:div :class "narrow"
                                                 (:h1 "New project")
                                                 (when error (htm (:p :class "profile-bad" (hs error))))
                                                 (:form :method "post" :action "/projects/new"
                                                   (str (the-object site (field "namespace" "Namespace" :options (namespace-choices user)
                                                                                :value (let ((g (url-query-value req "group"))) (if g (format nil "group:~a" g) (format nil "user:~d" (getf user :id)))))))
                                                   (str (the-object site (field "path" "Project path" :required t :value (param req "path")
                                                                                :hint "The address: letters, digits, dot, hyphen, underscore.")))
                                                   (str (the-object site (field "name" "Name" :value (param req "name"))))
                                                   (str (the-object site (field "description" "Description" :rows 2 :value (param req "description"))))
                                                   (str (the-object site (field "visibility" "Visibility" :options '(("public" . "Public: anyone may read") ("internal" . "Internal: anyone signed in") ("private" . "Private: members only"))
                                                                                :value (or (param req "visibility") (string-downcase (symbol-name *default-visibility*))))))
                                                   (str (the-object site (field "default_branch" "Default branch" :value *default-branch-name*)))
                                                   (str (the-object site (field "readme" "Start with a README" :type "checkbox")))
                                                   (str (the-object site (button "Create project"))))))))
                                         :status (if error 400 200))))

(defun parse-namespace-choice (text user)
  "Two values, the namespace record and its kind, from user:<id> or group:<id>."
  (let* ((colon (position #\: (or text "")))
         (kind (and colon (subseq text 0 colon)))
         (id (and colon (ignore-errors (parse-integer text :start (1+ colon))))))
    (cond ((equal kind "group")
           (let ((group (record :group id)))
             (unless (and group (>= (or (group-access-level group user) 0) 40)) (forbidden "Not your group."))
             (values group :group)))
          (t (values user :user)))))

(defun new-project-post (req ent)
  (let ((user (require-user)))
    (handler-case
        (multiple-value-bind (namespace kind) (parse-namespace-choice (param req "namespace") user)
          (let ((project (make-project! namespace kind (param req "path")
                                        :name (param req "name") :description (param req "description")
                                        :visibility (intern (string-upcase (or (param req "visibility") "public")) :keyword)
                                        :default-branch (if (blank? (param req "default_branch")) *default-branch-name* (trim (param req "default_branch")))
                                        :creator user :initialize-with-readme? (boolean-param req "readme"))))
            (redirect req ent (project-url project))))
      (http-error (e) (new-project-page req ent :error (http-error-text e))))))

(defun new-group-page (req ent &key error)
  (let ((site (site)))
    (require-user)
    (respond-html req ent
                  (the-object site (page "New group"
                                         (html (:div :class "narrow"
                                                 (:h1 "New group")
                                                 (when error (htm (:p :class "profile-bad" (hs error))))
                                                 (:form :method "post" :action "/groups/new"
                                                   (str (the-object site (field "path" "Group path" :required t :value (param req "path"))))
                                                   (str (the-object site (field "name" "Name" :value (param req "name"))))
                                                   (str (the-object site (field "description" "Description" :rows 2)))
                                                   (str (the-object site (field "visibility" "Visibility" :options '(("public" . "Public") ("internal" . "Internal") ("private" . "Private")) :value "public")))
                                                   (str (the-object site (button "Create group"))))))))
                                         :status (if error 400 200))))

(defun new-group-post (req ent)
  (let ((user (require-user)))
    (handler-case
        (let ((group (make-group! (param req "path") (param req "name") :description (param req "description")
                                                                         :visibility (intern (string-upcase (or (param req "visibility") "public")) :keyword)
                                                                         :owner user)))
          (redirect req ent (group-url group)))
      (http-error (e) (new-group-page req ent :error (http-error-text e))))))

;;; A user's page, a group's page

(defun user-page (req ent user)
  (let ((site (site)))
    (respond-html req ent
                  (the-object site (page (user-display user)
                                         (html (:div :class "project-head"
                                                 (str (the-object site (avatar user)))
                                                 (:div (:h1 (hs (user-display user)))
                                                       (:p :class "desc" "@" (hs (getf user :username))
                                                           (unless (blank? (getf user :bio)) (htm " · " (hs (getf user :bio)))))))
                                               (:h2 "Projects")
                                               (str (project-cards (remove-if-not #'can-read-project? (user-projects user)) :empty "No projects."))))))))

(defun group-page (req ent group)
  (unless (can-read-group? group) (not-found "No such group."))
  (let ((site (site)) (level (group-access-level group *current-user*)))
    (respond-html req ent
                  (the-object site (page (getf group :name)
                                         (html (:div :class "project-head"
                                                 (:div (:h1 (hs (getf group :name)))
                                                       (:p :class "desc" (hs (getf group :full-path))
                                                           (unless (blank? (getf group :description)) (htm " · " (hs (getf group :description))))))
                                                 (:div :class "actions"
                                                   (when (and level (>= level 40))
                                                     (htm (:a :class "btn btn-small" :href (format nil "/projects/new?group=~d" (getf group :id)) "New project")
                                                          (:a :class "btn btn-small btn-quiet" :href (format nil "~a/-/settings" (group-url group)) "Settings")))))
                                               (:h2 "Projects")
                                               (str (project-cards (remove-if-not #'can-read-project? (group-projects group)) :empty "No projects yet."))
                                               (let ((subs (remove-if-not #'can-read-group? (subgroups group))))
                                                 (when subs
                                                   (htm (:h2 "Subgroups")
                                                        (:ul (dolist (g subs) (htm (:li (:a :href (group-url g) (hs (getf g :full-path))))))))))
                                               (:h2 "Members")
                                               (str (the-object site (table '("Member" "Role")
                                                                            (mapcar (lambda (pair) (list (the-object site (user-link (car pair))) (hs (access-name (cdr pair)))))
                                                                                    (members-with-users group)))))))))))

(defun group-settings-page (req ent group &key notice)
  (unless (>= (or (group-access-level group *current-user*) 0) 40) (forbidden))
  (let ((site (site)))
    (respond-html req ent
                  (the-object site (page (format nil "~a settings" (getf group :name))
                                         (html (:h1 (hs (getf group :name)) " settings")
                                               (:div :class "grid"
                                                 (:div
                                                   (:form :method "post" :action (format nil "~a/-/settings" (group-url group)) :class "card"
                                                     (:h2 :class "card-title" "General")
                                                     (str (the-object site (field "name" "Name" :value (getf group :name))))
                                                     (str (the-object site (field "description" "Description" :rows 2 :value (getf group :description))))
                                                     (str (the-object site (field "visibility" "Visibility" :options '(("public" . "Public") ("internal" . "Internal") ("private" . "Private"))
                                                                                  :value (string-downcase (symbol-name (getf group :visibility))))))
                                                     (str (the-object site (button "Save"))))
                                                   (str (members-card site (format nil "~a/-/settings/members" (group-url group)) group)))
                                                 (:div (str (variables-card site (format nil "~a/-/settings/variables" (group-url group)) group)))))
                                         :notice (or notice (flash-notice req)))))))

(defun members-card (site action record)
  (html (:form :method "post" :action action :class "card"
          (:h2 :class "card-title" "Members")
          (str (the-object site (table '("Member" "Role" "")
                                       (mapcar (lambda (pair)
                                                 (list (the-object site (user-link (car pair)))
                                                       (hs (access-name (cdr pair)))
                                                       (html (:button :type "submit" :class "btn btn-small btn-quiet" :name "remove" :value (getf (car pair) :username) "Remove"))))
                                               (members-with-users record))
                                       :empty "No members of its own.")))
          (:div :class "actions"
            (:input :type "text" :name "username" :placeholder "username" :required t)
            (:select :name "access"
              (loop for (level . name) in *access-levels* do (htm (:option :value level :selected (and (= level 30) "selected") (hs name)))))
            (str (the-object site (button "Add" :class "btn btn-small")))))))

(defun members-post (req ent record back)
  (let ((remove (param req "remove")) (username (param req "username")))
    (cond (remove (let ((user (find-user remove))) (when user (set-member! record user nil)))
                  (redirect req ent (with-notice back "Removed.")))
          (t (let ((user (find-user username)) (access (integer-param req "access" 30)))
               (unless user (bad-request "No user ~a." username))
               (set-member! record user access)
               (redirect req ent (with-notice back (format nil "~a is a ~a." username (access-name access)))))))))

(defun variables-card (site action record)
  (html (:form :method "post" :action action :class "card"
          (:h2 :class "card-title" "CI variables")
          (str (the-object site (table '("Key" "Value" "")
                                       (mapcar (lambda (v)
                                                 (list (html (:code (hs (getf v :key))))
                                                       (if (getf v :masked?) "••••••" (html (:code (hs (getf v :value)))))
                                                       (html (:button :type "submit" :class "btn btn-small btn-quiet" :name "remove" :value (getf v :key) "Remove"))))
                                               (getf record :variables))
                                       :empty "No variables.")))
          (:div :class "actions"
            (:input :type "text" :name "key" :placeholder "KEY" :pattern "[A-Za-z_][A-Za-z0-9_]*")
            (:input :type "text" :name "value" :placeholder "value")
            (:label (:input :type "checkbox" :name "masked" :value "1") " masked")
            (str (the-object site (button "Add" :class "btn btn-small")))))))

(defun variables-post (req ent record back)
  (let ((remove (param req "remove")))
    (cond (remove (set-variable! record remove nil) (redirect req ent (with-notice back "Removed.")))
          (t (let ((key (param req "key")) (value (param req "value")))
               (unless (and key (ppcre:scan "^[A-Za-z_][A-Za-z0-9_]*$" key)) (bad-request "A variable's key is letters, digits and underscores."))
               (set-variable! record key (or value "") :masked? (boolean-param req "masked"))
               (redirect req ent (with-notice back (format nil "~a set." key))))))))

;;; Admin

(defun admin-page (req ent &key new-secret)
  (require-admin)
  (let ((site (site)))
    (respond-html req ent
                  (the-object site (page "Admin"
                                         (html (:h1 "Admin area")
                                               (:div :class "grid"
                                                 (:div
                                                   (str (the-object site (card (the-object site (table '("User" "Email" "Admin" "State" "")
                                                                                                        (mapcar (lambda (u)
                                                                                                                  (list (the-object site (user-link u)) (hs (getf u :email))
                                                                                                                        (if (getf u :admin?) "yes" "")
                                                                                                                        (hs (status-text (getf u :state)))
                                                                                                                        (html (:form :method "post" :action (format nil "/admin/users/~d" (getf u :id)) :style "display:inline"
                                                                                                                                (:button :class "btn btn-small btn-quiet" :name "toggle" :value "admin" "admin")
                                                                                                                                (:button :class "btn btn-small btn-quiet" :name "toggle" :value "block" (if (eq (getf u :state) :active) "block" "unblock"))))))
                                                                                                                (records :user))))
                                                                               :title "Users")))
                                                   (str (the-object site (card (html (:form :method "post" :action "/admin/users"
                                                                                       (str (the-object site (field "username" "Username" :required t)))
                                                                                       (str (the-object site (field "email" "Email" :type "email" :required t)))
                                                                                       (str (the-object site (field "password" "Password" :type "password" :required t)))
                                                                                       (str (the-object site (button "Create user")))))
                                                                               :title "New user"))))
                                                 (:div
                                                   (when new-secret
                                                     (htm (:div :class "card" (:p :class "profile-ok" "The application's secret, shown once:") (:pre (hs new-secret)))))
                                                   (str (the-object site (card (html (str (the-object site (table '("Name" "Client id" "Redirect URIs")
                                                                                                                  (mapcar (lambda (app) (list (hs (getf app :name)) (html (:code (hs (getf app :client-id))))
                                                                                                                                              (hs (format nil "~{~a~^ ~}" (getf app :redirect-uris)))))
                                                                                                                          (records :oauth-app))
                                                                                                                  :empty "No applications.")))
                                                                                     (:form :method "post" :action "/admin/applications"
                                                                                       (str (the-object site (field "name" "Name" :required t)))
                                                                                       (str (the-object site (field "redirect_uris" "Redirect URIs (one per line)" :rows 2 :required t)))
                                                                                       (str (the-object site (field "scopes" "Scopes" :value "api")))
                                                                                       (str (the-object site (button "Register application")))))
                                                                               :title "OAuth applications")))
                                                   (str (the-object site (card (html (:dl :class "kv"
                                                                                       (:dt "Data") (:dd (:code (hs (namestring *data-directory*))))
                                                                                       (:dt "Docker") (:dd (hs (if (docker-available?) (or (ignore-errors (docker-version)) "reachable") "not reachable")))
                                                                                       (:dt "Runner tags") (:dd (hs (format nil "~{~a~^, ~}" *runner-tags*)))
                                                                                       (:dt "Running jobs") (:dd (hs (princ-to-string (running-count))))
                                                                                       (:dt "Houses") (:dd (hs (format nil "~{~a~^, ~}" (mapcar (lambda (h) (getf h :name)) *deploy-houses*))))
                                                                                       (:dt "Lisp") (:dd (hs (format nil "~a ~a" (lisp-implementation-type) (lisp-implementation-version))))))
                                                                               :title "This forge"))))))
                                         :notice (flash-notice req))))))

(defun admin-user-post (req ent id)
  (require-admin)
  (let ((user (record :user id)))
    (unless user (not-found))
    (cond ((equal (param req "toggle") "admin") (update-record! user :admin? (not (getf user :admin?))))
          ((equal (param req "toggle") "block") (update-record! user :state (if (eq (getf user :state) :active) :blocked :active))))
    (redirect req ent (with-notice "/admin" "Done."))))

(defun admin-new-user-post (req ent)
  (require-admin)
  (handler-case (progn (make-user! (param req "username") (param req "username") (param req "email") (param req "password"))
                       (redirect req ent (with-notice "/admin" "Created.")))
    (http-error (e) (redirect req ent (with-notice "/admin" (http-error-text e))))))

(defun admin-application-post (req ent)
  (require-admin)
  (multiple-value-bind (app secret)
      (make-oauth-app! (param req "name") (split-on (ppcre:regex-replace-all "[\\s,]+" (or (param req "redirect_uris") "") " ") #\Space)
                       :scopes (split-on (or (param req "scopes") "api") #\Space) :owner *current-user*)
    (declare (ignore app))
    (admin-page req ent :new-secret secret)))

;;; Help

(defun help-page (req ent)
  (respond-html req ent
                (the-object (site) (page "Help"
                                         (html (:div :class "narrow prose"
                                                 (:h1 "Help")
                                                 (:h2 "Cloning and pushing")
                                                 (:p "Over HTTPS. A public project clones with no sign-in; pushing takes your username and password, or a " (:a :href "/-/profile/tokens" "personal access token") " in place of the password.")
                                                 (:pre (hs (format nil "git clone ~a/group/project.git~%git push origin master" *site-url*)))
                                                 (:h2 "Pipelines")
                                                 (:p "A project with a " (:code "ci.sexp") " or a " (:code ".gitlab-ci.yml") " at its root runs a pipeline on every push. Jobs run in containers from the image they name; their output is the job's trace, their artifacts are kept as declared.")
                                                 (:pre (hs "(:pipeline
 :stages (:test :deploy)
 :default (:image \"genworks/gendl:devo-ccl\")
 :jobs
 ((:test :stage :test :lisp (:load (\"ci/test.lisp\")))
  (:deploy :stage :deploy
           :rules ((:if (:and (:default-branch) (:variable \"DEPLOY\" \"on\"))))
           :deploy (:house :common-lisp.app))))"))
                                                 (:h2 "The API")
                                                 (:p "At " (:code "/api/v4") ", in GitLab's shapes: projects, repository trees and files, commits, branches, issues, merge requests, pipelines, jobs and their traces and artifacts. Send a token as " (:code "PRIVATE-TOKEN") ".")
                                                 (:h2 "Deploying")
                                                 (:p "A project that meets the hosting house's profile (an " (:code "image.sexp") " and a " (:code "monetize.sexp") " at its root) has a Deploy button for its Maintainers, and may deploy itself at the end of a green pipeline.")))))))
