;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :anvil)

;;
;; A project's settings: general, protected branches, members, CI
;; variables, webhooks, mirrors, and the dangerous things.
;;

(defun settings-page (req ent project &key notice)
  (require-can :maintain project)
  (let* ((site (site)) (url (project-url project)) (view (project-view project))
         (base (format nil "~a/-/settings" url)))
    (respond-html req ent
                  (project-page project "Settings"
                                (html (:div :class "grid"
                                        (:div
                                          (:form :method "post" :action base :class "card"
                                            (:h2 :class "card-title" "General")
                                            (str (the-object site (field "name" "Name" :value (getf project :name))))
                                            (str (the-object site (field "description" "Description" :rows 2 :value (getf project :description))))
                                            (str (the-object site (field "visibility" "Visibility" :options '(("public" . "Public: anyone may read") ("internal" . "Internal: anyone signed in") ("private" . "Private: members only"))
                                                                         :value (string-downcase (symbol-name (getf project :visibility))))))
                                            (str (the-object site (field "default_branch" "Default branch" :options (or (mapcar (lambda (b) (cons (first b) (first b))) (the-object view branches))
                                                                                                                         (list (cons (getf project :default-branch) (getf project :default-branch))))
                                                                         :value (getf project :default-branch))))
                                            (str (the-object site (field "protected" "Protected branches (one per line)" :rows 2 :value (format nil "~{~a~%~}" (getf project :protected-branches))
                                                                         :hint "Maintainers alone push to these, and only fast-forwards; merge requests merge into them.")))
                                            (:div :class "field"
                                              (:label (:input :type "checkbox" :name "issues" :value "1" :checked (and (getf project :issues-enabled? t) "checked")) " Issues")
                                              (:label (:input :type "checkbox" :name "merge_requests" :value "1" :checked (and (getf project :merge-requests-enabled? t) "checked")) " Merge requests")
                                              (:label (:input :type "checkbox" :name "ci" :value "1" :checked (and (getf project :ci-enabled? t) "checked")) " Pipelines"))
                                            (str (the-object site (button "Save"))))
                                          (str (members-card site (format nil "~a/members" base) project))
                                          (:form :method "post" :action (format nil "~a/hooks" base) :class "card"
                                            (:h2 :class "card-title" "Webhooks")
                                            (str (the-object site (table '("URL" "Events" "Last" "")
                                                                         (mapcar (lambda (h)
                                                                                   (list (html (:code (hs (getf h :url))))
                                                                                         (hs (format nil "~{~a~^, ~}" (getf h :events)))
                                                                                         (if (getf h :last-at) (html (hs (or (and (getf h :last-status) (princ-to-string (getf h :last-status))) (getf h :last-note) "?")) " " (str (the-object site (time-html (getf h :last-at))))) "never")
                                                                                         (html (:button :type "submit" :class "btn btn-small btn-quiet" :name "remove" :value (getf h :id) "Remove"))))
                                                                                 (getf project :hooks))
                                                                         :empty "No webhooks.")))
                                            (:div :class "actions"
                                              (:input :type "url" :name "url" :placeholder "https://example.org/hook")
                                              (:input :type "text" :name "token" :placeholder "secret token (optional)")
                                              (dolist (event '("push" "tag_push" "issue" "merge_request" "pipeline"))
                                                (htm (:label (:input :type "checkbox" :name "events" :value event :checked (and (string= event "push") "checked")) " " (hs event))))
                                              (str (the-object site (button "Add" :class "btn btn-small")))))
                                          (:form :method "post" :action (format nil "~a/mirrors" base) :class "card"
                                            (:h2 :class "card-title" "Push mirrors")
                                            (:p :class "hint" "After every push, every branch and tag is pushed on to these over HTTPS with the user and token given (a GitHub fine-grained token with contents: write, for instance).")
                                            (str (the-object site (table '("Address" "User" "Last" "")
                                                                         (mapcar (lambda (m)
                                                                                   (list (html (:code (hs (getf m :url)))) (hs (or (getf m :user) ""))
                                                                                         (if (getf m :last-at) (html (hs (or (getf m :last-note) "")) " " (str (the-object site (time-html (getf m :last-at))))) "never")
                                                                                         (html (:button :type "submit" :class "btn btn-small btn-quiet" :name "remove" :value (getf m :id) "Remove")
                                                                                               " " (:button :type "submit" :class "btn btn-small btn-quiet" :name "push" :value (getf m :id) "Push now"))))
                                                                                 (getf project :mirrors))
                                                                         :empty "No mirrors.")))
                                            (:div :class "actions"
                                              (:input :type "url" :name "url" :placeholder "https://github.com/org/project.git")
                                              (:input :type "text" :name "user" :placeholder "user")
                                              (:input :type "password" :name "token" :placeholder "token")
                                              (str (the-object site (button "Add" :class "btn btn-small"))))))
                                        (:div
                                          (str (variables-card site (format nil "~a/variables" base) project))
                                          (when *deploy-houses*
                                            (str (the-object site (card (html (:p "The Deploy button and the deploy-on-green setting live on the " (:a :href (format nil "~a/-/deployments" url) "Deployments") " page."))
                                                                        :title "Deployment"))))
                                          (:div :class "card"
                                            (:h2 :class "card-title" "Danger")
                                            (:form :method "post" :action (format nil "~a/rename" base) :class "actions"
                                              (:input :type "text" :name "path" :value (getf project :path) :pattern "[A-Za-z0-9][A-Za-z0-9._-]*")
                                              (str (the-object site (button "Rename path" :class "btn btn-small btn-quiet" :confirm "Rename the project? Clone addresses change."))))
                                            (when (can? :admin project)
                                              (htm (:form :method "post" :action (format nil "~a/delete" base) :class "actions"
                                                     (:input :type "text" :name "confirm" :placeholder (getf project :path-with-namespace) :required t)
                                                     (str (the-object site (button "Delete project" :class "btn btn-small btn-danger" :confirm "Delete the project and its repository for good?"))))))))))
                                :active :code :notice (or notice (flash-notice req))))))

(defun settings-post (req ent project)
  (require-can :maintain project)
  (let ((base (format nil "~a/-/settings" (project-url project)))
        (default (param req "default_branch")))
    (when (and default (not (string= default (getf project :default-branch))))
      (ingot:set-default-branch! (project-repository project) default))
    (update-record! project
                    :name (if (blank? (param req "name")) (getf project :path) (trim (param req "name")))
                    :description (or (param req "description") "")
                    :visibility (let ((v (param req "visibility"))) (if (member v '("public" "internal" "private") :test #'string=) (intern (string-upcase v) :keyword) (getf project :visibility)))
                    :default-branch (or default (getf project :default-branch))
                    :protected-branches (remove-duplicates (remove "" (mapcar #'trim (split-on (or (param req "protected") "") #\Newline)) :test #'string=) :test #'string=)
                    :issues-enabled? (boolean-param req "issues")
                    :merge-requests-enabled? (boolean-param req "merge_requests")
                    :ci-enabled? (boolean-param req "ci"))
    (project-changed! project)
    (redirect req ent (with-notice base "Saved."))))

(defun settings-hooks-post (req ent project)
  (require-can :maintain project)
  (let ((base (format nil "~a/-/settings" (project-url project)))
        (remove (param req "remove")))
    (cond (remove (remove-hook! project (parse-integer remove)) (redirect req ent (with-notice base "Removed.")))
          (t (let ((url (param req "url")))
               (unless (and url (starts-with? "http" url)) (bad-request "A hook needs an http(s) address."))
               (add-hook! project url :token (let ((tk (param req "token"))) (and (not (blank? tk)) tk))
                                      :events (or (query-values req "events") '("push")))
               (redirect req ent (with-notice base "Hook added.")))))))

(defun settings-mirrors-post (req ent project)
  (require-can :maintain project)
  (let ((base (format nil "~a/-/settings" (project-url project)))
        (remove (param req "remove")) (push (param req "push")))
    (cond (remove (update-record! project :mirrors (remove (parse-integer remove) (getf project :mirrors) :key (lambda (m) (getf m :id))))
                  (redirect req ent (with-notice base "Removed.")))
          (push (enqueue-hook! (let ((project (find-project (getf project :id)))) (lambda () (push-mirrors! project))))
                (redirect req ent (with-notice base "Pushing in the background; the Last column will say.")))
          (t (let ((url (param req "url")))
               (unless (and url (starts-with? "https://" url)) (bad-request "A mirror is an https address."))
               (update-record! project :mirrors (append (getf project :mirrors)
                                                        (list (list :id (next-id :mirror) :url url :user (param req "user") :token (param req "token")
                                                                    :created (get-universal-time)))))
               (redirect req ent (with-notice base "Mirror added; it is pushed after the next push, or with Push now.")))))))

(defun settings-rename-post (req ent project)
  (require-can :maintain project)
  (let ((renamed (rename-project! project (trim (param req "path")))))
    (forget-project-view! project)
    (redirect req ent (with-notice (format nil "~a/-/settings" (project-url renamed)) "Renamed."))))

(defun settings-delete-post (req ent project)
  (require-can :admin project)
  (unless (equal (param req "confirm") (getf project :path-with-namespace))
    (redirect req ent (with-notice (format nil "~a/-/settings" (project-url project)) "Type the project's full path to confirm."))
    (return-from settings-delete-post nil))
  (forget-project-view! project)
  (delete-project! project)
  (redirect req ent (with-notice "/dashboard" "Deleted.")))
