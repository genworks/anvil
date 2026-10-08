;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :anvil)

;;
;; Who may do what.  GitLab's access levels: Guest 10, Reporter 20,
;; Developer 30, Maintainer 40, Owner 50; the level on a project is the
;; higher of the project's own and the group's (inherited up the
;; groups); the owner of a user namespace owns its projects; an
;; administrator has every level everywhere.  Visibility: public reads
;; for anyone, internal for anyone signed in, private for members.
;;

(defun group-access-level (group user)
  "USER's level on GROUP, inherited from its parents; nil for none."
  (and user group
       (let ((own (member-access (getf group :members) (getf user :id)))
             (parent (and (getf group :parent-id) (record :group (getf group :parent-id)))))
         (let ((inherited (and parent (group-access-level parent user))))
           (cond ((and own inherited) (max own inherited))
                 (t (or own inherited)))))))

(defun project-access-level (project &optional (user *current-user*))
  "USER's level on PROJECT (an integer), or nil."
  (cond ((null user) nil)
        ((admin? user) 50)
        (t (multiple-value-bind (namespace kind) (project-namespace project)
             (let ((own (member-access (getf project :members) (getf user :id)))
                   (through (ecase kind
                              (:user (and (eql (getf namespace :id) (getf user :id)) 50))
                              (:group (group-access-level namespace user)))))
               (cond ((and own through) (max own through))
                     (t (or own through))))))))

(defun visible? (visibility &optional (user *current-user*))
  (ecase (or visibility :private)
    (:public t)
    (:internal (and user t))
    (:private nil)))

(defun can-read-project? (project &optional (user *current-user*))
  (or (visible? (getf project :visibility) user)
      (and (project-access-level project user) t)))

(defun can-read-group? (group &optional (user *current-user*))
  (or (visible? (getf group :visibility) user)
      (and (group-access-level group user) t)))

(defun access-at-least? (project level &optional (user *current-user*))
  (let ((have (project-access-level project user)))
    (and have (>= have level))))

(defun can? (ability project &optional (user *current-user*))
  "Whether USER may do ABILITY on PROJECT."
  (ecase ability
    (:read (can-read-project? project user))
    (:read-code (can-read-project? project user))
    (:download-code (can-read-project? project user))
    (:create-issue (and (can-read-project? project user) user t))
    (:create-note (and (can-read-project? project user) user t))
    (:create-merge-request (and user (can-read-project? project user) t))
    (:update-issue (access-at-least? project 20 user))
    (:push (access-at-least? project 30 user))
    (:push-protected (access-at-least? project 40 user))
    (:merge (access-at-least? project 40 user))
    (:read-variables (access-at-least? project 40 user))
    (:run-pipeline (access-at-least? project 30 user))
    (:read-pipeline (can-read-project? project user))
    (:deploy (access-at-least? project 40 user))
    (:maintain (access-at-least? project 40 user))
    (:admin (access-at-least? project 50 user))))

(defun require-can (ability project &optional (user *current-user*))
  (unless (can? ability project user)
    (if user
        (forbidden (format nil "You may not ~(~a~) here." (substitute #\Space #\- (symbol-name ability))))
        (if (can-read-project? project nil)
            (unauthorized)
            (not-found))))
  t)

(defun require-project (path-or-id &optional (ability :read))
  "The project PATH-OR-ID, which the current user must be able to
ABILITY; 404 for one they may not even see."
  (let ((project (find-project path-or-id)))
    (unless (and project (can-read-project? project)) (not-found "No such project."))
    (require-can ability project)
    project))

(defun protected-branch? (project branch)
  (and (member branch (getf project :protected-branches) :test #'string=) t))

(defun can-push-branch? (project branch &optional (user *current-user*))
  (if (protected-branch? project branch)
      (can? :push-protected project user)
      (can? :push project user)))

(defun visible-projects (&optional (user *current-user*))
  "The projects USER may see, most recently active first."
  (sort (remove-if-not (lambda (p) (can-read-project? p user)) (all-projects))
        #'> :key (lambda (p) (or (getf p :last-activity) 0))))

(defun member-projects (user)
  "The projects USER is a member of, through a group or directly, or owns."
  (remove-if-not (lambda (p) (project-access-level p user)) (all-projects)))

(defun visible-groups (&optional (user *current-user*))
  (remove-if-not (lambda (g) (can-read-group? g user)) (records :group)))
