;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :anvil)

;;
;; Browsing the repository: a tree, a file, a raw file, an archive, and
;; the web editor (a new file, an edited file, a deleted file).
;;

(defun split-ref-and-path (project segments)
  "SEGMENTS are what follows /-/tree/ (or blob, raw, commits): the
ref, which may hold slashes, then the path.  The longest leading run
that names a branch or tag wins; else the first segment is the ref."
  (let ((repository (project-repository project))
        (decoded (mapcar #'url-decode segments)))
    (loop for n from (length decoded) downto 1
          for candidate = (format nil "~{~a~^/~}" (subseq decoded 0 n))
          when (or (ingot:ref-value* repository (format nil "refs/heads/~a" candidate))
                   (ingot:ref-value* repository (format nil "refs/tags/~a" candidate)))
            do (return (values candidate (format nil "~{~a~^/~}" (subseq decoded n))))
          finally (return (values (first decoded) (format nil "~{~a~^/~}" (rest decoded)))))))

(defun tree-page (req ent project ref path)
  (let* ((repository (project-repository project))
         (commit-oid (or (ingot:resolve-commit repository ref) (not-found "No such ref.")))
         (commit (ingot:read-commit repository commit-oid))
         (entry (or (ingot:tree-entry-at repository (ingot:commit-tree commit) path) (not-found "No such path."))))
    (cond ((ingot::tree-entry-tree? entry)
           (let ((entries (ingot:read-tree repository (ingot:tree-entry-oid entry))))
             (respond-html req ent
                           (project-page project (if (string= path "") "Files" path)
                                         (html (str (ref-picker-html project ref "tree" path))
                                               (when (can? :push project)
                                                 (htm (:p :class "actions" (:a :class "btn btn-small btn-quiet" :href (format nil "~a/-/new/~a~@[/~a~]" (project-url project) (url-encode ref) (and (plusp (length path)) (path-encode path))) "New file"))))
                                               (str (tree-table-html project ref commit-oid path entries))
                                               (let ((readme (readme-entry entries)))
                                                 (when (and readme (plusp (length path)))
                                                   (let ((data (ingot:read-blob repository (ingot:tree-entry-oid readme))))
                                                     (htm (:div :class "card" :style "margin-top:1rem"
                                                            (:h2 :class "card-title" (hs (ingot:tree-entry-name readme)))
                                                            (:div :class "prose" (str (if (markdown-file? (ingot:tree-entry-name readme))
                                                                                          (markdown-html (ingot:octets-to-string data)
                                                                                                         :base-url (format nil "~a/-/blob/~a/~a" (project-url project) (url-encode ref) (path-encode path))
                                                                                                         :raw-base-url (format nil "~a/-/raw/~a/~a" (project-url project) (url-encode ref) (path-encode path)))
                                                                                          (html (:pre (hs (ingot:octets-to-string data)))))))))))))
                                         :active :code))))
          (t (blob-page req ent project ref path)))))

(defun blob-page (req ent project ref path)
  (let* ((site (site)) (url (project-url project))
         (repository (project-repository project))
         (commit-oid (or (ingot:resolve-commit repository ref) (not-found "No such ref."))))
    (multiple-value-bind (data entry) (ingot:blob-at repository commit-oid path)
      (unless data (not-found "No such file."))
      (let* ((binary? (ingot:binary-octets? data))
             (big? (> (length data) *blob-view-limit*))
             (name (ingot::last-path-part path))
             (raw-url (format nil "~a/-/raw/~a/~a" url (url-encode ref) (path-encode path))))
        (respond-html req ent
                      (project-page project name
                                    (html (str (ref-picker-html project ref "blob" path))
                                          (:div :class "blob"
                                            (:div :class "blob-head"
                                              (:strong (hs name))
                                              (:span (hs (format nil "~:d bytes" (length data))))
                                              (:span (:code (hs (subseq (ingot:tree-entry-oid entry) 0 8))))
                                              (:div :class "actions"
                                                (:a :class "btn btn-small btn-quiet" :href raw-url "Raw")
                                                (:a :class "btn btn-small btn-quiet" :href (format nil "~a/-/commits/~a/~a" url (url-encode ref) (path-encode path)) "History")
                                                (when (and (can-push-branch? project ref) (not binary?) (not big?))
                                                  (htm (:a :class "btn btn-small btn-quiet" :href (format nil "~a/-/edit/~a/~a" url (url-encode ref) (path-encode path)) "Edit")))
                                                (when (can-push-branch? project ref)
                                                  (htm (:form :method "post" :action (format nil "~a/-/delete/~a/~a" url (url-encode ref) (path-encode path)) :style "display:inline"
                                                         (str (the-object site (button "Delete" :class "btn btn-small btn-quiet" :confirm (format nil "Delete ~a on ~a?" name ref)))))))))
                                            (cond (big? (htm (:p :class "empty" :style "padding:1rem" "A large file: " (:a :href raw-url "download it") ".")))
                                                  ((image-file? name) (htm (:p :style "padding:1rem" (:img :src raw-url :alt name :style "max-width:100%"))))
                                                  (binary? (htm (:p :class "empty" :style "padding:1rem" "A binary file: " (:a :href raw-url "download it") ".")))
                                                  ((and (markdown-file? name) (not (url-query-value req "plain")))
                                                   (htm (:div :class "prose" :style "padding:1rem"
                                                          (str (markdown-html (ingot:octets-to-string data)
                                                                              :base-url (format nil "~a/-/blob/~a~@[/~a~]" url (url-encode ref) (let ((d (directory-of path))) (and (plusp (length d)) (path-encode d))))
                                                                              :raw-base-url (format nil "~a/-/raw/~a~@[/~a~]" url (url-encode ref) (let ((d (directory-of path))) (and (plusp (length d)) (path-encode d)))))))
                                                        (:p :class "hint" :style "padding:0 1rem 1rem" (:a :href (format nil "?plain=1") "Show the source"))))
                                                  (t (str (code-html (ingot:octets-to-string data)))))))
                                    :active :code :wide? t))))))

(defun directory-of (path)
  (let ((slash (position #\/ path :from-end t))) (if slash (subseq path 0 slash) "")))

(defun image-file? (name)
  (let ((lower (string-downcase name)))
    (some (lambda (ext) (ends-with? ext lower)) '(".png" ".jpg" ".jpeg" ".gif" ".svg" ".webp" ".ico"))))

(defun code-html (text)
  "TEXT as numbered lines."
  (html (:div :class "code"
          (:table
            (:tbody
              (loop for line in (ingot::split-lines text)
                    for n from 1
                    do (htm (:tr :id (format nil "L~d" n)
                              (:td :class "ln" (:a :href (format nil "#L~d" n) :class "quiet" (hs (princ-to-string n))))
                              (:td (hs line))))))))))

(defun raw-page (req ent project ref path)
  (let* ((repository (project-repository project))
         (commit-oid (or (ingot:resolve-commit repository ref) (not-found "No such ref.")))
         (data (or (ingot:blob-at repository commit-oid path) (not-found "No such file."))))
    (respond req ent data :content-type (let ((type (content-type-for path)))
                                          ;; HTML and SVG are served as text, so a stranger's page does not run on the forge's name
                                          (if (or (starts-with? "text/html" type) (string= type "image/svg+xml") (starts-with? "text/javascript" type))
                                              "text/plain; charset=utf-8" type))
             :cache-control (if (ingot:oid? ref) "public, max-age=31536000" "no-cache")
             :headers (list :x-content-type-options "nosniff"))))

(defun archive-page (req ent project ref name)
  (let* ((repository (project-repository project))
         (sha (or (ingot:resolve-commit repository ref) (not-found "No such ref.")))
         (format-text (cond ((ends-with? ".zip" name) "zip") ((ends-with? ".tar" name) "tar") (t "tar.gz"))))
    (serve-archive req ent project repository sha ref format-text)))

;;; The web editor

(defun edit-page (req ent project ref path &key new? error content)
  (unless (can-push-branch? project ref) (forbidden "You may not push to this branch."))
  (let* ((site (site)) (url (project-url project))
         (repository (project-repository project))
         (commit-oid (ingot:resolve-commit repository ref))
         (existing (and (not new?) commit-oid (ingot:blob-at repository commit-oid path)))
         (text (or content (and existing (ingot:octets-to-string existing)) "")))
    (when (and (not new?) (null existing)) (not-found "No such file."))
    (respond-html req ent
                  (project-page project (if new? "New file" (format nil "Edit ~a" path))
                                (html (when error (htm (:p :class "profile-bad" (hs error))))
                                      (:form :method "post" :action (format nil "~a/-/~a/~a~@[/~a~]" url (if new? "new" "edit") (url-encode ref) (and (plusp (length path)) (path-encode path)))
                                        (when new?
                                          (str (the-object site (field "name" "File name" :required t :value (param req "name")
                                                                       :hint (format nil "Under ~a~a on ~a." (getf project :path) (if (plusp (length path)) (format nil "/~a" path) "") ref)))))
                                        (:div :class "field"
                                          (:label :for "content" "Content")
                                          (:textarea :id "content" :name "content" :rows 28 :spellcheck "false" :style "width:100%;font-family:var(--mono)" (hs text)))
                                        (str (the-object site (field "message" "Commit message" :required t
                                                                     :value (or (param req "message") (if new? "" (format nil "Update ~a" (ingot::last-path-part path)))))))
                                        (:p :class "hint" "Committed to " (:code (hs ref)) " as " (hs (user-display *current-user*)) ".")
                                        (str (the-object site (button (if new? "Create file" "Commit changes"))))
                                        " " (:a :class "btn btn-quiet" :href (if new? (format nil "~a/-/tree/~a~@[/~a~]" url (url-encode ref) (and (plusp (length path)) (path-encode path)))
                                                                                  (format nil "~a/-/blob/~a/~a" url (url-encode ref) (path-encode path)))
                                              "Cancel")))
                                :active :code :wide? t))))

(defun edit-post (req ent project ref path &key new?)
  (require-user)
  (unless (can-push-branch? project ref) (forbidden "You may not push to this branch."))
  (let* ((repository (project-repository project))
         (content (ppcre:regex-replace-all (string #\Return) (or (param req "content") "") ""))
         (message (param req "message"))
         (file-path (if new?
                        (let ((name (trim (param req "name"))))
                          (when (or (blank? name) (search ".." name) (starts-with? "/" name)) (bad-request "A file name."))
                          (if (plusp (length path)) (format nil "~a/~a" path name) name))
                        path)))
    (when (blank? message) (return-from edit-post (edit-page req ent project ref path :new? new? :error "A commit message." :content content)))
    (handler-case
        (let ((oid (ingot:make-commit! repository :branch ref :message message :author (project-committer *current-user*)
                                                  :actions (list (list :action (if new? :create :update) :path file-path :content content)))))
          (after-push! project *current-user* (list (ingot::make-ref-command :old "" :new oid :name (format nil "refs/heads/~a" ref) :status :ok)))
          (redirect req ent (with-notice (format nil "~a/-/blob/~a/~a" (project-url project) (url-encode ref) (path-encode file-path)) "Committed.")))
      (ingot:git-error (e) (edit-page req ent project ref path :new? new? :error (princ-to-string e) :content content)))))

(defun delete-post (req ent project ref path)
  (require-user)
  (unless (can-push-branch? project ref) (forbidden "You may not push to this branch."))
  (let ((repository (project-repository project)))
    (handler-case
        (let ((oid (ingot:make-commit! repository :branch ref :message (format nil "Delete ~a" path) :author (project-committer *current-user*)
                                                  :actions (list (list :action :delete :path path)))))
          (after-push! project *current-user* (list (ingot::make-ref-command :old "" :new oid :name (format nil "refs/heads/~a" ref) :status :ok)))
          (redirect req ent (with-notice (format nil "~a/-/tree/~a~@[/~a~]" (project-url project) (url-encode ref) (let ((d (directory-of path))) (and (plusp (length d)) (path-encode d)))) (format nil "~a deleted." path))))
      (ingot:git-error (e) (bad-request "~a" e)))))

(defun switch-page (req ent project)
  "The ref menu's target: back to the same kind of page on the chosen ref."
  (let ((kind (or (url-query-value req "kind") "tree"))
        (ref (or (url-query-value req "ref") (getf project :default-branch)))
        (path (or (url-query-value req "path") "")))
    (redirect req ent (format nil "~a/-/~a/~a~@[/~a~]" (project-url project)
                              (if (member kind '("tree" "blob" "commits") :test #'string=) kind "tree")
                              (url-encode ref) (and (plusp (length path)) (path-encode path)))
              :status 302)))
