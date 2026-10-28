;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :anvil)

;;
;; Releases: the list, one release, and the form that makes one -- a
;; tag (existing, or made at a ref) with a name and notes, the notes
;; written from the commits since the tag before when left blank.  The
;; downloads are the tag's archives.
;;

(defun release-archives-html (project release)
  (let ((url (project-url project)) (tag (getf release :tag-name)))
    (html (:a :href (format nil "~a/-/archive/~a/~a-~a.tar.gz" url (url-encode tag) (getf project :path) (ref-slug tag)) "tar.gz")
          " · " (:a :href (format nil "~a/-/archive/~a/~a-~a.zip" url (url-encode tag) (getf project :path) (ref-slug tag)) "zip"))))

(defun release-card-html (project release &key whole?)
  "One release: its name, tag, date, author, notes, assets and downloads."
  (let* ((url (project-url project))
         (repository (project-repository project))
         (commit-oid (ingot:resolve-commit repository (getf release :tag-name)))
         (author (record :user (getf release :author-id)))
         (tag (url-encode (getf release :tag-name))))
    (html (:div :class "card release"
            (:div :class "thing-head"
              (:h2 (if whole?
                       (htm (hs (getf release :name)))
                       (htm (:a :href (format nil "~a/-/releases/~a" url tag) (hs (getf release :name))))))
              (:a :class "badge" :href (format nil "~a/-/tree/~a" url tag) (hs (getf release :tag-name)))
              (when commit-oid (htm (:a :href (format nil "~a/-/commit/~a" url commit-oid) (:code :class "sha" (hs (subseq commit-oid 0 8))))))
              (:span :class "quiet" (when author (htm (str (user-link-html author)) " · "))
                     (str (time-html (or (getf release :released-at) (getf release :created))))))
            (unless (blank? (getf release :description))
              (htm (:div :class "prose" (str (markdown-html (getf release :description))))))
            (:div :class "assets"
              (:h3 "Assets")
              (:ul
                (dolist (asset (getf release :assets))
                  (htm (:li (:a :href (format nil "~a/-/releases/~a/downloads/~a" url tag (url-encode (getf asset :name))) (hs (getf asset :name)))
                            " " (:span :class "quiet" (hs (format nil "~:d bytes · ~a" (getf asset :size) (plural (or (getf asset :downloads) 0) "download"))))
                            (when (and whole? (can? :maintain project))
                              (htm " " (:form :method "post" :action (format nil "~a/-/releases/~a/assets/~a/delete" url tag (url-encode (getf asset :name))) :style "display:inline"
                                          (str (form-button "remove" :class "linklike quiet" :confirm (format nil "Remove ~a?" (getf asset :name))))))))))
                (dolist (link (getf release :links))
                  (htm (:li (:a :href (getf link :url) :rel "nofollow" (hs (getf link :name))) " " (:span :class "quiet" (hs (getf link :link-type)))
                            (when (and whole? (can? :maintain project))
                              (htm " " (:form :method "post" :action (format nil "~a/-/releases/~a/links/~d/delete" url tag (getf link :id)) :style "display:inline"
                                          (str (form-button "remove" :class "linklike quiet" :confirm (format nil "Remove the link ~a?" (getf link :name))))))))))
                (:li "Source: " (str (release-archives-html project release)))))))))

(defun release-asset-forms-html (project release)
  "The forms that add a file or a link to RELEASE, for those who may push."
  (let ((url (project-url project)) (tag (url-encode (getf release :tag-name))))
    (html (:div :class "grid"
            (:form :method "post" :action (format nil "~a/-/releases/~a/assets" url tag) :enctype "multipart/form-data" :class "card"
              (:h3 :class "card-title" "Add a file")
              (:div :class "field" (:input :type "file" :name "file" :required t))
              (str (form-button "Upload" :class "btn btn-small")))
            (:form :method "post" :action (format nil "~a/-/releases/~a/links" url tag) :class "card"
              (:h3 :class "card-title" "Add a link")
              (str (form-field "name" "Name" :required t))
              (str (form-field "url" "URL" :type "url" :required t :placeholder "https://"))
              (str (form-field "link_type" "Kind" :options '(("other" . "other") ("package" . "package") ("image" . "image") ("runbook" . "runbook"))))
              (str (form-button "Add link" :class "btn btn-small")))))))

(defun releases-page (req ent project)
  (let ((releases (project-releases project)) (url (project-url project)))
    (respond-html req ent
                  (project-page project "Releases"
                                (html (:div :class "thing-head"
                                        (:h2 "Releases")
                                        (when (can? :push project)
                                          (htm (str (mutating-link (format nil "~a/-/releases/new" url) "New release" :class "btn btn-small")))))
                                      (if releases
                                          (dolist (release releases) (str (release-card-html project release)))
                                          (str (empty-state-html "No releases yet"
                                                                 "A release is a tag with a name and notes; the notes can be written from the commits since the tag before."
                                                                 :action-href (and (can? :push project) (format nil "~a/-/releases/new" url))
                                                                 :action-label "New release"))))
                                :active :releases :notice (flash-notice req)))))

(defun release-page (req ent project release)
  (respond-html req ent
                (project-page project (getf release :name)
                              (html (str (release-card-html project release :whole? t))
                                    (when (can? :push project) (str (release-asset-forms-html project release)))
                                    (when (can? :maintain project)
                                      (htm (:form :method "post" :action (format nil "~a/-/releases/~a/delete" (project-url project) (url-encode (getf release :tag-name)))
                                             (str (form-button "Delete this release" :class "btn btn-small btn-quiet"
                                                               :confirm "Delete the release? The tag stays."))))))
                              :active :releases :notice (flash-notice req))))

(defun release-download-page (req ent project release name)
  "/-/releases/<tag>/downloads/<name>: the asset's file, and one more download counted."
  (let* ((asset (or (release-asset release name) (not-found "No such asset.")))
         (octets (or (release-asset-octets project release asset) (not-found "The asset's file is gone."))))
    (count-asset-download! release asset)
    (respond req ent octets :content-type (getf asset :content-type)
             :cache-control "public, max-age=3600"
             :headers (list :content-disposition (format nil "attachment; filename=\"~a\"" (getf asset :name))))))

(defun release-asset-post (req ent project release)
  (require-can :push project)
  (let ((part (or (multipart-part (multipart-parts req) "file") (bad-request "A file, in the field named file."))))
    (add-release-asset! project release (getf part :filename) (getf part :data) :content-type (getf part :content-type))
    (redirect req ent (with-notice (format nil "~a/-/releases/~a" (project-url project) (url-encode (getf release :tag-name)))
                                   (format nil "~a added." (safe-file-name (getf part :filename)))))))

(defun release-asset-delete-post (req ent project release name)
  (require-can :maintain project)
  (remove-release-asset! project release name)
  (redirect req ent (with-notice (format nil "~a/-/releases/~a" (project-url project) (url-encode (getf release :tag-name))) "Asset removed.")))

(defun release-link-post (req ent project release)
  (require-can :push project)
  (add-release-link! release (param req "name") (param req "url") :link-type (or (param req "link_type") "other"))
  (redirect req ent (with-notice (format nil "~a/-/releases/~a" (project-url project) (url-encode (getf release :tag-name))) "Link added.")))

(defun release-link-delete-post (req ent project release id)
  (require-can :maintain project)
  (remove-release-link! release id)
  (redirect req ent (with-notice (format nil "~a/-/releases/~a" (project-url project) (url-encode (getf release :tag-name))) "Link removed.")))

(defun new-release-page (req ent project &key error)
  (let* ((view (project-view project))
         (tags (mapcar #'first (the-object view tags))))
    (respond-html req ent
                  (project-page project "New release"
                                (html (:div :class "narrow" :style "margin-top:0"
                                        (:h2 "New release")
                                        (when error (htm (:p :class "profile-bad" (hs error))))
                                        (:form :method "post" :action (format nil "~a/-/releases/new" (project-url project))
                                          (str (form-field "tag_name" "Tag" :required t :placeholder "v1.0"
                                                           :hint (if tags
                                                                     (format nil "An existing tag (~{~a~^, ~}~:[~;, ...~]) or a new one, made at the ref below." (subseq tags 0 (min 6 (length tags))) (> (length tags) 6))
                                                                     "A new tag, made at the ref below.")))
                                          (str (form-field "ref" "Make the tag at" :value (getf project :default-branch) :hint "A branch or a commit; ignored when the tag exists."))
                                          (str (form-field "name" "Release title" :placeholder "Left blank, the tag's name."))
                                          (str (form-field "description" "Notes" :rows 8 :placeholder "Markdown. Left blank, the commits since the tag before."))
                                          (str (form-button "Create release")))))
                                :active :releases))))

(defun new-release-post (req ent project)
  (require-can :push project)
  (handler-case
      (let ((release (make-release! project *current-user* (trim (param req "tag_name"))
                                    :name (param req "name") :description (param req "description") :ref (param req "ref"))))
        (redirect req ent (with-notice (format nil "~a/-/releases/~a" (project-url project) (url-encode (getf release :tag-name)))
                                       (format nil "Release ~a made." (getf release :name)))))
    (http-error (e) (new-release-page req ent project :error (http-error-text e)))))

(defun release-delete-post (req ent project release)
  (require-can :maintain project)
  (delete-release! project release)
  (redirect req ent (with-notice (format nil "~a/-/releases" (project-url project)) "Release deleted; the tag stays.")))

(defun release-link-json (project release link)
  "GitLab's shape for one of a release's links; an uploaded asset is one
whose direct_asset_url is the forge's own download address."
  (let ((url (project-web-url project)) (tag (url-encode (getf release :tag-name))))
    (if (getf link :size)
        (json-table "id" (strcat "asset-" (getf link :name)) "name" (getf link :name)
                    "url" (format nil "~a/-/releases/~a/downloads/~a" url tag (url-encode (getf link :name)))
                    "direct_asset_url" (format nil "~a/-/releases/~a/downloads/~a" url tag (url-encode (getf link :name)))
                    "link_type" "package" "size" (getf link :size) "downloads" (or (getf link :downloads) 0))
        (json-table "id" (getf link :id) "name" (getf link :name) "url" (getf link :url)
                    "direct_asset_url" (getf link :url) "link_type" (getf link :link-type)))))

(defun release-json (project release)
  "GitLab's release shape; the tag's archives are the sources, the files
kept beside the release and the links added to it are the links."
  (let* ((repository (project-repository project))
         (commit-oid (ingot:resolve-commit repository (getf release :tag-name)))
         (url (project-web-url project)) (tag (url-encode (getf release :tag-name)))
         (links (append (getf release :assets) (getf release :links))))
    (json-table "tag_name" (getf release :tag-name) "name" (getf release :name)
                "description" (getf release :description)
                "created_at" (iso-timestamp-ms (getf release :created))
                "released_at" (iso-timestamp-ms (or (getf release :released-at) (getf release :created)))
                "upcoming_release" 'yason:false
                "author" (or (user-json (record :user (getf release :author-id))) :null)
                "commit" (if commit-oid (commit-json project (ingot:read-commit repository commit-oid)) :null)
                "commit_path" (and commit-oid (format nil "~a/-/commit/~a" (project-url project) commit-oid))
                "tag_path" (format nil "~a/-/tags" (project-url project))
                "assets" (json-table "count" (+ 2 (length links))
                                     "sources" (vector (json-table "format" "zip" "url" (format nil "~a/-/archive/~a/~a-~a.zip" url tag (getf project :path) (ref-slug (getf release :tag-name))))
                                                       (json-table "format" "tar.gz" "url" (format nil "~a/-/archive/~a/~a-~a.tar.gz" url tag (getf project :path) (ref-slug (getf release :tag-name)))))
                                     "links" (coerce (mapcar (lambda (link) (release-link-json project release link)) links) 'vector))
                "_links" (json-table "self" (format nil "~a/-/releases/~a" url tag)))))
