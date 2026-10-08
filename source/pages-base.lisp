;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :anvil)

;;
;; The pages' foundation: the site object (one shared instance: the
;; chrome, the page frame, the pieces every page is made of) and the
;; project view (one per project, its repository facts cached until a
;; push).  The pages themselves are the pages-*.lisp files, each a
;; function of a request answering HTML through `page'.
;;
;; HTML is written with cl-who in its native form, as every Gendl page
;; here is; text from a stranger goes through `hs' on its way in.
;;

(defun hs (text) (html-escape text))

(defmacro html (&body body)
  "A string of HTML from cl-who forms."
  `(with-lhtml-string () ,@body))

(defvar *site-script* "" "The pages' one script; set below, after the site object.")

(define-object anvil-site (base-object)

  :documentation
  (:description "The forge's one shared site object: the frame every
page is drawn in (head, bar, footer), and the small pieces.  Pages ask
it with (the-object (site) (page ...)).")

  :computed-slots
  ((site-name *site-name*)
   (stylesheet-version (let ((file (probe-file (static-file "anvil.css"))))
                         (if file (file-write-date file) 0)))
   (head-html
    (html (:meta :charset "utf-8")
          (:meta :name "viewport" :content "width=device-width, initial-scale=1")
          (:link :rel "stylesheet" :href (format nil "/static/anvil.css?v=~a" (the stylesheet-version)))
          (:link :rel "icon" :href "/static/anvil.svg" :type "image/svg+xml")
          (:script (str *site-script*)))))

  :functions
  (("String of HTML: a whole page.  TITLE in the tab; BODY the main
column; CRUMBS a list of (label . href) over it; NOTICE a line shown
in a band under the bar; WIDE? for the full width."
    page (title body &key crumbs notice (wide? nil) (header nil))
    (html
      (str "<!doctype html>")
      (:html :lang "en"
        (:head (:title (hs (format nil "~a · ~a" title (the site-name))))
               (str (the head-html)))
        (:body
          (str (the bar-html))
          (when notice (htm (:div :class "notice" (str notice))))
          (when crumbs (str (the (crumbs-html crumbs))))
          (when header (str header))
          (:main :class (if wide? "main wide" "main") (str body))
          (str (the footer-html))))))

   ("String of HTML: the bar across the top: the name, the explore
link, the search, and the signed-in user's menu or the sign-in link."
    bar-html ()
    (html
      (:header :class "bar"
        (:a :class "brand" :href "/" (:span :class "brand-mark" "⚒") (hs (the site-name)))
        (:nav :class "bar-nav"
          (:a :href "/explore" "Explore")
          (when *current-user* (htm (:a :href "/dashboard" "Your projects")))
          (:a :href "/explore/groups" "Groups"))
        (:form :class "bar-search" :method "get" :action "/search"
          (:input :type "search" :name "q" :placeholder "Search projects" :aria-label "Search projects"))
        (:div :class "bar-user"
          (cond (*current-user*
                 (htm (:a :class "btn btn-small" :href "/projects/new" "New project")
                      (:details :class "menu"
                        (:summary (:span :class "avatar" (hs (user-avatar-letter *current-user*)))
                                  (hs (getf *current-user* :username)))
                        (:div :class "menu-items"
                          (:a :href (user-url *current-user*) "Your profile")
                          (:a :href "/-/profile" "Settings")
                          (:a :href "/-/profile/tokens" "Access tokens")
                          (when (admin?) (htm (:a :href "/admin" "Admin area")))
                          (:form :method "post" :action "/users/sign_out" (:button :type "submit" :class "linklike" "Sign out"))))))
                (t (htm (:a :class "btn btn-small" :href "/users/sign_in" "Sign in")
                        (when *registration-open?* (htm (:a :class "quiet" :href "/users/sign_up" "Register"))))))))))

   (crumbs-html (crumbs)
    (html (:nav :class "crumbs" :aria-label "Breadcrumb"
            (loop for (label . href) in crumbs
                  for first = t then nil
                  do (unless first (htm (:span :class "crumb-sep" "/")))
                     (if href (htm (:a :href href (hs label))) (htm (:span (hs label))))))))

   (footer-html ()
    (html (:footer :class "foot"
            (:span (hs (the site-name)) " · a forge in Common Lisp and Gendl, "
                   (:a :href "https://gitlab.common-lisp.net/clo/anvil" "Anvil") " " (hs *version*))
            (:span (:a :href "/help" "Help") " · " (:a :href "/api/v4/version" "API")))))

   ;; Pieces

   ("String of HTML: a card with an optional title."
    card (body &key title class)
    (html (:section :class (format nil "card~@[ ~a~]" class)
            (when title (htm (:h2 :class "card-title" (str title))))
            (str body))))

   ("String of HTML: a status badge for a pipeline or job."
    status-badge (status &key href)
    (let* ((text (status-text status))
           (inner (html (:span :class (format nil "badge status-~a" text) (hs text)))))
      (if href (html (:a :href href :class "badge-link" (str inner))) inner)))

   (avatar (user &key (size "avatar"))
    (if user
        (html (:a :class size :href (user-url user) :title (hs (user-display user)) (hs (user-avatar-letter user))))
        (html (:span :class size "?"))))

   (user-link (user)
    (if user
        (html (:a :class "user" :href (user-url user) (hs (user-display user))))
        (html (:span :class "user" "someone"))))

   (time-html (universal)
    (if universal
        (html (:time :datetime (iso-timestamp universal) :title (iso-timestamp universal) (hs (time-ago universal))))
        ""))

   ("String of HTML: a labeled form field."
    field (name label &key (type "text") value placeholder required hint rows (autocomplete "off") options checked)
    (html (:div :class "field"
            (:label :for name (hs label))
            (cond (rows (htm (:textarea :id name :name name :rows rows :placeholder placeholder :required required (hs (or value "")))))
                  (options (htm (:select :id name :name name
                                  (loop for (v . l) in options
                                        do (htm (:option :value v :selected (and (equal v value) "selected") (hs l)))))))
                  ((string= type "checkbox")
                   (htm (:input :type "checkbox" :id name :name name :value "1" :checked (and checked "checked"))))
                  (t (htm (:input :type type :id name :name name :value value :placeholder placeholder
                                  :required required :autocomplete autocomplete))))
            (when hint (htm (:p :class "hint" (str hint)))))))

   (button (label &key (type "submit") (class "btn") name value formaction confirm)
    (html (:button :type type :class class :name name :value value :formaction formaction
                   :onclick (and confirm (format nil "return confirm('~a')" (hs confirm)))
            (hs label))))

   ("String of HTML: a table."
    table (columns rows &key class empty)
    (if (null rows)
        (html (:p :class "empty" (hs (or empty "Nothing here yet."))))
        (html (:div :class (format nil "table-wrap~@[ ~a~]" class)
                (:table
                  (:thead (:tr (dolist (column columns) (htm (:th (str column))))))
                  (:tbody (dolist (row rows)
                            (htm (:tr (dolist (cell row) (htm (:td (str cell)))))))))))))

   ("String of HTML: the tabs of a project page."
    tabs (items active)
    (html (:nav :class "tabs"
            (loop for (key label href count) in items
                  do (htm (:a :class (if (eq key active) "tab active" "tab") :href href
                            (hs label)
                            (when count (htm (:span :class "count" (hs count))))))))))

   (markdown (text &key base-url raw-base-url)
    (html (:div :class "prose" (str (markdown-html text :base-url base-url :raw-base-url raw-base-url)))))

   (empty-state (title text &key action-href action-label)
    (html (:div :class "empty-state"
            (:h3 (hs title))
            (:p (str text))
            (when action-href (htm (:a :class "btn" :href action-href (hs action-label)))))))

   (oauth-consent-html (app redirect-uri state scope challenge method)
    (the (page "Authorize"
               (html (:div :class "narrow"
                       (:h1 "Authorize " (hs (getf app :name)))
                       (:p (:strong (hs (getf app :name))) " asks to act on " (hs *site-name*) " as "
                           (:strong (hs (getf *current-user* :username))) " with the scope " (:code (hs scope)) ".")
                       (:p :class "hint" "It will be sent back to " (:code (hs redirect-uri)) ".")
                       (:form :method "post" :action "/oauth/authorize" :class "actions"
                         (:input :type "hidden" :name "client_id" :value (getf app :client-id))
                         (:input :type "hidden" :name "redirect_uri" :value redirect-uri)
                         (:input :type "hidden" :name "state" :value state)
                         (:input :type "hidden" :name "scope" :value scope)
                         (:input :type "hidden" :name "code_challenge" :value challenge)
                         (:input :type "hidden" :name "code_challenge_method" :value method)
                         (:input :type "hidden" :name "response_type" :value "code")
                         (str (the (button "Authorize" :name "decision" :value "allow")))
                         (str (the (button "Deny" :name "decision" :value "deny" :class "btn btn-quiet")))))))))))

(defvar *site* nil)

(defun site ()
  (or *site* (setq *site* (make-object 'anvil-site))))

(defun static-directory ()
  (or (ignore-errors (merge-pathnames "static/" (glisp:system-home :anvil)))
      (merge-pathnames "../static/" (or *load-truename* *compile-file-truename* #p"/projects/gw/anvil/source/"))))

(defun static-file (name)
  (merge-pathnames name (static-directory)))

(defparameter *site-script*
  "(function(){
document.addEventListener('DOMContentLoaded',function(){
  var t=document.querySelector('[data-trace]');
  if(t){var url=t.getAttribute('data-trace'),live=t.getAttribute('data-live')==='1',size=parseInt(t.getAttribute('data-size')||'0',10);
    function poll(){fetch(url+'?start='+size).then(function(r){return r.json();}).then(function(j){
      if(j.text){t.textContent+=j.text;size=j.size;window.scrollTo(0,document.body.scrollHeight);}
      if(j.live){setTimeout(poll,2000);}else{setTimeout(function(){location.reload();},1500);}
    }).catch(function(){setTimeout(poll,5000);});}
    if(live)setTimeout(poll,2000);}
  var ds=document.querySelectorAll('[data-poll]');
  if(ds.length){setTimeout(function(){location.reload();},10000);}
  var ms=document.querySelectorAll('details.menu');
  document.addEventListener('click',function(e){for(var i=0;i<ms.length;i++)if(!ms[i].contains(e.target))ms[i].open=false;});
  var c=document.querySelector('[data-copy]');
  if(c){c.addEventListener('click',function(){var v=c.getAttribute('data-copy');if(navigator.clipboard)navigator.clipboard.writeText(v);c.textContent='copied';});}
});})();"
  "The pages' one script: the job trace that follows itself, pages that
poll while something runs, the menus, the copy button.")

(defun simple-page (title body-html &key notice)
  (the-object (site) (page title (html (:div :class "narrow" (:h1 (hs title)) (str body-html))) :notice notice)))

;;; The project view: repository facts cached until a push

(define-object project-view (base-object)

  :documentation
  (:description "One per project for the life of the image: what costs
something to compute about the repository and changes only on a push.
A push sets head-stamp, and everything below it is computed afresh on
the next request.")

  :input-slots
  ((project-id)
   (head-stamp 0 :settable))

  :computed-slots
  ((project (progn (the head-stamp) (record :project (the project-id))))
   (repository (project-repository (the project)))
   (default-branch (getf (the project) :default-branch))
   (head-oid (progn (the head-stamp) (ingot:resolve-commit (the repository) (the default-branch))))
   (empty? (null (the head-oid)))
   (branches (progn (the head-stamp)
                    (sort (loop for (name . oid) in (ingot:branches (the repository))
                                collect (list name oid (ignore-errors (ingot:read-commit (the repository) oid))))
                          #'> :key (lambda (entry) (if (third entry) (ingot:commit-committer-time (third entry)) 0)))))
   (tags (progn (the head-stamp)
                (sort (loop for (name . oid) in (ingot:tags (the repository))
                            for commit-oid = (ingot:peel-to-commit (the repository) oid)
                            collect (list name oid (and commit-oid (ignore-errors (ingot:read-commit (the repository) commit-oid)))))
                      #'> :key (lambda (entry) (if (third entry) (ingot:commit-committer-time (third entry)) 0)))))
   (head-commit (and (the head-oid) (ingot:read-commit (the repository) (the head-oid))))
   (root-entries (and (the head-commit) (ingot:read-tree (the repository) (ingot:commit-tree (the head-commit)))))
   (readme-entry (and (the root-entries) (readme-entry (the root-entries))))
   (readme-html (let ((entry (the readme-entry)))
                  (and entry
                       (let ((data (ingot:read-blob (the repository) (ingot:tree-entry-oid entry))))
                         (if (markdown-file? (ingot:tree-entry-name entry))
                             (markdown-html (ingot:octets-to-string data)
                                            :base-url (format nil "~a/-/blob/~a" (project-url (the project)) (url-encode (the default-branch)))
                                            :raw-base-url (format nil "~a/-/raw/~a" (project-url (the project)) (url-encode (the default-branch))))
                             (html (:pre (hs (ingot:octets-to-string data)))))))))
   (file-count (if (the head-commit) (length (ingot:tree-paths (the repository) (ingot:commit-tree (the head-commit)))) 0))
   (commit-count (if (the head-oid) (ingot:count-commits (the repository) (the head-oid) :limit 10000) 0))
   (profile (progn (the head-stamp) (and (the head-oid) (ignore-errors (profile-report (the project) (the head-oid))))))
   (ci-file (and (the head-oid)
                 (cond ((ingot:blob-at (the repository) (the head-oid) "ci.sexp") "ci.sexp")
                       ((ingot:blob-at (the repository) (the head-oid) ".gitlab-ci.yml") ".gitlab-ci.yml")
                       (t nil))))))

(defvar *project-views* (make-hash-table))
(defvar *project-views-lock* (bt:make-lock "anvil project views"))

(defun project-view (project)
  (bt:with-lock-held (*project-views-lock*)
    (or (gethash (getf project :id) *project-views*)
        (setf (gethash (getf project :id) *project-views*)
              (make-object 'project-view :project-id (getf project :id))))))

(defun refresh-project-view! (project)
  (let ((view (bt:with-lock-held (*project-views-lock*) (gethash (getf project :id) *project-views*))))
    (when view
      (the-object view (set-slot! :head-stamp (1+ (the-object view head-stamp)))))))

(pushnew 'refresh-project-view! *project-changed-hooks*)

(defun forget-project-view! (project)
  (bt:with-lock-held (*project-views-lock*) (remhash (getf project :id) *project-views*)))

;;; What every page handler shares

(defmacro with-page-errors ((req ent) &body body)
  "Run BODY; an http-error becomes a page with its status, any other
error a 500 page (and a log line)."
  `(handler-case (progn ,@body)
     (http-error (e)
       (page-error ,req ,ent (http-error-status e) (http-error-text e)))
     (ingot:not-found (e)
       (page-error ,req ,ent 404 (princ-to-string e)))
     (ci-spec-error (e)
       (page-error ,req ,ent 400 (princ-to-string e)))
     (error (e)
       (log-line "~a ~a: ~a" (http-method ,req) (request-path ,req) e)
       (page-error ,req ,ent 500 (format nil "Something went wrong: ~a" e)))))

(defun page-error (req ent status text)
  (if (starts-with? "/api/" (request-path req))
      (respond-error-json req ent status text)
      (respond-html req ent
                    (the-object (site) (page (format nil "~d" status)
                                             (html (:div :class "narrow"
                                                     (:h1 (hs (case status (404 "Not found") (403 "Not allowed") (401 "Sign in") (t (format nil "~d" status)))))
                                                     (:p (hs text))
                                                     (when (and (= status 401) (not *current-user*))
                                                       (htm (:p (:a :class "btn" :href (format nil "/users/sign_in?return_to=~a" (url-encode (request-path req))) "Sign in"))))))))
                    :status status)))

(defun flash-notice (req)
  "A notice carried in the query (?notice=...) after a redirect."
  (let ((notice (url-query-value req "notice")))
    (and notice (hs notice))))

(defun with-notice (url notice)
  (format nil "~a~a notice=~a" url (if (position #\? url) "&" "?") (url-encode notice)))
