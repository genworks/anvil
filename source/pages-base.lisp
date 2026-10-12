;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :anvil)

;;
;; The pages' foundation: the site object (the frame every page is
;; drawn in), the functions that build the pieces of a page, and the
;; project view (one per project, its repository facts cached until a
;; push).  The pages themselves are the pages-*.lisp files, each a
;; function of a request answering HTML through `page-html'.
;;
;; HTML is written with cl-who in its native form; text from a stranger
;; goes through `hs' on its way in.
;;

(defun hs (text) (html-escape text))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; cl-who prints literal strings and (str ...), (esc ...), (fmt ...)
  ;; only, and evaluates any other form for effect; `html' walks its
  ;; body so that an (hs ...) in body position prints, and a POST form
  ;; is greyed on a read-only forge.
  (defun post-form? (form)
    "Whether FORM is (:form ... :method \"post\" ...), written so."
    (and (eq (first form) :form)
         (let ((method (getf (nth-value 0 (who-attributes-and-body (rest form))) :method)))
           (and (stringp method) (string-equal method "post")))))
  (defun who-attributes-and-body (forms)
    "FORMS after a tag's keyword: two values, the attribute plist and the body."
    (loop while (and (consp forms) (keywordp (first forms)) (consp (rest forms)))
          collect (pop forms) into attributes
          collect (who-code (pop forms)) into attributes
          finally (return (values attributes forms))))
  (defun who-body (form)
    "FORM in a tag's or htm's body position, with (hs ...) made to print."
    (cond ((atom form) form)
          ((eq (first form) 'hs) `(str ,(who-code form)))
          ((eq (first form) 'htm) `(htm ,@(mapcar #'who-body (rest form))))
          ((keywordp (first form))
           (multiple-value-bind (attributes body) (who-attributes-and-body (rest form))
             (if (post-form? form)
                 ;; a form that changes something: its controls greyed
                 ;; on a read-only forge (read-only-fieldset)
                 `(,(first form) ,@attributes
                   (when *read-only?* (str (read-only-fieldset-open)))
                   ,@(mapcar #'who-body body)
                   (when *read-only?* (str "</fieldset>")))
                 `(,(first form) ,@attributes ,@(mapcar #'who-body body)))))
          ((and (consp (first form)) (keywordp (first (first form))))
           ;; ((:tag attrs...) body...)
           `((,(first (first form)) ,@(nth-value 0 (who-attributes-and-body (rest (first form)))))
             ,@(mapcar #'who-body (rest form))))
          (t (who-code form))))
  (defun who-code (form)
    "FORM as Lisp code, with any (htm ...) inside it walked as a body."
    (cond ((atom form) form)
          ((eq (first form) 'quote) form)
          ;; a nested html walks its own body when it expands
          ((eq (first form) 'html) form)
          ((eq (first form) 'htm) `(htm ,@(mapcar #'who-body (rest form))))
          (t (let ((walked nil) (tail form))
               (loop while (consp tail) do (push (who-code (pop tail)) walked))
               ;; a dotted tail stays as it is
               (let ((list (nreverse walked))) (if tail (nconc list tail) list)))))))

(defmacro html (&body body)
  "A string of HTML from cl-who forms; see the walk above."
  `(with-lhtml-string () ,@(mapcar #'who-body body)))

;;; Read-only: a form's controls sit in a disabled fieldset (the walk
;;; above puts it there), and a link to a change becomes a greyed span;
;;; both say why in their title.  The refusal itself is the gate in
;;; initialize.lisp.

(defun read-only-fieldset-open ()
  (format nil "<fieldset class=\"read-only\" disabled=\"disabled\" title=\"~a\">" (hs *read-only-text*)))

(defun mutating-link (href label &key (class "") style)
  "String of HTML: a link to HREF, a page that changes something; on a
read-only forge, the same control greyed and inert."
  (if *read-only?*
      (html (:span :class (format nil "~a disabled" class) :style style :aria-disabled "true"
                   :title (hs *read-only-text*) (hs label)))
      (html (:a :class (and (plusp (length class)) class) :style style :href href (hs label)))))

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

(define-object anvil-site (base-object)

  :documentation
  (:description "The forge's one shared site object: the frame every
page is drawn in (head, bar, footer), computed once from the site's
settings except for the bar, which depends on who is asking.  Pages
reach it through `page-html'.")

  :computed-slots
  ((site-name *site-name*)
   (stylesheet-version (let ((file (probe-file (static-file "anvil.css"))))
                         (if file (file-write-date file) 0)))
   (head-html
    (html (:meta :charset "utf-8")
          (:meta :name "viewport" :content "width=device-width, initial-scale=1")
          (:link :rel "stylesheet" :href (format nil "/static/anvil.css?v=~a" (the stylesheet-version)))
          (:link :rel "icon" :href "/static/anvil.svg" :type "image/svg+xml")
          (:script (str *site-script*))))
   (footer-html
    (html (:footer :class "foot"
            (:span (hs (the site-name)) " · a forge in Common Lisp and Gendl, "
                   (:a :href "https://gitlab.common-lisp.net/clo/anvil" "Anvil") " " (hs *version*))
            (:span (:a :href "/help" "Help") " · " (:a :href "/api/v4/version" "API"))))))

  :functions
  (("String of HTML: a whole page.  TITLE in the tab; BODY the main
column; CRUMBS a list of (label . href) over it; NOTICE a line shown
in a band under the bar; HEADER above the main column; WIDE? for the
full width."
    page (title body &key crumbs notice wide? header)
    (html
      (str "<!doctype html>")
      (:html :lang "en"
        (:head (:title (hs (format nil "~a · ~a" title (the site-name))))
               (str (the head-html)))
        (:body
          (str (the bar-html))
          (when notice (htm (:div :class "notice" (str notice))))
          (when crumbs (str (crumbs-html crumbs)))
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
                 (htm (str (mutating-link "/projects/new" "New project" :class "btn btn-small"))
                      (:details :class "menu"
                        (:summary (:span :class "avatar" (hs (user-avatar-letter *current-user*)))
                                  (hs (getf *current-user* :username)))
                        (:div :class "menu-items"
                          (:a :href (user-url *current-user*) "Your profile")
                          (:a :href "/-/profile" "Settings")
                          (:a :href "/-/profile/tokens" "Access tokens")
                          (when (admin?) (htm (:a :href "/admin" "Admin area")))
                          (:form :method "post" :action "/users/sign_out" (:button :type "submit" :class "linklike" "Sign out"))))))
                (t (htm (str (mutating-link "/users/sign_in" "Sign in" :class "btn btn-small"))
                        (when *registration-open?* (htm (str (mutating-link "/users/sign_up" "Register" :class "quiet")))))))))))))

(defvar *site* nil)

(defun site ()
  (or *site* (setq *site* (make-object 'anvil-site))))

(defun page-html (title body &key crumbs notice wide? header)
  "A whole page in the site's frame (anvil-site's `page')."
  (the-object (site) (page title body :crumbs crumbs :notice notice :wide? wide? :header header)))

(defun simple-page (title body-html &key notice)
  (page-html title (html (:div :class "narrow" (:h1 (hs title)) (str body-html))) :notice notice))

;;; The pieces pages are made of

(defun crumbs-html (crumbs)
  (html (:nav :class "crumbs" :aria-label "Breadcrumb"
          (loop for (label . href) in crumbs
                for first = t then nil
                do (unless first (htm (:span :class "crumb-sep" "/")))
                   (if href (htm (:a :href href (hs label))) (htm (:span (hs label))))))))

(defun card-html (body &key title class)
  (html (:section :class (format nil "card~@[ ~a~]" class)
          (when title (htm (:h2 :class "card-title" (str title))))
          (str body))))

(defun status-badge-html (status &key href)
  "The badge of a pipeline's or a job's STATUS, a link when HREF."
  (let* ((text (status-text status))
         (inner (html (:span :class (format nil "badge status-~a" text) (hs text)))))
    (if href (html (:a :href href :class "badge-link" (str inner))) inner)))

(defun avatar-html (user &key (size "avatar"))
  (if user
      (html (:a :class size :href (user-url user) :title (hs (user-display user)) (hs (user-avatar-letter user))))
      (html (:span :class size "?"))))

(defun user-link-html (user)
  (if user
      (html (:a :class "user" :href (user-url user) (hs (user-display user))))
      (html (:span :class "user" "someone"))))

(defun time-html (universal)
  (if universal
      (html (:time :datetime (iso-timestamp universal) :title (iso-timestamp universal) (hs (time-ago universal))))
      ""))

(defun form-field (name label &key (type "text") value placeholder required hint rows (autocomplete "off") options checked)
  "A labeled field: a textarea when ROWS, a select when OPTIONS (an
alist of value and label), a checkbox when TYPE says, else an input."
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

(defun form-button (label &key (type "submit") (class "btn") name value formaction confirm)
  (html (:button :type type :class class :name name :value value :formaction formaction
                 :onclick (and confirm (format nil "return confirm('~a')" (hs confirm)))
          (hs label))))

(defun html-table (columns rows &key class empty)
  "A table of ROWS, each a list of HTML strings, under COLUMNS; the
text EMPTY when there are no rows."
  (if (null rows)
      (html (:p :class "empty" (hs (or empty "Nothing here yet."))))
      (html (:div :class (format nil "table-wrap~@[ ~a~]" class)
              (:table
                (:thead (:tr (dolist (column columns) (htm (:th (str column))))))
                (:tbody (dolist (row rows)
                          (htm (:tr (dolist (cell row) (htm (:td (str cell)))))))))))))

(defun tabs-html (items active)
  "The tabs of a project page: ITEMS are (key label href [count])."
  (html (:nav :class "tabs"
          (loop for (key label href count) in items
                do (htm (:a :class (if (eq key active) "tab active" "tab") :href href
                          (hs label)
                          (when count (htm (:span :class "count" (hs count))))))))))

(defun prose-html (text &key base-url raw-base-url)
  "Markdown TEXT, rendered, in a prose block."
  (html (:div :class "prose" (str (markdown-html text :base-url base-url :raw-base-url raw-base-url)))))

(defun empty-state-html (title text &key action-href action-label)
  (html (:div :class "empty-state"
          (:h3 (hs title))
          (:p (str text))
          (when action-href (htm (str (mutating-link action-href action-label :class "btn")))))))

(defun oauth-consent-html (app redirect-uri state scope challenge method)
  (page-html "Authorize"
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
                       (str (form-button "Authorize" :name "decision" :value "allow"))
                       (str (form-button "Deny" :name "decision" :value "deny" :class "btn btn-quiet")))))))

(defun static-directory ()
  (or (ignore-errors (merge-pathnames "static/" (glisp:system-home :anvil)))
      (merge-pathnames "../static/" (or *load-truename* *compile-file-truename* #p"/projects/gw/anvil/source/"))))

(defun static-file (name)
  (merge-pathnames name (static-directory)))

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

(pushnew 'forget-project-view! *project-deleted-hooks*)

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
       (page-error ,req ,ent 500 "Something went wrong; the forge's log says what."))))

(defun page-error (req ent status text)
  (if (starts-with? "/api/" (request-path req))
      (respond-error-json req ent status text)
      (respond-html req ent
                    (page-html (format nil "~d" status)
                               (html (:div :class "narrow"
                                       (:h1 (hs (case status (404 "Not found") (403 "Not allowed") (401 "Sign in") (t (format nil "~d" status)))))
                                       (:p (hs text))
                                       (when (and (= status 401) (not *current-user*))
                                         (htm (:p (str (mutating-link (format nil "/users/sign_in?return_to=~a" (url-encode (request-path req))) "Sign in" :class "btn"))))))))
                    :status status)))

(defun flash-notice (req)
  "A notice carried in the query (?notice=...) after a redirect."
  (let ((notice (url-query-value req "notice")))
    (and notice (hs notice))))

(defun with-notice (url notice)
  (format nil "~a~anotice=~a" url (if (position #\? url) "&" "?") (url-encode notice)))
