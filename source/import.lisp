;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :anvil)

;;
;; Importing from a GitLab: repositories through Ingot's fetch with
;; AllegroServe's client as the transport, issues and merge requests
;; with their notes through GitLab's REST API, written as the native
;; records with their numbers kept and :imported-from on each.  People
;; are not imported: a user of the same username here is matched, else
;; the importer authors the text with a first line naming who wrote
;; it there.  Importing again brings only what is new.
;;

(defparameter *git-user-agent* "git/anvil-import"
  "What the fetch client calls itself: common-lisp.net's bot challenge
(Anubis) passes a client whose user agent begins with git/ and whose
Accept-Encoding names gzip, and challenges anything else.")

(defparameter *fetch-timeout* 1800
  "Seconds. How long one git request to another server may take: a
whole history comes in one answer.")

(defparameter *gitlab-timeout* 120
  "Seconds. How long one API request to a GitLab may take.")

;;; Git over HTTP, the client side

(defun http-git-transport (url &key auth)
  "A transport for ingot:fetch!: a function of a service keyword and a
body that speaks smart HTTP to the repository at URL (its .git
address) with AllegroServe's client, in process.  AUTH is (user .
password-or-token), or nil for a public repository."
  (let ((url (string-right-trim "/" url)))
    (lambda (service body)
      (let* ((service-name (format nil "git-~(~a~)" service))
             (address (if body
                          (format nil "~a/~a" url service-name)
                          (format nil "~a/info/refs?service=~a" url service-name)))
             ;; User-Agent and Accept go through the client's own keywords:
             ;; given as headers they ride beside its defaults, and the
             ;; challenge reads the first of each.  gzip at q=0 meets the
             ;; rule while the answer comes plain, which AllegroServe's
             ;; client cannot inflate on every Lisp.
             (headers (list (cons "Accept-Encoding" "identity, gzip;q=0"))))
        (multiple-value-bind (answer status response-headers)
            (net.aserve.client:do-http-request address
              :method (if body :post :get) :content body
              :content-type (and body (format nil "application/x-~a-request" service-name))
              :headers headers :user-agent *git-user-agent*
              :accept (if body (format nil "application/x-~a-result" service-name) "*/*")
              :basic-authorization auth :format :binary :keep-alive nil
              :timeout *fetch-timeout*
              :ssl-args (let ((host (url-host url))) (and host (list :server-name host))))
          (unless (eql status 200)
            (error "~a answered ~a to ~a." url status (if body service-name "the refs advertisement")))
          (let ((octets (ingot:as-octets answer))
                (type (cdr (assoc :content-type response-headers))))
            (when (and (equalp (cdr (assoc :content-encoding response-headers)) "gzip")
                       (> (length octets) 2) (= (aref octets 0) #x1f) (= (aref octets 1) #x8b))
              (setq octets (ingot:gunzip octets)))
            (unless (and type (starts-with? "application/x-git" type))
              (error "~a answered ~a, not a git service: a challenge page, or a sign-in?" url (or type "no content type")))
            octets))))))

(defun fetch-repository! (project url &key auth)
  "Bring PROJECT's repository up to date with the one at URL: every
branch and tag, HEAD too, and the project's default branch with it.
Values as ingot:fetch!'s: the refs set, the refs deleted, the objects
received."
  (let ((repository (project-repository project)))
    (multiple-value-bind (set deleted received)
        (ingot:fetch! repository (http-git-transport url :auth auth))
      (let ((head (ingot:default-branch repository)))
        (unless (equal head (getf project :default-branch))
          (setq project (update-record! project :default-branch head
                                                :protected-branches (adjoin head (getf project :protected-branches) :test #'string=)))))
      (when (or set deleted)
        (touch-project! project)
        (project-changed! (find-project (getf project :id))))
      (values set deleted received))))

;;; GitLab's API, the little of it an import reads

(defun make-gitlab-client (url &key token)
  "A GitLab at URL (\"https://gitlab.common-lisp.net\"), asked with
TOKEN (a personal access token with read_api) when given."
  (list :url (string-right-trim "/" url) :token token))

(defun gitlab-get (client path &key params pages?)
  "GET PATH under the client's /api/v4 with PARAMS (an alist of
strings): the parsed JSON.  With PAGES?, every page of a list,
followed through X-Next-Page, as one list."
  (flet ((one (page)
           (let* ((query (format nil "~{~a=~a~^&~}"
                                 (loop for (k . v) in (append params (when page (list (cons "per_page" "100") (cons "page" (princ-to-string page)))))
                                       collect (url-encode k) collect (url-encode v))))
                  (address (format nil "~a/api/v4~a~@[?~a~]" (getf client :url) path (and (plusp (length query)) query))))
             (multiple-value-bind (body status headers)
                 (net.aserve.client:do-http-request address
                   :method :get
                   :user-agent (format nil "anvil/~a" *version*)
                   :headers (when (getf client :token) (list (cons "PRIVATE-TOKEN" (getf client :token))))
                   :external-format :utf-8 :timeout *gitlab-timeout* :keep-alive nil
                   :ssl-args (let ((host (url-host (getf client :url)))) (and host (list :server-name host))))
               (unless (eql status 200)
                 (error "~a answered ~a to ~a~@[: ~a~]." (getf client :url) status path
                        (and (stringp body)
                             (let ((json (ignore-errors (parse-json body))))
                               (and (hash-table-p json) (json-get json "message"))))))
               ;; common-lisp.net's bot challenge (Anubis) answers anonymous
               ;; API calls with a 200 page of HTML; a token gets JSON
               (let ((type (cdr (assoc :content-type headers))))
                 (unless (and type (search "json" type))
                   (error "~a answered ~a to ~a, not JSON: ~:[a bot challenge, most likely; pass :token (a personal access token with read_api)~;a challenge page or a sign-in~]."
                          (getf client :url) (or type "no content type") path (getf client :token))))
               (values (parse-json body)
                       (let ((next (cdr (assoc :x-next-page headers))))
                         (and (stringp next) (plusp (length next)) (ignore-errors (parse-integer next)))))))))
    (if pages?
        (let ((all nil) (page 1))
          (loop
            (multiple-value-bind (items next) (one page)
              (setq all (append all (coerce items 'list)))
              (if (and next (> next page)) (setq page next) (return))))
          all)
        (one nil))))

(defun last-segment (path)
  (subseq path (1+ (or (position #\/ path :from-end t) -1))))

(defun gitlab-find-group (client full-path)
  "The group at FULL-PATH (subgroups as a/b), as the API describes it,
or nil.  Found through a search: a path with a slash in it does not
survive every HTTP client as a URL segment."
  (find full-path (gitlab-get client "/groups" :params (list (cons "search" (last-segment full-path)) (cons "all_available" "true")) :pages? t)
        :key (lambda (g) (json-get g "full_path")) :test #'string-equal))

(defun gitlab-find-project (client path-with-namespace)
  "The project at PATH-WITH-NAMESPACE as the API describes it, or nil."
  (find path-with-namespace
        (gitlab-get client "/projects" :params (list (cons "search" (last-segment path-with-namespace)) (cons "simple" "true")) :pages? t)
        :key (lambda (p) (json-get p "path_with_namespace")) :test #'string-equal))

(defun gitlab-group-projects (client group)
  "The projects of GROUP (as the API describes it), its own and not its
subgroups', by path."
  (gitlab-get client (format nil "/groups/~d/projects" (json-get group "id"))
              :params '(("simple" . "true") ("include_subgroups" . "false") ("order_by" . "path") ("sort" . "asc"))
              :pages? t))

(defun gitlab-visibility (json)
  (let ((v (json-get json "visibility")))
    (cond ((equal v "private") :private)
          ((equal v "internal") :internal)
          (t :public))))

(defun gitlab-feature-public? (json feature)
  "Whether FEATURE (\"issues\", \"merge_requests\", \"repository\") of the
GitLab project JSON is open to everyone who can see the project: its
access level `enabled', or not given.  `private' is members only;
`disabled' is off."
  (let ((level (json-get json (format nil "~a_access_level" feature))))
    (or (null level) (equal level "enabled"))))

(defun gitlab-time (text)
  (or (parse-iso-timestamp text) (get-universal-time)))

(defun json-true? (value) (eq value 'yason:true))

;;; People: a user here of the same name, else the importer

(defun imported-user (json importer)
  "The user here for JSON (a GitLab user with a username): one of the
same username when there is one, else IMPORTER."
  (or (and json (find-user (json-get json "username"))) importer))

(defun imported-by-line (json when)
  (format nil "*~a (@~a) on GitLab, ~a.*"
          (or (json-get json "name") (json-get json "username") "Someone")
          (or (json-get json "username") "?")
          (iso-date (gitlab-time when))))

(defun imported-text (text author-json when author)
  "TEXT, beginning with a line saying who wrote it there and when
unless AUTHOR is that person here."
  (if (and author-json (string-equal (getf author :username) (json-get author-json "username")))
      (or text "")
      (format nil "~a~%~%~a" (imported-by-line author-json when) (or text ""))))

(defun imported-assignee-id (json)
  (let ((assignee (json-get json "assignee")))
    (and assignee (let ((user (find-user (json-get assignee "username")))) (and user (getf user :id))))))

(defun imported-note (json importer)
  (let ((author (imported-user (json-get json "author") importer))
        (system? (json-true? (json-get json "system"))))
    (list :id (next-id :note) :author-id (getf author :id)
          :body (if system?
                    (or (json-get json "body") "")
                    (imported-text (json-get json "body") (json-get json "author") (json-get json "created_at") author))
          :created (gitlab-time (json-get json "created_at"))
          :system? system?
          :imported-from (list :id (json-get json "id")))))

;;; Issues and merge requests, numbers kept

(defun import-gitlab-issues! (client project gitlab-id importer)
  "Every issue of the GitLab project GITLAB-ID not here yet (by its
iid), with its notes; answers how many were added."
  (let ((existing (project-issues project :state :all)) (count 0) (max-iid 0))
    (dolist (issue (gitlab-get client (format nil "/projects/~d/issues" gitlab-id)
                               :params '(("state" . "all") ("order_by" . "created_at") ("sort" . "asc")) :pages? t))
      (let ((iid (json-get issue "iid")))
        (setq max-iid (max max-iid iid))
        (unless (find iid existing :key (lambda (i) (getf i :iid)))
          (let* ((author (imported-user (json-get issue "author") importer))
                 (notes (gitlab-get client (format nil "/projects/~d/issues/~d/notes" gitlab-id iid)
                                    :params '(("order_by" . "created_at") ("sort" . "asc")) :pages? t)))
            (save-record! (list :kind :issue :id (next-id :issue)
                                :created (gitlab-time (json-get issue "created_at"))
                                :updated (parse-iso-timestamp (json-get issue "updated_at"))
                                :project-id (getf project :id) :iid iid
                                :title (or (json-get issue "title") "(untitled)")
                                :description (imported-text (json-get issue "description") (json-get issue "author") (json-get issue "created_at") author)
                                :author-id (getf author :id) :assignee-id (imported-assignee-id issue)
                                :labels (coerce (or (json-get issue "labels") #()) 'list)
                                :milestone (let ((m (json-get issue "milestone"))) (and m (json-get m "title")))
                                :state (if (equal (json-get issue "state") "closed") :closed :opened)
                                :closed-at (parse-iso-timestamp (json-get issue "closed_at"))
                                :notes (mapcar (lambda (n) (imported-note n importer)) notes)
                                :imported-from (list :id (json-get issue "id") :url (json-get issue "web_url"))))
            (incf count)))))
    (ensure-counter! (format nil "issue-~d" (getf project :id)) max-iid)
    count))

(defun import-gitlab-merge-requests! (client project gitlab-id importer)
  "Every merge request of the GitLab project GITLAB-ID not here yet,
with its notes, as a record of the same state (opened, merged,
closed); its branches are named as they were there, whether or not
the fetch brought them.  Answers how many were added."
  (let ((existing (project-merge-requests project :state :all)) (count 0) (max-iid 0))
    (dolist (mr (gitlab-get client (format nil "/projects/~d/merge_requests" gitlab-id)
                            :params '(("state" . "all") ("order_by" . "created_at") ("sort" . "asc")) :pages? t))
      (let ((iid (json-get mr "iid")))
        (setq max-iid (max max-iid iid))
        (unless (find iid existing :key (lambda (m) (getf m :iid)))
          (let* ((author (imported-user (json-get mr "author") importer))
                 (notes (gitlab-get client (format nil "/projects/~d/merge_requests/~d/notes" gitlab-id iid)
                                    :params '(("order_by" . "created_at") ("sort" . "asc")) :pages? t))
                 (state (let ((s (json-get mr "state")))
                          (cond ((equal s "merged") :merged) ((equal s "closed") :closed) (t :opened))))
                 (merger (let ((json (or (json-get mr "merge_user") (json-get mr "merged_by"))))
                           (and json (imported-user json nil)))))
            (save-record! (list :kind :merge-request :id (next-id :merge-request)
                                :created (gitlab-time (json-get mr "created_at"))
                                :updated (parse-iso-timestamp (json-get mr "updated_at"))
                                :project-id (getf project :id) :iid iid
                                :title (or (json-get mr "title") "(untitled)")
                                :description (imported-text (json-get mr "description") (json-get mr "author") (json-get mr "created_at") author)
                                :author-id (getf author :id) :assignee-id (imported-assignee-id mr)
                                :labels (coerce (or (json-get mr "labels") #()) 'list)
                                :source-branch (json-get mr "source_branch") :target-branch (json-get mr "target_branch")
                                :remove-source-branch? nil
                                :state state
                                :notes (mapcar (lambda (n) (imported-note n importer)) notes)
                                :merge-commit (json-get mr "merge_commit_sha")
                                :merged-by-id (and merger (getf merger :id))
                                :merged-at (parse-iso-timestamp (json-get mr "merged_at"))
                                :closed-at (parse-iso-timestamp (json-get mr "closed_at"))
                                :imported-from (list :id (json-get mr "id") :url (json-get mr "web_url"))))
            (incf count)))))
    (ensure-counter! (format nil "merge-request-~d" (getf project :id)) max-iid)
    count))

;;; Namespaces: a group here for a group there, made when missing

(defun ensure-import-group! (json importer)
  "The group here for JSON (a GitLab group, with its full_path), made
with its name, description and visibility when missing, its parents
first.  Answers the record."
  (let ((full-path (json-get json "full_path")))
    (or (find-group full-path)
        (let* ((slash (position #\/ full-path :from-end t))
               (parent (and slash (ensure-import-group! (json-table "full_path" (subseq full-path 0 slash)
                                                                    "name" (subseq full-path 0 slash) "path" (subseq full-path 0 slash))
                                                        importer)))
               (group (make-group! (last-segment full-path) (json-get json "name")
                                   :description (or (json-get json "description") "")
                                   :visibility (gitlab-visibility json)
                                   :owner importer :parent parent)))
          (update-record! group :imported-from (list :url (json-get json "web_url") :id (json-get json "id") :at (get-universal-time))
                                :created (gitlab-time (json-get json "created_at")))))))

(defun ensure-import-namespace! (json importer)
  "Two values: the namespace here for JSON (a GitLab project's
namespace) and its kind.  A user's namespace there lands on the user
of that name here when there is one, else on a group of that path."
  (let ((full-path (json-get json "full_path")))
    (multiple-value-bind (record kind) (find-namespace full-path)
      (if record
          (values record kind)
          (values (ensure-import-group! json importer) :group)))))

;;; Projects and groups

(defun import-gitlab-project! (client json &key namespace kind importer (issues? t) (merge-requests? t) (repository? t) auth
                                                (public-only? *read-only?*))
  "JSON, a GitLab project as the API describes it, made here as the
same path in NAMESPACE (KIND :group or :user; by default the namespace
of the same path here, made as a group when missing), its repository
fetched (every branch and tag, HEAD too), its issues and merge
requests brought over with their notes.  A project that exists already
is brought up to date instead.  AUTH is (user . token) for a
repository that is not public.  PUBLIC-ONLY? (on by default on a
read-only forge, which serves the public) leaves out a project that is
not public there, and the parts of a public one open to members only.
A part that fails (the repository, the issues, the merge requests) is
reported and the rest goes on.  Values: the project record (nil when
left out) and a report plist."
  (let* ((path (json-get json "path"))
         (gitlab-id (json-get json "id"))
         (full (format nil "~a/~a" (if namespace (namespace-path namespace kind) (json-get (json-get json "namespace") "full_path")) path))
         (report (list :project full :gitlab-id gitlab-id)))
    (when (and public-only? (not (eq (gitlab-visibility json) :public)))
      (log-line "import ~a: left out, ~(~a~) there" full (gitlab-visibility json))
      (return-from import-gitlab-project! (values nil (append report (list :skipped (format nil "~(~a~) there" (gitlab-visibility json)))))))
    (when public-only?
      ;; a listing (simple=true) carries no access levels: ask for the
      ;; project itself, and on no answer leave its issues and merge
      ;; requests out rather than guess
      (let ((full-json (if (json-get json "issues_access_level")
                           json
                           (handler-case (gitlab-get client (format nil "/projects/~d" gitlab-id))
                             (error (e)
                               (log-line "import ~a: no access levels (~a): its issues and merge requests left out" full e)
                               nil)))))
        (setq repository? (and repository? (gitlab-feature-public? (or full-json json) "repository"))
              issues? (and issues? full-json (gitlab-feature-public? full-json "issues"))
              merge-requests? (and merge-requests? full-json (gitlab-feature-public? full-json "merge_requests")))))
    (unless namespace
      (multiple-value-setq (namespace kind) (ensure-import-namespace! (json-get json "namespace") importer)))
    (let ((project (or (find-project full)
                       (make-project! namespace kind path
                                      :name (json-get json "name")
                                      :description (or (json-get json "description") "")
                                      :visibility (gitlab-visibility json)
                                      :default-branch (or (json-get json "default_branch") *default-branch-name*)
                                      :creator importer))))
      (setq project (update-record! project
                                    :imported-from (list :url (json-get json "web_url") :id gitlab-id :api (getf client :url) :at (get-universal-time))
                                    :created (gitlab-time (json-get json "created_at"))))
      (when repository?
        (handler-case
            (multiple-value-bind (set deleted received) (fetch-repository! project (json-get json "http_url_to_repo") :auth auth)
              (setf (getf report :refs-set) (length set)
                    (getf report :refs-deleted) (length deleted)
                    (getf report :objects) received))
          (error (e)
            (log-line "import ~a: the repository: ~a" full e)
            (setf (getf report :repository-error) (princ-to-string e)))))
      (when (and issues? (getf project :issues-enabled? t))
        (handler-case (setf (getf report :issues) (import-gitlab-issues! client project gitlab-id importer))
          (error (e)
            (log-line "import ~a: the issues: ~a" full e)
            (setf (getf report :issues-error) (princ-to-string e)))))
      (when (and merge-requests? (getf project :merge-requests-enabled? t))
        (handler-case (setf (getf report :merge-requests) (import-gitlab-merge-requests! client project gitlab-id importer))
          (error (e)
            (log-line "import ~a: the merge requests: ~a" full e)
            (setf (getf report :merge-requests-error) (princ-to-string e)))))
      (let ((project (find-project (getf project :id))))
        (update-record! project :last-activity (gitlab-time (json-get json "last_activity_at")))
        (log-line "imported ~a: ~s" full report)
        (values (find-project (getf project :id)) report)))))

(defun import-gitlab-group! (client group-path &key (projects :all) importer (issues? t) (merge-requests? t) (repository? t) auth
                                                    (public-only? *read-only?*))
  "The group GROUP-PATH of the GitLab behind CLIENT made here as it is
there, and its projects imported: all of them, or those whose path
(or path with namespace) is in PROJECTS; with PUBLIC-ONLY?, the public
ones (import-gitlab-project!).  Values: the group record and a report
per project."
  (let* ((json (or (gitlab-find-group client group-path) (error "No group ~a at ~a." group-path (getf client :url))))
         (group (ensure-import-group! json importer))
         (reports nil))
    (dolist (p (gitlab-group-projects client json))
      (when (or (eq projects :all)
                (member (json-get p "path") projects :test #'string-equal)
                (member (json-get p "path_with_namespace") projects :test #'string-equal))
        (multiple-value-bind (project report)
            (import-gitlab-project! client p :namespace group :kind :group :importer importer
                                             :issues? issues? :merge-requests? merge-requests? :repository? repository? :auth auth
                                             :public-only? public-only?)
          (declare (ignore project))
          (push report reports))))
    (values group (nreverse reports))))

(defun import-from-gitlab! (url &key token groups projects into importer (issues? t) (merge-requests? t) (repository? t) auth
                                     (public-only? *read-only?*))
  "Import from the GitLab at URL (\"https://gitlab.common-lisp.net\"),
asked with TOKEN (a personal access token with read_api; public
things need none).  GROUPS: group paths, each imported with every
project, or (\"path\" \"project\" ...) for some of its projects.
PROJECTS: \"namespace/path\" strings, each landing in the namespace of
that path here (made as a group when missing), or all of them in the
group INTO.  IMPORTER: the user here who authors what has no author
here, the first administrator by default.  AUTH: (user . token) for
repositories that are not public.  PUBLIC-ONLY?, on by default on a
read-only forge: only what the public sees there is brought here
(import-gitlab-project!), whatever TOKEN can see.  Answers a report per
project."
  (let* ((client (make-gitlab-client url :token token))
         (importer (or importer
                       (find-record :user (lambda (u) (getf u :admin?)))
                       (error "No importer: make an administrator first.")))
         (into-group (and into (or (find-group into) (make-group! into into :owner importer))))
         (reports nil))
    (dolist (g groups)
      (multiple-value-bind (group group-reports)
          (import-gitlab-group! client (if (consp g) (first g) g)
                                :projects (if (consp g) (rest g) :all)
                                :importer importer :issues? issues? :merge-requests? merge-requests? :repository? repository? :auth auth
                                :public-only? public-only?)
        (declare (ignore group))
        (setq reports (append reports group-reports))))
    (dolist (p projects)
      (let ((json (or (gitlab-find-project client p) (error "No project ~a at ~a." p url))))
        (multiple-value-bind (project report)
            (import-gitlab-project! client json :namespace into-group :kind (and into-group :group) :importer importer
                                                :issues? issues? :merge-requests? merge-requests? :repository? repository? :auth auth
                                                :public-only? public-only?)
          (declare (ignore project))
          (setq reports (append reports (list report))))))
    reports))
