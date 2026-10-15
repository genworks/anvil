;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :anvil)

;;
;; The dispatcher: every address the forge answers, routed to the page
;; or door that serves it; and initialize!, which loads the store,
;; publishes the handler and starts the threads.
;;

(defun path-segments (path)
  (remove "" (uiop:split-string path :separator "/") :test #'string=))

(defun route (req ent)
  (let* ((path (request-path req))
         (segments (path-segments path))
         (method (http-method req)))
    (flet ((get? () (eq method :get))
           (post? () (eq method :post))
           (seg (n) (nth n segments)))
      (cond
        ((null segments) (home-page req ent))
        ((and (equal path "/robots.txt") (get?))
         (respond req ent *robots-txt* :content-type "text/plain; charset=utf-8" :cache-control "public, max-age=86400"))
        ;; static files
        ((string= (seg 0) "static")
         (let ((name (format nil "~{~a~^/~}" (rest segments))))
           (when (or (search ".." name) (blank? name)) (not-found))
           (let ((file (probe-file (static-file name))))
             (unless file (not-found))
             (respond req ent (ingot:read-file-octets file) :content-type (content-type-for name)
                      :cache-control "public, max-age=86400"))))
        ;; the API
        ((and (string= (seg 0) "api") (equal (seg 1) "v4"))
         (api-dispatch req ent (cddr segments)))
        ;; OAuth
        ((string= (seg 0) "oauth")
         (cond ((equal (seg 1) "authorize") (oauth-authorize-page req ent))
               ((and (equal (seg 1) "token") (post?)) (oauth-token-door req ent))
               ((and (equal (seg 1) "revoke") (post?)) (oauth-revoke-door req ent))
               (t (not-found))))
        ;; accounts
        ((string= (seg 0) "users")
         (cond ((equal (seg 1) "sign_in") (if (post?) (sign-in-post req ent) (sign-in-page req ent)))
               ((equal (seg 1) "sign_up") (if (post?) (sign-up-post req ent) (sign-up-page req ent)))
               ((and (equal (seg 1) "sign_out") (post?)) (sign-out-post req ent))
               (t (not-found))))
        ((and (string= (seg 0) "-") (equal (seg 1) "profile"))
         (cond ((null (seg 2)) (if (post?) (profile-post req ent) (profile-page req ent)))
               ((and (equal (seg 2) "password") (post?)) (password-post req ent))
               ((equal (seg 2) "tokens")
                (cond ((and (seg 3) (equal (seg 4) "revoke") (post?)) (token-revoke-post req ent (parse-integer (seg 3))))
                      ((post?) (tokens-post req ent))
                      (t (tokens-page req ent))))
               ((equal (seg 2) "keys")
                (cond ((and (seg 3) (equal (seg 4) "delete") (post?)) (key-delete-post req ent (parse-integer (seg 3))))
                      ((post?) (keys-post req ent))
                      (t (keys-page req ent))))
               (t (not-found))))
        ;; the site's pages
        ((string= (seg 0) "explore") (if (equal (seg 1) "groups") (explore-groups-page req ent) (explore-page req ent)))
        ((string= (seg 0) "dashboard") (dashboard-page req ent))
        ((string= (seg 0) "search") (explore-page req ent))
        ((string= (seg 0) "help") (help-page req ent))
        ((and (string= (seg 0) "projects") (equal (seg 1) "new")) (if (post?) (new-project-post req ent) (new-project-page req ent)))
        ((and (string= (seg 0) "groups") (equal (seg 1) "new")) (if (post?) (new-group-post req ent) (new-group-page req ent)))
        ((string= (seg 0) "admin")
         (cond ((null (seg 1)) (admin-page req ent))
               ((and (equal (seg 1) "users") (null (seg 2)) (post?)) (admin-new-user-post req ent))
               ((and (equal (seg 1) "users") (seg 2) (post?)) (admin-user-post req ent (parse-integer (seg 2))))
               ((and (equal (seg 1) "applications") (post?)) (admin-application-post req ent))
               (t (not-found))))
        ;; namespaces, projects, git
        (t (route-namespace req ent segments))))))

(defun find-project-prefix (segments)
  "Two values: the project the longest leading run of SEGMENTS names
(with or without .git on the last), and the segments after it."
  (loop for n from (min 4 (length segments)) downto 2
        do (let* ((head (subseq segments 0 n))
                  (last (car (last head)))
                  (clean (if (ends-with? ".git" last) (subseq last 0 (- (length last) 4)) last))
                  (project (find-project (format nil "~{~a~^/~}" (append (butlast head) (list clean))))))
             (when project (return (values project (subseq segments n) (ends-with? ".git" last)))))
        finally (return (values nil nil nil))))

(defun route-namespace (req ent segments)
  (multiple-value-bind (project rest git-suffix?) (find-project-prefix segments)
    (cond
      (project
       (let ((rest-path (format nil "/~{~a~^/~}" rest)))
         (cond ((and (or git-suffix? (git-request? rest-path)) (git-request? rest-path))
                (handle-git-request req ent project rest-path))
               ((not (can-read-project? project)) (if *current-user* (not-found "No such project.") (unauthorized)))
               (git-suffix? (redirect req ent (project-url project) :status 301))
               (t (route-project req ent project rest)))))
      (t
       ;; a group (any depth) with its own pages, or a user
       (let* ((dash (position "-" segments :test #'string=))
              (namespace-segments (if dash (subseq segments 0 dash) segments))
              (group (find-group (format nil "~{~a~^/~}" namespace-segments))))
         (cond ((and group dash (equal (nth (1+ dash) segments) "settings"))
                (let ((what (nth (+ dash 2) segments)))
                  (cond ((null what) (if (eq (http-method req) :post)
                                         (progn (unless (>= (or (group-access-level group *current-user*) 0) 40) (forbidden))
                                                (update-record! group :name (param req "name") :description (param req "description")
                                                                      :visibility (intern (string-upcase (or (param req "visibility") "public")) :keyword))
                                                (redirect req ent (with-notice (format nil "~a/-/settings" (group-url group)) "Saved.")))
                                         (group-settings-page req ent group)))
                        ((string= what "members") (unless (>= (or (group-access-level group *current-user*) 0) 40) (forbidden))
                         (members-post req ent group (format nil "~a/-/settings" (group-url group))))
                        ((string= what "variables") (unless (>= (or (group-access-level group *current-user*) 0) 40) (forbidden))
                         (variables-post req ent group (format nil "~a/-/settings" (group-url group))))
                        (t (not-found)))))
               (group (group-page req ent group))
               ((and (= (length segments) 1) (find-user (first segments)) (not dash))
                (user-page req ent (find-user (first segments))))
               (t (not-found "No such page."))))))))

(defun route-project (req ent project rest)
  (let ((method (http-method req)))
    (labels ((post? () (eq method :post))
             (seg (n) (nth n rest))
             (iid-thing (kind n) (let ((iid (and (seg n) (every #'digit-char-p (seg n)) (parse-integer (seg n)))))
                                   (and iid (if (eq kind :issue) (find-issue project iid) (find-merge-request project iid))))))
      (cond
        ((null rest) (project-home-page req ent project))
        ((not (string= (seg 0) "-")) (not-found "No such page."))
        (t
         (let ((what (seg 1)))
           (cond
             ((null what) (project-home-page req ent project))
             ((string= what "tree")
              (multiple-value-bind (ref path) (if (cddr rest) (split-ref-and-path project (cddr rest)) (values (getf project :default-branch) ""))
                (tree-page req ent project ref path)))
             ((string= what "blob")
              (multiple-value-bind (ref path) (split-ref-and-path project (cddr rest))
                (blob-page req ent project ref path)))
             ((string= what "raw")
              (multiple-value-bind (ref path) (split-ref-and-path project (cddr rest))
                (raw-page req ent project ref path)))
             ((string= what "commits")
              (multiple-value-bind (ref path) (if (cddr rest) (split-ref-and-path project (cddr rest)) (values (getf project :default-branch) ""))
                (commits-page req ent project ref path)))
             ((string= what "commit") (commit-page req ent project (url-decode (seg 2))))
             ((string= what "compare") (compare-page req ent project))
             ((string= what "switch") (switch-page req ent project))
             ((string= what "branches")
              (cond ((and (null (seg 2)) (post?)) (branch-create-post req ent project))
                    ((and (seg 2) (equal (car (last rest)) "delete") (post?))
                     (branch-delete-post req ent project (format nil "~{~a~^/~}" (mapcar #'url-decode (butlast (cddr rest))))))
                    (t (branches-page req ent project))))
             ((string= what "tags") (tags-page req ent project))
             ((string= what "archive")
              (multiple-value-bind (ref name) (values (format nil "~{~a~^/~}" (mapcar #'url-decode (butlast (cddr rest)))) (car (last rest)))
                (archive-page req ent project ref name)))
             ((and (string= what "star") (post?))
              (require-user)
              (if (member (getf *current-user* :id) (getf project :stars)) (unstar-project! project *current-user*) (star-project! project *current-user*))
              (redirect req ent (project-url project)))
             ((or (string= what "issues") (string= what "merge_requests"))
              (let ((kind (if (string= what "issues") :issue :merge-request)))
                (cond ((null (seg 2)) (things-list-page req ent project kind))
                      ((string= (seg 2) "new") (if (post?) (new-thing-post req ent project kind) (new-thing-page req ent project kind)))
                      (t (let ((thing (or (iid-thing kind 2) (not-found "No such item."))))
                           (cond ((null (seg 3)) (thing-page req ent project thing))
                                 ((not (post?)) (not-found))
                                 ((string= (seg 3) "notes") (note-post req ent project thing))
                                 ((string= (seg 3) "close") (thing-state-post req ent project thing :close))
                                 ((string= (seg 3) "reopen") (thing-state-post req ent project thing :reopen))
                                 ((string= (seg 3) "edit") (thing-edit-post req ent project thing))
                                 ((and (string= (seg 3) "merge") (eq kind :merge-request)) (merge-post req ent project thing))
                                 (t (not-found))))))))
             ((string= what "pipelines")
              (cond ((null (seg 2)) (if (post?) (pipeline-run-post req ent project) (pipelines-page req ent project)))
                    (t (let ((pipeline (and (every #'digit-char-p (seg 2)) (record :pipeline (parse-integer (seg 2))))))
                         (unless (and pipeline (eql (getf pipeline :project-id) (getf project :id))) (not-found "No such pipeline."))
                         (cond ((null (seg 3)) (pipeline-page req ent project pipeline))
                               ((and (string= (seg 3) "cancel") (post?)) (pipeline-action-post req ent project pipeline :cancel))
                               ((and (string= (seg 3) "retry") (post?)) (pipeline-action-post req ent project pipeline :retry))
                               (t (not-found)))))))
             ((string= what "jobs")
              (let ((job (and (seg 2) (every #'digit-char-p (seg 2)) (record :job (parse-integer (seg 2))))))
                (unless (and job (eql (getf job :project-id) (getf project :id))) (not-found "No such job."))
                (cond ((null (seg 3)) (job-page req ent project job))
                      ((string= (seg 3) "trace.json") (job-trace-json req ent project job))
                      ((and (string= (seg 3) "retry") (post?)) (job-action-post req ent project job :retry))
                      ((and (string= (seg 3) "play") (post?)) (job-action-post req ent project job :play))
                      ((and (string= (seg 3) "cancel") (post?)) (job-action-post req ent project job :cancel))
                      ((string= (seg 3) "artifacts") (job-artifacts-page req ent project job (format nil "~{~a~^/~}" (mapcar #'url-decode (cdddr rest)))))
                      (t (not-found)))))
             ((string= what "deployments") (if (post?) (deployments-post req ent project) (deployments-page req ent project)))
             ((string= what "settings")
              (cond ((null (seg 2)) (if (post?) (settings-post req ent project) (settings-page req ent project)))
                    ((not (post?)) (redirect req ent (format nil "~a/-/settings" (project-url project))))
                    ((string= (seg 2) "members") (require-can :maintain project) (members-post req ent project (format nil "~a/-/settings" (project-url project))))
                    ((string= (seg 2) "variables") (require-can :maintain project) (variables-post req ent project (format nil "~a/-/settings" (project-url project))))
                    ((string= (seg 2) "hooks") (settings-hooks-post req ent project))
                    ((string= (seg 2) "mirrors") (settings-mirrors-post req ent project))
                    ((string= (seg 2) "mirror") (settings-pull-mirror-post req ent project))
                    ((string= (seg 2) "rename") (settings-rename-post req ent project))
                    ((string= (seg 2) "delete") (settings-delete-post req ent project))
                    (t (not-found))))
             ((string= what "edit")
              (multiple-value-bind (ref path) (split-ref-and-path project (cddr rest))
                (if (post?) (edit-post req ent project ref path) (edit-page req ent project ref path))))
             ((string= what "new")
              (multiple-value-bind (ref path) (split-ref-and-path project (cddr rest))
                (if (post?) (edit-post req ent project ref path :new? t) (edit-page req ent project ref path :new? t))))
             ((and (string= what "delete") (post?))
              (multiple-value-bind (ref path) (split-ref-and-path project (cddr rest))
                (delete-post req ent project ref path)))
             (t (not-found "No such page.")))))))))

;;; Read-only: one gate in front of the dispatcher, so that a mutation
;;; is refused whichever page or program sent it

(defun read-only-refused? (req)
  "Whether a read-only forge refuses REQ: every method but GET and HEAD
(a fetch's POST to git-upload-pack aside), all of OAuth, the
advertisement that opens a push, and the deployments page, which
asks the house and writes what it hears."
  (let ((path (request-path req)))
    (or (and (not (member (http-method req) '(:get :head)))
             (not (ends-with? "/git-upload-pack" path)))
        (starts-with? "/oauth/" path)
        (and (ends-with? "/info/refs" path) (equal (url-query-value req "service") "git-receive-pack"))
        (ends-with? "/-/deployments" path))))

(defun refuse-read-only (req ent)
  "Answer REQ with the read-only refusal, in the form its asker reads:
plain text for git, JSON for the API and OAuth, else a page."
  (let ((path (request-path req)))
    (cond ((or (ends-with? "/info/refs" path) (ends-with? "/git-receive-pack" path))
           (respond-text req ent (format nil "~a Pushes are refused.~%" *read-only-text*) :status 403))
          ((starts-with? "/api/" path)
           (respond-error-json req ent 403 (format nil "403 Forbidden - ~a" *read-only-text*)))
          ((or (starts-with? "/oauth/token" path) (starts-with? "/oauth/revoke" path))
           (respond-json req ent (json-table "error" "access_denied" "error_description" *read-only-text*) :status 403))
          (t (respond-html req ent (page-html "Read-only"
                                                            (html (:div :class "narrow"
                                                                    (:h1 "Read-only for now")
                                                                    (:p (hs *read-only-text*) " Browsing works; nothing can be changed here.")
                                                                    (:p (:a :class "btn" :href "/explore" "Explore")))))
                           :status 403)))))

;;; The handler and the server

(defun anvil-handler (req ent)
  (with-request-user (req)
    (with-page-errors (req ent)
      (if (and *read-only?* (read-only-refused? req))
          (refuse-read-only req ent)
          (route req ent)))))

(defvar *published-servers* nil)

(defun publish! (&key (server net.aserve:*wserver*) (hosts *hosts*))
  "Publish the forge on SERVER for HOSTS (nil: every host)."
  (net.aserve:publish-prefix :prefix "/" :server server :host hosts :function #'anvil-handler)
  (pushnew server *published-servers*)
  server)

(defvar *housekeeping-thread* nil)

(defun housekeeping! ()
  (reap-sessions!)
  (reap-artifacts!)
  (refresh-open-deployments!)
  (bt:with-lock-held (*oauth-lock*) (prune-oauth-codes!)))

(defun initialize! (&key (server net.aserve:*wserver*) (hosts *hosts*) (runner? t))
  "Load the store, publish the forge on SERVER for HOSTS, start the
runner and the housekeeping.  Safe to call again.  ANVIL_READ_ONLY in
the environment makes the forge read-only whatever the start-up file
said; it never makes one writable."
  (when (read-only-environment?) (setq *read-only?* t))
  (ensure-directories-exist (data-path "db/"))
  (ensure-directories-exist (data-path "repositories/"))
  (ensure-directories-exist (data-path "ci/traces/"))
  (ensure-directories-exist (data-path "ci/artifacts/"))
  (load-store!)
  (setq *site* nil)
  (clrhash *project-views*)
  (publish! :server server :hosts hosts)
  (fail-orphaned-jobs!)
  (when runner? (start-runner))
  (start-hook-thread)
  (start-pull-mirror-thread)
  (setq *housekeeping-thread* (loop-thread *housekeeping-thread* "anvil housekeeping" 60 #'housekeeping!))
  (log-line "~a ~a published~@[ for ~{~a~^, ~}~]~:[~;, READ-ONLY~]; data in ~a; docker ~:[not reachable~;reachable~]; runner tags ~{~a~^, ~}"
            *site-name* *version* hosts *read-only?* (namestring *data-directory*) (docker-available?) *runner-tags*)
  :initialized)

(defun start! (&key (port 9090) (hosts *hosts*) (runner? t))
  "Start a web server of the forge's own on PORT and publish there: the
way to run Anvil beside other applications in one image, on a port
that carries nothing else."
  (let ((server (net.aserve:start :port port :listeners 10 :server :new)))
    (setq *site-url* (or *site-url* (format nil "http://localhost:~d" port)))
    (initialize! :server server :hosts hosts :runner? runner?)
    server))

(defun stop! ()
  (stop-runner)
  (dolist (server *published-servers*)
    (ignore-errors (net.aserve:shutdown :server server)))
  (setq *published-servers* nil)
  :stopped)
