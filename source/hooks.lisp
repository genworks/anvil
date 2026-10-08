;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :anvil)

;;
;; What happens after something happened: a push starts a pipeline,
;; posts the webhooks, pushes the mirrors and stamps the project; an
;; issue, a merge request or a pipeline posts its webhooks.  Deliveries
;; go from a thread of their own, never from the request that caused
;; them.
;;

(defvar *hook-thread* nil)
(defvar *hook-queue* nil)
(defvar *hook-queue-lock* (bt:make-lock "anvil hooks"))

(defun enqueue-hook! (function)
  "Run FUNCTION (of no arguments) on the hooks thread, soon."
  (bt:with-lock-held (*hook-queue-lock*)
    (setq *hook-queue* (append *hook-queue* (list function))))
  (start-hook-thread))

(defun start-hook-thread ()
  (setq *hook-thread*
        (loop-thread *hook-thread* "anvil hooks" 1
                     (lambda ()
                       (loop
                         (let ((next (bt:with-lock-held (*hook-queue-lock*) (pop *hook-queue*))))
                           (unless next (return))
                           (handler-case (funcall next)
                             (error (e) (log-line "hook: ~a" e)))))))))

;;; After a push

(defun after-push! (project user commands)
  "COMMANDS are the ref-commands a push made (ingot structures, with
their :status).  For each ref that changed: the pipeline, the hooks,
the mirrors."
  (let ((changed (remove-if-not (lambda (c) (eq (ingot::ref-command-status c) :ok)) commands)))
    (when changed
      (touch-project! project)
      (project-changed! project)
      (dolist (command changed)
        (let ((name (ingot::ref-command-name command))
              (old (ingot::ref-command-old command))
              (new (ingot::ref-command-new command)))
          (unless (string= new ingot::*zero-oid*)
            (when (or (starts-with? "refs/heads/" name) (starts-with? "refs/tags/" name))
              (handler-case (start-pipeline-for-push! project user name new)
                (error (e) (log-line "pipeline for ~a ~a: ~a" (getf project :path-with-namespace) name e)))))
          (enqueue-hook! (let ((project project) (user user) (name name) (old old) (new new))
                           (lambda () (deliver-push-hooks! project user name old new))))))
      (enqueue-hook! (let ((project project)) (lambda () (push-mirrors! project)))))))

(defun start-pipeline-for-push! (project user ref new)
  "A pipeline for the commit NEW on REF when the project's CI is on and
the tree carries a specification."
  (when (getf project :ci-enabled? t)
    (run-pipeline! project new :ref ref :source :push :user user :quiet? t)))

;;; Webhooks, in GitLab's shapes

(defun hook-user-json (user)
  (if user
      (json-table "id" (getf user :id) "name" (user-display user) "username" (getf user :username)
                  "email" (getf user :email))
      (json-table "id" 0 "name" *site-name* "username" "anvil")))

(defun hook-project-json (project)
  (json-table "id" (getf project :id) "name" (getf project :name)
              "description" (getf project :description)
              "web_url" (project-web-url project)
              "git_http_url" (project-clone-url project)
              "namespace" (project-namespace-path project)
              "visibility_level" (ecase (getf project :visibility) (:private 0) (:internal 10) (:public 20))
              "path_with_namespace" (getf project :path-with-namespace)
              "default_branch" (getf project :default-branch)
              "homepage" (project-web-url project)
              "url" (project-clone-url project)
              "http_url" (project-clone-url project)))

(defun commit-json-for-hook (project commit)
  (json-table "id" (ingot:commit-oid commit)
              "message" (ingot:commit-message commit)
              "title" (ingot:commit-subject commit)
              "timestamp" (iso-timestamp (git-time-universal (ingot:commit-author commit)))
              "url" (format nil "~a/-/commit/~a" (project-web-url project) (ingot:commit-oid commit))
              "author" (json-table "name" (ingot:commit-author-name commit) "email" (ingot:commit-author-email commit))))

(defun deliver-push-hooks! (project user ref old new)
  (let* ((hooks (remove-if-not (lambda (h) (member (if (starts-with? "refs/tags/" ref) "tag_push" "push")
                                                   (getf h :events) :test #'string=))
                               (getf project :hooks))))
    (when hooks
      (let* ((repository (project-repository project))
             (commits (if (string= new ingot::*zero-oid*)
                          nil
                          (handler-case (ingot:log-commits repository new :limit 20)
                            (error () nil))))
             (body (json-table "object_kind" (if (starts-with? "refs/tags/" ref) "tag_push" "push")
                               "event_name" (if (starts-with? "refs/tags/" ref) "tag_push" "push")
                               "before" old "after" new "ref" ref
                               "checkout_sha" (if (string= new ingot::*zero-oid*) :null new)
                               "user_id" (and user (getf user :id)) "user_name" (and user (user-display user))
                               "user_username" (and user (getf user :username))
                               "project_id" (getf project :id)
                               "project" (hook-project-json project)
                               "repository" (json-table "name" (getf project :name) "url" (project-clone-url project)
                                                        "homepage" (project-web-url project))
                               "commits" (coerce (mapcar (lambda (c) (commit-json-for-hook project c)) commits) 'vector)
                               "total_commits_count" (length commits))))
        (dolist (hook hooks)
          (deliver-hook! project hook (if (starts-with? "refs/tags/" ref) "Tag Push Hook" "Push Hook") body))))))

(defun deliver-hook! (project hook event body)
  "POST BODY (a table) to HOOK as GitLab does; the delivery's status is
kept on the hook."
  (let ((status nil) (note nil))
    (handler-case
        (multiple-value-bind (answer code)
            (http-post-json (getf hook :url) (json-string body)
                            :headers (append (list (cons "X-Gitlab-Event" event)
                                                   (cons "X-Anvil-Event" event))
                                             (when (getf hook :token) (list (cons "X-Gitlab-Token" (getf hook :token))))))
          (declare (ignore answer))
          (setq status code))
      (error (e) (setq note (princ-to-string e))))
    (let ((current (find-project (getf project :id))))
      (when current
        (update-record! current :hooks
                        (mapcar (lambda (h) (if (eql (getf h :id) (getf hook :id))
                                                (append (list :last-status status :last-note note :last-at (get-universal-time))
                                                        h)
                                                h))
                                (getf current :hooks)))))))

(defun http-post-json (url body &key headers (timeout *webhook-timeout*))
  "POST BODY (a string of JSON) to URL with AllegroServe's client, in
process.  Values: the answer's text and its status."
  (multiple-value-bind (answer status)
      (net.aserve.client:do-http-request url
        :method :post :content body :content-type "application/json"
        :headers (cons (cons "User-Agent" (format nil "anvil/~a" *version*)) headers)
        :external-format :utf-8 :timeout timeout :keep-alive nil
        :ssl-args (let ((host (url-host url))) (and host (list :server-name host))))
    (values answer status)))

(defun http-get-json (url &key headers (timeout *webhook-timeout*))
  (multiple-value-bind (answer status)
      (net.aserve.client:do-http-request url
        :method :get :headers (cons (cons "User-Agent" (format nil "anvil/~a" *version*)) headers)
        :external-format :utf-8 :timeout timeout :keep-alive nil
        :ssl-args (let ((host (url-host url))) (and host (list :server-name host))))
    (values (and (stringp answer) (plusp (length answer)) (ignore-errors (parse-json answer))) status)))

(defun url-host (url)
  (let* ((start (search "://" url))
         (from (and start (+ start 3)))
         (end (and from (or (position-if (lambda (c) (member c '(#\/ #\: #\?))) url :start from) (length url)))))
    (and from (< from end) (subseq url from end))))

(defun deliver-thing-hooks! (project thing user action)
  "An issue's or merge request's webhooks (object_kind issue or
merge_request), with ACTION open, close, reopen, update, merge."
  (let* ((issue? (eq (getf thing :kind) :issue))
         (event-name (if issue? "issue" "merge_request"))
         (hooks (remove-if-not (lambda (h) (member event-name (getf h :events) :test #'string=)) (getf project :hooks))))
    (when hooks
      (let ((body (json-table "object_kind" event-name "event_type" event-name
                              "user" (hook-user-json user)
                              "project" (hook-project-json project)
                              "object_attributes"
                              (json-table "id" (getf thing :id) "iid" (getf thing :iid) "title" (getf thing :title)
                                          "description" (getf thing :description)
                                          "state" (status-text (getf thing :state))
                                          "action" action
                                          "url" (strcat *site-url* (thing-url project thing))
                                          "source_branch" (getf thing :source-branch)
                                          "target_branch" (getf thing :target-branch)
                                          "author_id" (getf thing :author-id)
                                          "created_at" (iso-timestamp (getf thing :created))))))
        (dolist (hook hooks)
          (enqueue-hook! (let ((hook hook)) (lambda () (deliver-hook! project hook (if issue? "Issue Hook" "Merge Request Hook") body)))))))))

(defun deliver-pipeline-hooks! (project pipeline)
  (let ((hooks (remove-if-not (lambda (h) (member "pipeline" (getf h :events) :test #'string=)) (getf project :hooks))))
    (when hooks
      (let ((body (json-table "object_kind" "pipeline"
                              "object_attributes" (json-table "id" (getf pipeline :id) "ref" (getf pipeline :ref)
                                                              "sha" (getf pipeline :sha)
                                                              "status" (status-text (getf pipeline :status))
                                                              "source" (status-text (getf pipeline :source))
                                                              "stages" (coerce (getf pipeline :stages) 'vector)
                                                              "created_at" (iso-timestamp (getf pipeline :created))
                                                              "duration" (pipeline-duration pipeline))
                              "project" (hook-project-json project)
                              "builds" (coerce (mapcar (lambda (job)
                                                         (json-table "id" (getf job :id) "stage" (getf job :stage) "name" (getf job :name)
                                                                     "status" (status-text (getf job :status))))
                                                       (pipeline-jobs pipeline))
                                               'vector))))
        (dolist (hook hooks)
          (enqueue-hook! (let ((hook hook)) (lambda () (deliver-hook! project hook "Pipeline Hook" body)))))))))

;;; Mirrors: a push of every branch and tag to another forge.  The
;;; client side of receive-pack over smart HTTP.

(defun push-mirrors! (project)
  (dolist (mirror (getf project :mirrors))
    (handler-case (push-mirror! project mirror)
      (error (e)
        (log-line "mirror ~a: ~a" (getf mirror :url) e)
        (note-mirror-status! project mirror (princ-to-string e))))))

(defun note-mirror-status! (project mirror note)
  (let ((current (find-project (getf project :id))))
    (when current
      (update-record! current :mirrors
                      (mapcar (lambda (m) (if (eql (getf m :id) (getf mirror :id))
                                              (append (list :last-note note :last-at (get-universal-time)) m)
                                              m))
                              (getf current :mirrors))))))

(defun push-mirror! (project mirror)
  "Push every branch and tag of the project to MIRROR's URL (https, with
its user and token as Basic credentials)."
  (let* ((repository (project-repository project))
         (url (string-right-trim "/" (getf mirror :url)))
         (url (if (ends-with? ".git" url) url (strcat url ".git")))
         (auth (and (getf mirror :user) (getf mirror :token)
                    (cons (getf mirror :user) (getf mirror :token))))
         (refs (append (ingot:refs repository "refs/heads/") (ingot:refs repository "refs/tags/"))))
    ;; what the other side has
    (multiple-value-bind (advert status)
        (net.aserve.client:do-http-request (format nil "~a/info/refs?service=git-receive-pack" url)
          :method :get :basic-authorization auth :format :binary :keep-alive nil :timeout 60
          :headers '(("User-Agent" . "git/anvil-mirror"))
          :ssl-args (let ((host (url-host url))) (and host (list :server-name host))))
      (unless (eql status 200) (error "the mirror answered ~a to the refs advertisement" status))
      (let* ((lines (multiple-value-bind (first rest) (ingot:read-pkt-lines (ingot::as-octets advert))
                      (declare (ignore first))
                      (ingot:read-pkt-lines (ingot::as-octets advert) :start rest)))
             (theirs (loop for line in lines
                           for text = (ingot::pkt-text line)
                           for nul = (position (code-char 0) text)
                           for clean = (if nul (subseq text 0 nul) text)
                           for space = (position #\Space clean)
                           when (and space (not (search "capabilities^{}" clean)))
                             collect (cons (subseq clean (1+ space)) (subseq clean 0 space))))
             (commands nil) (wants nil))
        (loop for (name . oid) in refs
              for current = (cdr (assoc name theirs :test #'string=))
              unless (equal current oid)
                do (push (format nil "~a ~a ~a" (or current ingot::*zero-oid*) oid name) commands)
                   (push oid wants))
        (when commands
          (let* ((haves (mapcar #'cdr theirs))
                 (oids (ingot:reachable-objects repository wants
                                                (remove-if-not (lambda (h) (ingot:object-exists? repository h)) haves)))
                 (pack (ingot::buffer-octets (ingot:write-pack repository oids)))
                 (buffer (ingot::make-buffer (+ 1024 (length pack))))
                 (first? t))
            (dolist (command (reverse commands))
              (ingot:write-pkt-line buffer (format nil "~a~a~%" command (if first? (format nil "~a report-status" (code-char 0)) "")))
              (setq first? nil))
            (ingot:write-pkt-flush buffer)
            (ingot::buffer-write buffer pack)
            (multiple-value-bind (answer status)
                (net.aserve.client:do-http-request (format nil "~a/git-receive-pack" url)
                  :method :post :basic-authorization auth :format :binary :keep-alive nil :timeout 300
                  :content (ingot::buffer-octets buffer) :content-type "application/x-git-receive-pack-request"
                  :headers '(("User-Agent" . "git/anvil-mirror") ("Accept" . "application/x-git-receive-pack-result"))
                  :ssl-args (let ((host (url-host url))) (and host (list :server-name host))))
              (unless (eql status 200) (error "the mirror answered ~a to the push" status))
              (let ((report (ingot:octets-to-string (ingot::as-octets answer))))
                (if (search "unpack ok" report)
                    (note-mirror-status! project mirror (format nil "pushed ~a" (plural (length commands) "ref")))
                    (error "the mirror refused the push: ~a" report))))))
        (unless commands (note-mirror-status! project mirror "up to date"))))))

;;; The project's view object hears of a change

(defvar *project-changed-hooks* nil "Functions of a project record, called after a push.")

(defun project-changed! (project)
  (dolist (hook *project-changed-hooks*)
    (handler-case (funcall hook project)
      (error (e) (log-line "project-changed hook: ~a" e)))))
