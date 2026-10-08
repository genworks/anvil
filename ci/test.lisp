;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

;;;
;;; ci/test.lisp: load Anvil on a COLD Gendl image and work it.
;;;
;;; Run by the pipeline inside genworks/gendl:devo-ccl or devo-sbcl, an
;;; image that has never seen this checkout: loads Ingot and Anvil from
;;; the checkout (a warning in their own files fails the job), runs
;;; Ingot's checks, starts the forge on a port of its own with its state
;;; in a scratch directory, and then asks it, as a visitor and as a
;;; program would: the pages, the API's project, commit, issue and merge
;;; request flows, a push and a clone over the smart protocol with no
;;; git binary, a pipeline made from a ci.sexp, the OAuth2 flow.
;;;
;;; Locally, in the image, with ANVIL_DIR naming a checkout:
;;;   cd /opt/gendl && ANVIL_DIR=/path/to/anvil \
;;;     ./gdl/program/gdl-ccl -n -b --load /path/to/anvil/ci/test.lisp
;;;

(in-package :gdl-user)

(gendl:start-gendl!)
(load-quicklisp)

(defparameter *anvil-dir*
  (uiop:ensure-directory-pathname
   (or (uiop:getenv "ANVIL_DIR") (uiop:getenv "CI_PROJECT_DIR")
       (error "Set ANVIL_DIR or CI_PROJECT_DIR to the anvil checkout."))))

(defparameter *test-port* 9097)
(defparameter *base* (format nil "http://127.0.0.1:~d" *test-port*))

(defvar *problems* nil)
(defvar *checks* 0)

(defun problem (control &rest arguments)
  (let ((text (apply #'format nil control arguments)))
    (push text *problems*)
    (format t "~&FAIL  ~a~%" text)
    nil))

(defun check (name ok?)
  (incf *checks*)
  (format t "~&~a ~a~%" (if ok? "ok  " "FAIL") name)
  (unless ok? (push name *problems*))
  ok?)

(defun finish ()
  (finish-output)
  (format t "~&~%~d check~:p, ~d problem~:p.~%" *checks* (length *problems*))
  (dolist (text (reverse *problems*)) (format t "  ~a~%" text))
  (finish-output)
  (uiop:quit (if *problems* 1 0)))

(format t "~&Cold load of anvil from ~a~%Image: ~a ~a, Gendl ~a~%"
        *anvil-dir* (lisp-implementation-type) (lisp-implementation-version) *gendl-version*)

;;; The load: the libraries first with their warnings muffled, then
;;; Ingot and Anvil, where every warning counts.

(handler-case
    (progn
      (uiop:enable-deferred-warnings-check)
      (asdf:load-asd (merge-pathnames "ingot/ingot.asd" *anvil-dir*))
      (asdf:load-asd (merge-pathnames "anvil.asd" *anvil-dir*))
      (handler-bind ((warning #'muffle-warning))
        (ql:quickload (remove-if-not #'stringp
                                     (mapcar (lambda (d) (if (symbolp d) (string-downcase d) d))
                                             (append (asdf:system-depends-on (asdf:find-system :ingot))
                                                     (remove "ingot" (mapcar (lambda (d) (if (symbolp d) (string-downcase d) d))
                                                                             (asdf:system-depends-on (asdf:find-system :anvil)))
                                                             :test #'equal))))
                      :silent t))
      (let* ((*compile-verbose* nil) (*load-verbose* nil)
             (report (make-string-output-stream))
             (*error-output* (make-broadcast-stream *error-output* report)))
        (handler-bind ((warning (lambda (w)
                                  (problem "loading: ~a: ~a" (type-of w) w)
                                  (muffle-warning w))))
          (asdf:load-system :ingot)
          (asdf:load-system :anvil))
        (dolist (line (uiop:split-string (get-output-stream-string report) :separator '(#\Newline)))
          (let ((text (string-trim "; " line)))
            (when (and (plusp (length text))
                       (cl-ppcre:scan "(?i)undefined|undeclared|unused|warning" text)
                       (not (cl-ppcre:scan "(?i)^compiler warnings" text)))
              (problem "compiling: ~a" text))))))
  (error (e)
    (problem "loading: ERROR ~a: ~a" (type-of e) e)
    (finish)))

(format t "~&anvil loaded~:[~; with warnings~].~%" *problems*)

;;; Ingot's own checks

(handler-case (load (merge-pathnames "ingot/test/test.lisp" *anvil-dir*))
  (error (e) (problem "ingot checks: ~a" e)))

;;; The forge, on a port of its own, its state in a scratch directory

(defparameter *scratch* (merge-pathnames (format nil "anvil-ci-~d/" (random 1000000000)) (uiop:temporary-directory)))

(handler-case
    (progn
      (setf (symbol-value (find-symbol "*DATA-DIRECTORY*" :anvil)) *scratch*
            (symbol-value (find-symbol "*SITE-URL*" :anvil)) *base*
            (symbol-value (find-symbol "*HOSTS*" :anvil)) nil
            (symbol-value (find-symbol "*DOCKER-SOCKET*" :anvil)) nil
            (symbol-value (find-symbol "*DEPLOY-HOUSES*" :anvil))
            (list (list :key :test-house :name "test house" :lab-url (format nil "~a/no-such-lab" *base*)
                        :secret "test-secret-test-secret" :domain "example.test")))
      (uiop:symbol-call :anvil :start! :port *test-port*))
  (error (e) (problem "starting: ~a" e) (finish)))

;;; Asking it

(defvar *cookie* nil "The session cookie after a sign-in.")

(defun ask (path &key (method :get) content content-type token headers (accept "*/*") cookie? basic)
  "Values: the body (a string), the status, the headers."
  (handler-case
      (multiple-value-bind (body status response-headers)
          (net.aserve.client:do-http-request (format nil "~a~a" *base* path)
            :method method :content content :content-type content-type
            :accept accept :keep-alive nil :timeout 60
            :basic-authorization basic
            :headers (append headers
                             (when token (list (cons "PRIVATE-TOKEN" token)))
                             (when (and cookie? *cookie*) (list (cons "Cookie" *cookie*))))
            :external-format :utf-8)
        (values (if (stringp body) body (and body (babel:octets-to-string body :encoding :utf-8 :errorp nil))) status response-headers))
    (error (e) (values (format nil "~a" e) nil nil))))

(defun ask-octets (path &key (method :get) content content-type basic headers)
  (handler-case
      (multiple-value-bind (body status)
          (net.aserve.client:do-http-request (format nil "~a~a" *base* path)
            :method method :content content :content-type content-type :keep-alive nil :timeout 60
            :basic-authorization basic :headers headers :format :binary)
        (values (ingot:as-octets body) status))
    (error (e) (values (format nil "~a" e) nil))))

(defun json (text) (ignore-errors (yason:parse text :json-arrays-as-vectors t)))
(defun jget (table key) (and (hash-table-p table) (gethash key table)))

(defun status-ok? (path &optional (expected 200))
  (multiple-value-bind (body status) (ask path)
    (declare (ignore body))
    (check (format nil "GET ~a -> ~a" path expected) (eql status expected))))

;; a user, a token, a project
(defvar *user* (uiop:symbol-call :anvil :make-user! "tester" "Tester" "tester@example.test" "password-of-tester" :admin? t))
(defvar *token* (uiop:symbol-call :anvil :make-token! *user* "ci" '("api")))
(defvar *second* (uiop:symbol-call :anvil :make-user! "other" "Other" "other@example.test" "password-of-other"))
(defvar *second-token* (uiop:symbol-call :anvil :make-token! *second* "ci" '("api")))

(check "the home page" (eql 200 (nth-value 1 (ask "/"))))
(check "explore" (eql 200 (nth-value 1 (ask "/explore"))))
(check "sign-in page" (eql 200 (nth-value 1 (ask "/users/sign_in"))))
(check "help" (eql 200 (nth-value 1 (ask "/help"))))
(check "the stylesheet" (eql 200 (nth-value 1 (ask "/static/anvil.css"))))
(check "API version" (let ((v (json (ask "/api/v4/version")))) (and v (stringp (jget v "version")))))
(check "API user with a token" (equal "tester" (jget (json (ask "/api/v4/user" :token *token*)) "username")))
(check "API user without a token is 401" (eql 401 (nth-value 1 (ask "/api/v4/user"))))

(let* ((answer (ask "/api/v4/projects" :method :post :token *token*
                                      :content "name=Smoke&path=smoke&description=A+smoke+project&initialize_with_readme=true&visibility=public"
                                      :content-type "application/x-www-form-urlencoded"))
       (project (json answer)))
  (check "a project made through the API" (equal "tester/smoke" (jget project "path_with_namespace")))
  (check "the project page" (eql 200 (nth-value 1 (ask "/tester/smoke"))))
  (check "the project in the list" (find "tester/smoke" (coerce (or (json (ask "/api/v4/projects?search=smoke")) #()) 'list)
                                        :key (lambda (p) (jget p "path_with_namespace")) :test #'equal))
  (check "the tree" (let ((tree (json (ask "/api/v4/projects/tester%2Fsmoke/repository/tree"))))
                      (and (vectorp tree) (find "README.md" (coerce tree 'list) :key (lambda (e) (jget e "name")) :test #'equal))))
  (check "a raw file" (search "# Smoke" (ask "/api/v4/projects/1/repository/files/README.md/raw?ref=master")))
  (check "the blob page" (eql 200 (nth-value 1 (ask "/tester/smoke/-/blob/master/README.md"))))
  (check "the commits page" (eql 200 (nth-value 1 (ask "/tester/smoke/-/commits/master"))))
  ;; a commit through the API, on a new branch
  (let ((commit (json (ask "/api/v4/projects/1/repository/commits" :method :post :token *token*
                                                                    :content-type "application/json"
                                                                    :content "{\"branch\":\"feature\",\"start_branch\":\"master\",\"commit_message\":\"Add notes\",\"actions\":[{\"action\":\"create\",\"file_path\":\"NOTES.md\",\"content\":\"# Notes\\n\"},{\"action\":\"update\",\"file_path\":\"README.md\",\"content\":\"# Smoke\\n\\nEdited.\\n\"}]}"))))
    (check "a commit through the API" (equal "Add notes" (jget commit "title")))
    (check "the branch exists" (find "feature" (coerce (or (json (ask "/api/v4/projects/1/repository/branches")) #()) 'list)
                                     :key (lambda (b) (jget b "name")) :test #'equal))
    (check "the commit page" (eql 200 (nth-value 1 (ask (format nil "/tester/smoke/-/commit/~a" (jget commit "id"))))))
    (check "the compare page" (eql 200 (nth-value 1 (ask "/tester/smoke/-/compare?from=master&to=feature"))))
    ;; an issue, a note
    (let ((issue (json (ask "/api/v4/projects/1/issues" :method :post :token *token* :content-type "application/json"
                                                         :content "{\"title\":\"An issue\",\"description\":\"With **markdown**\",\"labels\":\"bug\"}"))))
      (check "an issue" (eql 1 (jget issue "iid")))
      (check "a note" (equal "Noted." (jget (json (ask "/api/v4/projects/1/issues/1/notes" :method :post :token *token*
                                                                                              :content "body=Noted." :content-type "application/x-www-form-urlencoded"))
                                            "body")))
      (check "the issue page renders the markdown" (search "<strong>markdown</strong>" (ask "/tester/smoke/-/issues/1")))
      (check "the issue closes" (equal "closed" (jget (json (ask "/api/v4/projects/1/issues/1" :method :put :token *token*
                                                                                               :content "state_event=close" :content-type "application/x-www-form-urlencoded"))
                                                      "state")))
      (check "a stranger may not close an issue" (eql 403 (nth-value 1 (ask "/api/v4/projects/1/issues/1" :method :put :token *second-token*
                                                                                                           :content "state_event=reopen" :content-type "application/x-www-form-urlencoded")))))
    ;; a merge request, merged
    (let ((mr (json (ask "/api/v4/projects/1/merge_requests" :method :post :token *token*
                                                              :content "source_branch=feature&target_branch=master&title=Feature" :content-type "application/x-www-form-urlencoded"))))
      (check "a merge request" (equal "can_be_merged" (jget mr "merge_status")))
      (check "the merge request page" (eql 200 (nth-value 1 (ask "/tester/smoke/-/merge_requests/1"))))
      (check "a stranger may not merge" (eql 403 (nth-value 1 (ask "/api/v4/projects/1/merge_requests/1/merge" :method :put :token *second-token*))))
      (let ((merged (json (ask "/api/v4/projects/1/merge_requests/1/merge" :method :put :token *token*))))
        (check "merged" (equal "merged" (jget merged "state")))
        (check "the merge is on master" (search "# Notes" (ask "/api/v4/projects/1/repository/files/NOTES.md/raw?ref=master")))
        (check "the merge commit has two parents"
               (= 2 (length (jget (json (ask (format nil "/api/v4/projects/1/repository/commits/~a" (jget merged "merge_commit_sha")))) "parent_ids")))))))
  ;; archives
  (check "a tar.gz archive" (let ((octets (ask-octets "/tester/smoke/-/archive/master/smoke-master.tar.gz")))
                              (and (vectorp octets) (> (length octets) 100) (= (aref octets 0) #x1f))))
  (check "a zip archive" (let ((octets (ask-octets "/api/v4/projects/1/repository/archive.zip")))
                           (and (vectorp octets) (= (aref octets 0) #x50)))))

;;; Git over HTTP, with no git binary: a push from a repository made
;;; with Ingot, and a clone back.

(let* ((root (merge-pathnames "local-repo.git/" *scratch*))
       (local (ingot:init-repository root))
       (who (ingot:make-identity "Tester" "tester@example.test"))
       (c1 (ingot:make-commit! local :branch "master" :author who :message "Pushed from Ingot"
                                     :actions '((:action :create :path "pushed.txt" :content "pushed\n")
                                                (:action :create :path "ci.sexp"
                                                 :content "(:pipeline :stages (:test) :jobs ((:check :stage :test :image \"alpine:3\" :script (\"echo hello\"))))\n"))))
       (project (json (ask "/api/v4/projects" :method :post :token *token*
                                             :content "name=Pushed&path=pushed&visibility=public" :content-type "application/x-www-form-urlencoded"))))
  (check "an empty project for the push" (equal "pushed" (jget project "path")))
  (check "the empty project's page" (eql 200 (nth-value 1 (ask "/tester/pushed"))))
  (let* ((advert (ask "/tester/pushed.git/info/refs?service=git-receive-pack" :basic (cons "tester" *token*)))
         (oids (ingot:reachable-objects local (list c1) nil))
         (pack (ingot:buffer-octets (ingot:write-pack local oids)))
         (request (let ((b (ingot:make-buffer)))
                    (ingot:write-pkt-line b (format nil "~a ~a refs/heads/master~a report-status~%" ingot:*zero-oid* c1 (code-char 0)))
                    (ingot:write-pkt-flush b)
                    (ingot:buffer-write b pack)
                    (ingot:buffer-octets b))))
    (check "the receive-pack advertisement" (and (stringp advert) (search "# service=git-receive-pack" advert) (search "report-status" advert)))
    (check "a push without credentials is 401" (eql 401 (nth-value 1 (ask-octets "/tester/pushed.git/git-receive-pack" :method :post :content request
                                                                                                                          :content-type "application/x-git-receive-pack-request"))))
    (multiple-value-bind (answer status)
        (ask-octets "/tester/pushed.git/git-receive-pack" :method :post :content request
                                                           :content-type "application/x-git-receive-pack-request" :basic (cons "tester" *token*))
      (check "the push is taken" (and (eql status 200) (search (ingot:string-to-octets "unpack ok") answer) (search (ingot:string-to-octets "ok refs/heads/master") answer)))))
  (check "the pushed file shows" (search "pushed" (ask "/api/v4/projects/2/repository/files/pushed.txt/raw?ref=master")))
  ;; the clone
  (let* ((advert (ask "/tester/pushed.git/info/refs?service=git-upload-pack"))
         (request (let ((b (ingot:make-buffer)))
                    (ingot:write-pkt-line b (format nil "want ~a multi_ack_detailed side-band-64k~%" c1))
                    (ingot:write-pkt-flush b)
                    (ingot:write-pkt-line b (format nil "done~%"))
                    (ingot:buffer-octets b))))
    (check "the upload-pack advertisement names the push" (and (stringp advert) (search c1 advert)))
    (multiple-value-bind (answer status)
        (ask-octets "/tester/pushed.git/git-upload-pack" :method :post :content request :content-type "application/x-git-upload-pack-request")
      (check "the clone answers a pack"
             (and (eql status 200)
                  (let ((received (ingot:make-buffer)) (at 8))
                    (loop
                      (multiple-value-bind (line next) (ingot::read-pkt-line answer at)
                        (setq at next)
                        (cond ((or (null line) (eq line :flush)) (return))
                              ((= (aref line 0) 1) (ingot:buffer-write received line :start 1)))))
                    (let ((objects (ingot:index-pack (ingot:buffer-octets received) (constantly nil))))
                      (and (= (length objects) 4)   ; a commit, a tree, two blobs
                           (find c1 objects :key #'ingot:object-oid :test #'string=))))))))
  ;; the pipeline the push started: no executor here, so it fails, saying why
  (sleep 3)
  (let* ((pipelines (json (ask "/api/v4/projects/2/pipelines")))
         (pipeline (and (vectorp pipelines) (plusp (length pipelines)) (aref pipelines 0))))
    (check "a pipeline was made for the push" (and pipeline (equal "master" (jget pipeline "ref"))))
    (when pipeline
      (let* ((jobs-text (ask (format nil "/api/v4/projects/2/pipelines/~d/jobs" (jget pipeline "id"))))
             (jobs (json jobs-text))
             (job (and (vectorp jobs) (plusp (length jobs)) (aref jobs 0))))
        (unless (check "the job is there" (and job (equal "check" (jget job "name"))))
          (format t "      pipeline: ~a~%      jobs: ~a~%" (ask (format nil "/api/v4/projects/2/pipelines/~d" (jget pipeline "id"))) jobs-text))
        (check "the job failed for want of an executor" (and job (equal "failed" (jget job "status"))
                                                             (search "executor" (or (jget job "failure_reason") ""))))
        (check "the pipeline page" (eql 200 (nth-value 1 (ask (format nil "/tester/pushed/-/pipelines/~d" (jget pipeline "id"))))))
        (when job
          (check "the job page" (eql 200 (nth-value 1 (ask (format nil "/tester/pushed/-/jobs/~d" (jget job "id"))))))
          (check "the trace" (stringp (ask (format nil "/api/v4/projects/2/jobs/~d/trace" (jget job "id"))))))))))

;;; The YAML reader on a real .gitlab-ci.yml

(let ((spec (handler-case (uiop:symbol-call :anvil :read-gitlab-ci "
stages: [build, test, deploy]
variables:
  GIT_DEPTH: \"1\"
default:
  image: alpine:3
.base: &base
  before_script:
    - echo before
build:
  <<: *base
  stage: build
  script:
    - echo build
  artifacts:
    paths: [out/]
    expire_in: 1 week
test:
  extends: .base
  stage: test
  script: |
    echo multi
    echo line
  rules:
    - if: '$CI_COMMIT_BRANCH == \"master\" && $DEPLOY != \"off\"'
      when: always
    - when: never
deploy:
  stage: deploy
  script: [\"echo deploy\"]
  only:
    - tags
  tags: [docker, special]
")
                (error (e) (problem "yaml: ~a" e) nil))))
  (check "a .gitlab-ci.yml compiles" (and spec (= 3 (length (getf spec :jobs)))))
  (when spec
    (let ((context (list :branch "master" :tag nil :ref "master" :source :push :default-branch "master" :paths nil :changed-paths :unknown
                         :variables (list (cons "CI_COMMIT_BRANCH" "master")))))
      (let ((chosen (uiop:symbol-call :anvil :jobs-for-context spec context)))
        (check "rules and only choose the jobs for a branch push"
               (equal '("build" "test") (mapcar (lambda (j) (getf j :name)) chosen)))
        (check "the anchor and the merge key reached the job"
               (equal '("echo before") (getf (find "build" chosen :key (lambda (j) (getf j :name)) :test #'string=) :before-script)))
        (check "a literal block scalar kept its lines"
               (equal '("echo multi" "echo line") (ingot:split-lines (first (getf (find "test" chosen :key (lambda (j) (getf j :name)) :test #'string=) :script)))))
        (check "artifacts expire_in is read" (equal "1 week" (getf (getf (find "build" chosen :key (lambda (j) (getf j :name)) :test #'string=) :artifacts) :expire-in))))
      (check "a tag push chooses build (no rules) and deploy (only tags), not test"
             (equal '("build" "deploy") (mapcar (lambda (j) (getf j :name))
                                        (uiop:symbol-call :anvil :jobs-for-context spec
                                                          (list :branch nil :tag "v1" :ref "v1" :source :push :default-branch "master" :paths nil :changed-paths :unknown :variables nil))))))))

;;; Signing in, and OAuth2 with PKCE

(multiple-value-bind (body status headers) (ask "/users/sign_in" :method :post :content "login=tester&password=password-of-tester" :content-type "application/x-www-form-urlencoded")
  (declare (ignore body))
  (let ((cookie (cdr (assoc :set-cookie headers))))
    (setq *cookie* (and cookie (subseq cookie 0 (position #\; cookie))))
    (check "sign in sets the session" (and (eql status 303) *cookie*))))
(check "the dashboard, signed in" (eql 200 (nth-value 1 (ask "/dashboard" :cookie? t))))
(check "the tokens page, signed in" (eql 200 (nth-value 1 (ask "/-/profile/tokens" :cookie? t))))
(check "the settings page, signed in" (eql 200 (nth-value 1 (ask "/tester/smoke/-/settings" :cookie? t))))
(check "the deployments page, signed in" (eql 200 (nth-value 1 (ask "/tester/smoke/-/deployments" :cookie? t))))
(check "the admin page, signed in" (eql 200 (nth-value 1 (ask "/admin" :cookie? t))))
(check "a web edit commits" (eql 303 (nth-value 1 (ask "/tester/smoke/-/edit/master/README.md" :method :post :cookie? t
                                                       :content "content=%23+Smoke%0A%0AEdited+on+the+web.%0A&message=Web+edit" :content-type "application/x-www-form-urlencoded"))))
(check "the web edit is on master" (search "Edited on the web" (ask "/api/v4/projects/1/repository/files/README.md/raw?ref=master")))

(multiple-value-bind (app secret) (uiop:symbol-call :anvil :make-oauth-app! "The gate" (list (format nil "~a/callback" *base*)))
  (let* ((verifier "a-verifier-of-some-length-for-pkce-s256-0123456789")
         (challenge (uiop:symbol-call :anvil :base64url (ingot:sha256-digest (ingot:string-to-octets verifier))))
         (authorize (format nil "/oauth/authorize?client_id=~a&redirect_uri=~a&response_type=code&scope=api&state=xyz&code_challenge=~a&code_challenge_method=S256"
                            (getf app :client-id) (net.aserve:uriencode-string (format nil "~a/callback" *base*)) challenge)))
    (check "the consent page" (eql 200 (nth-value 1 (ask authorize :cookie? t))))
    (multiple-value-bind (body status headers)
        (ask "/oauth/authorize" :method :post :cookie? t :content-type "application/x-www-form-urlencoded"
                                :content (format nil "client_id=~a&redirect_uri=~a&response_type=code&scope=api&state=xyz&code_challenge=~a&code_challenge_method=S256&decision=allow"
                                                 (getf app :client-id) (net.aserve:uriencode-string (format nil "~a/callback" *base*)) challenge))
      (declare (ignore body))
      (let* ((location (cdr (assoc :location headers)))
             (code (and location (cl-ppcre:register-groups-bind (c) ("code=([0-9a-f]+)" location) c))))
        (check "consent redirects with a code" (and (eql status 303) code (search "state=xyz" location)))
        (let ((token-answer (json (ask "/oauth/token" :method :post :content-type "application/x-www-form-urlencoded"
                                                      :content (format nil "grant_type=authorization_code&code=~a&redirect_uri=~a&client_id=~a&client_secret=~a&code_verifier=~a"
                                                                       code (net.aserve:uriencode-string (format nil "~a/callback" *base*)) (getf app :client-id) secret verifier)))))
          (check "the code becomes a token" (stringp (jget token-answer "access_token")))
          (check "the OAuth token names its user"
                 (equal "tester" (jget (json (ask "/api/v4/user" :headers (list (cons "Authorization" (format nil "Bearer ~a" (jget token-answer "access_token")))))) "username")))
          (check "a spent code is refused" (eql 400 (nth-value 1 (ask "/oauth/token" :method :post :content-type "application/x-www-form-urlencoded"
                                                                      :content (format nil "grant_type=authorization_code&code=~a&redirect_uri=~a&client_id=~a&client_secret=~a&code_verifier=~a"
                                                                                       code (net.aserve:uriencode-string (format nil "~a/callback" *base*)) (getf app :client-id) secret verifier))))))))))

;;; Permissions

(check "a private project is hidden from a stranger"
       (progn (ask "/api/v4/projects" :method :post :token *token* :content "name=Secret&path=secret&visibility=private" :content-type "application/x-www-form-urlencoded")
              (and (eql 404 (nth-value 1 (ask "/api/v4/projects/tester%2Fsecret" :token *second-token*)))
                   (eql 200 (nth-value 1 (ask "/api/v4/projects/tester%2Fsecret" :token *token*))))))
(check "a stranger may not push" (eql 403 (nth-value 1 (ask "/api/v4/projects/1/repository/commits" :method :post :token *second-token* :content-type "application/json"
                                                           :content "{\"branch\":\"master\",\"commit_message\":\"x\",\"actions\":[{\"action\":\"create\",\"file_path\":\"x\",\"content\":\"x\"}]}"))))

;;; The deployment door, against a house that is not there

(check "the profile report finds the files missing"
       (let ((report (uiop:symbol-call :anvil :profile-report (uiop:symbol-call :anvil :find-project "tester/smoke"))))
         (and (not (getf report :image?)) (getf report :faults))))

(ignore-errors (uiop:symbol-call :anvil :stop!))
(ignore-errors (uiop:delete-directory-tree *scratch* :validate t))
(finish)
