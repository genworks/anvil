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
;;; image that has never seen this checkout: loads Ingot (a repository
;;; of its own: INGOT_DIR, else the checkout beside this one, else
;;; ingot/ inside it, where the pipeline unpacks it) and Anvil from the
;;; checkout (a warning in their own files fails the job), runs
;;; Ingot's checks, starts the forge on a port of its own with its state
;;; in a scratch directory, and then asks it, as a visitor and as a
;;; program would: the pages, the API's project, commit, issue and merge
;;; request flows, a push and a clone over the smart protocol with no
;;; git binary, a pipeline made from a ci.sexp, the OAuth2 flow.
;;;
;;; Locally, in the image, with ANVIL_DIR naming a checkout:
;;;   cd /opt/gendl && ANVIL_DIR=/path/to/anvil INGOT_DIR=/path/to/ingot \
;;;     ./gdl/program/gdl-ccl -n -b --load /path/to/anvil/ci/test.lisp
;;;

(in-package :gdl-user)

(gendl:start-gendl!)
(load-quicklisp)

(defparameter *anvil-dir*
  (uiop:ensure-directory-pathname
   (or (uiop:getenv "ANVIL_DIR") (uiop:getenv "CI_PROJECT_DIR")
       (error "Set ANVIL_DIR or CI_PROJECT_DIR to the anvil checkout."))))

(defparameter *ingot-dir*
  (uiop:ensure-directory-pathname
   (or (let ((env (uiop:getenv "INGOT_DIR"))) (and env (plusp (length env)) env))
       (let ((beside (merge-pathnames "../ingot/" *anvil-dir*)))
         (and (probe-file (merge-pathnames "ingot.asd" beside)) beside))
       (merge-pathnames "ingot/" *anvil-dir*)))
  "Where Ingot is: INGOT_DIR, else the checkout beside this one, else
ingot/ inside it, where the pipeline unpacks it.")

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
      (asdf:load-asd (merge-pathnames "ingot.asd" *ingot-dir*))
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

(handler-case (load (merge-pathnames "test/test.lisp" *ingot-dir*))
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
(check "robots.txt keeps crawlers out of the history"
       (multiple-value-bind (body status) (ask "/robots.txt")
         (and (eql status 200) (search "User-agent: *" body) (search "Disallow: /*/-/commit/" body) (search "Disallow: /*/-/blob/" body))))
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
    ;; the commit's patch, both ways, and the feed
    (check "the commit as .diff" (let ((text (ask (format nil "/tester/smoke/-/commit/~a.diff" (jget commit "id")))))
                                    (and (search "diff --git a/NOTES.md b/NOTES.md" text) (search "+# Notes" text) (not (search "Subject:" text)))))
    (check "the commit as .patch" (let ((text (ask (format nil "/tester/smoke/-/commit/~a.patch" (jget commit "id")))))
                                     (and (search (format nil "From ~a Mon Sep 17 00:00:00 2001" (jget commit "id")) text)
                                          (search "Subject: [PATCH] Add notes" text) (search " create mode 100644 NOTES.md" text))))
    (check "the commits feed" (let ((text (ask "/tester/smoke/-/commits/feature?format=atom")))
                                (and (search "<feed xmlns=\"http://www.w3.org/2005/Atom\">" text) (search "<title>Add notes</title>" text))))
    ;; blame, on the page and through the API
    (check "the blame page" (let ((body (ask "/tester/smoke/-/blame/feature/README.md")))
                              (and (search "Add notes" body) (search "Edited." body) (search "class=\"code blame\"" body))))
    (check "blame through the API" (let ((runs (json (ask "/api/v4/projects/1/repository/files/README.md/blame?ref=feature"))))
                                     (and (vectorp runs) (= (length runs) 2)
                                          (search "Add notes" (jget (jget (aref runs 1) "commit") "message"))
                                          (equal "# Smoke" (aref (jget (aref runs 0) "lines") 0))
                                          (find "Edited." (coerce (jget (aref runs 1) "lines") 'list) :test #'equal))))
    ;; the file finder and the search
    (check "the file finder" (let ((body (ask "/tester/smoke/-/find_file/feature"))) (and (search "NOTES.md" body) (search "data-filter" body))))
    (check "the search page finds a line of code" (search "NOTES.md" (ask "/tester/smoke/-/search?search=Notes&scope=blobs&ref=feature")))
    (check "search through the API: blobs" (let ((hits (json (ask "/api/v4/projects/1/search?scope=blobs&search=notes&ref=feature"))))
                                              (and (vectorp hits) (= (length hits) 1) (equal "NOTES.md" (jget (aref hits 0) "path")) (eql 1 (jget (aref hits 0) "startline")))))
    (check "search through the API: commits" (= 1 (length (json (ask "/api/v4/projects/1/search?scope=commits&search=add+notes&ref=feature")))))
    (check "search through the API: a global search for the project" (= 1 (length (json (ask "/api/v4/search?scope=projects&search=smoke")))))
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
  ;; languages, from a commit with code in it
  (check "no languages in prose alone" (zerop (hash-table-count (json (ask "/api/v4/projects/1/languages")))))
  (let ((commit (json (ask "/api/v4/projects/1/repository/commits" :method :post :token *token*
                                                                    :content-type "application/json"
                                                                    :content "{\"branch\":\"master\",\"commit_message\":\"Add code\",\"actions\":[{\"action\":\"create\",\"file_path\":\"src/a.lisp\",\"content\":\"(defun a () 1)\\n\"},{\"action\":\"create\",\"file_path\":\"run.sh\",\"content\":\"echo hi\\n\"}]}"))))
    (check "a commit of code" (equal "Add code" (jget commit "title")))
    (let ((languages (json (ask "/api/v4/projects/1/languages"))))
      (check "languages by bytes, as percentages" (and (= 2 (hash-table-count languages))
                                                       (> (jget languages "Common Lisp") (jget languages "Shell"))
                                                       (= 100 (round (+ (jget languages "Common Lisp") (jget languages "Shell"))))))
      (check "the languages bar on the project page" (search "class=\"lang-bar\"" (ask "/tester/smoke")))))
  ;; releases: a tag made with the release, notes written from the commits, the badges
  (let ((release (json (ask "/api/v4/projects/1/releases" :method :post :token *token*
                                                           :content "tag_name=v0.1&name=First+release&ref=master" :content-type "application/x-www-form-urlencoded"))))
    (check "a release through the API" (and (equal "v0.1" (jget release "tag_name")) (equal "First release" (jget release "name"))))
    (check "the release's notes name the commits" (and (search "### Changes" (jget release "description")) (search "Add code (" (jget release "description"))))
    (check "the release made the tag" (find "v0.1" (coerce (json (ask "/api/v4/projects/1/repository/tags")) 'list) :key (lambda (tag) (jget tag "name")) :test #'equal))
    (check "the release listed" (= 1 (length (json (ask "/api/v4/projects/1/releases")))))
    (check "one release by its tag" (equal "First release" (jget (json (ask "/api/v4/projects/1/releases/v0.1")) "name")))
    (check "a second release of the tag is refused" (eql 400 (nth-value 1 (ask "/api/v4/projects/1/releases" :method :post :token *token*
                                                                                  :content "tag_name=v0.1" :content-type "application/x-www-form-urlencoded"))))
    (check "a stranger may not release" (eql 403 (nth-value 1 (ask "/api/v4/projects/1/releases" :method :post :token *second-token*
                                                                      :content "tag_name=v0.2&ref=master" :content-type "application/x-www-form-urlencoded"))))
    (check "the releases page" (let ((body (ask "/tester/smoke/-/releases"))) (and (search "First release" body) (search "/-/archive/v0.1/smoke-v0-1.tar.gz" body))))
    (check "the release page" (eql 200 (nth-value 1 (ask "/tester/smoke/-/releases/v0.1"))))
    (check "the new release form" (eql 200 (nth-value 1 (ask "/tester/smoke/-/releases/new" :token *token*))))
    (check "the pipeline badge" (let ((svg (ask "/tester/smoke/badges/master/pipeline.svg")))
                                  (and (search "<svg" svg) (search ">pipeline<" svg) (search ">unknown<" svg))))
    (check "the release badge names the release" (search ">v0.1<" (ask "/tester/smoke/badges/master/release.svg")))
    ;; assets: a file uploaded, served and counted; a link added
    (let* ((boundary "----anvilsmoke")
           (body (format nil "--~a~c~cContent-Disposition: form-data; name=\"file\"; filename=\"notes.txt\"~c~cContent-Type: text/plain~c~c~c~cthe notes~c~c--~a--~c~c"
                         boundary #\Return #\Newline #\Return #\Newline #\Return #\Newline #\Return #\Newline #\Return #\Newline boundary #\Return #\Newline))
           (asset (json (ask "/api/v4/projects/1/releases/v0.1/assets" :method :post :token *token*
                                                                         :content body :content-type (format nil "multipart/form-data; boundary=~a" boundary)))))
      (check "an asset uploaded" (and (equal "notes.txt" (jget asset "name")) (eql 9 (jget asset "size"))))
      (check "the asset downloads" (equal "the notes" (ask "/tester/smoke/-/releases/v0.1/downloads/notes.txt")))
      (check "the download counted, the asset among the release's links"
             (let* ((links (coerce (jget (jget (json (ask "/api/v4/projects/1/releases/v0.1")) "assets") "links") 'list))
                    (mine (find "notes.txt" links :key (lambda (l) (jget l "name")) :test #'equal)))
               (and mine (eql 1 (jget mine "downloads")) (search "/downloads/notes.txt" (jget mine "direct_asset_url")))))
      (check "a link added" (let ((link (json (ask "/api/v4/projects/1/releases/v0.1/assets/links" :method :post :token *token*
                                                                                                     :content "name=Docs&url=https://example.org/docs&link_type=runbook" :content-type "application/x-www-form-urlencoded"))))
                              (and (equal "Docs" (jget link "name")) (equal "runbook" (jget link "link_type")))))
      (check "the links listed" (= 1 (length (json (ask "/api/v4/projects/1/releases/v0.1/assets/links")))))
      (check "the release page shows the asset and the link" (let ((page (ask "/tester/smoke/-/releases/v0.1")))
                                                               (and (search "notes.txt" page) (search "1 download" page) (search "example.org/docs" page))))))
  ;; milestones, GitLab's shape
  (let ((milestone (json (ask "/api/v4/projects/1/milestones" :method :post :token *token*
                                                               :content "title=M1&description=The+first&due_date=2027-01-31" :content-type "application/x-www-form-urlencoded"))))
    (check "a milestone made" (and (eql 1 (jget milestone "iid")) (equal "active" (jget milestone "state")) (equal "2027-01-31" (jget milestone "due_date"))))
    (check "a bad date is refused" (eql 400 (nth-value 1 (ask "/api/v4/projects/1/milestones" :method :post :token *token*
                                                               :content "title=M2&due_date=soon" :content-type "application/x-www-form-urlencoded"))))
    (check "an issue given the milestone" (equal "M1" (jget (jget (json (ask "/api/v4/projects/1/issues/1" :method :put :token *token*
                                                                                     :content (format nil "milestone_id=~d" (jget milestone "id")) :content-type "application/x-www-form-urlencoded"))
                                                                 "milestone") "title")))
    (check "the milestone's issues" (= 1 (length (json (ask (format nil "/api/v4/projects/1/milestones/~d/issues" (jget milestone "id")))))))
    (check "the milestones page" (let ((page (ask "/tester/smoke/-/milestones"))) (and (search "M1" page) (search "class=\"progress\"" page))))
    (check "the milestone page" (search "An issue" (ask "/tester/smoke/-/milestones/1")))
    (check "the issue page links its milestone" (search "/-/milestones/1" (ask "/tester/smoke/-/issues/1")))
    (check "the milestone closes" (equal "closed" (jget (json (ask (format nil "/api/v4/projects/1/milestones/~d" (jget milestone "id")) :method :put :token *token*
                                                                        :content "state_event=close" :content-type "application/x-www-form-urlencoded"))
                                                        "state")))
    (check "closed milestones listed apart" (and (= 0 (length (json (ask "/api/v4/projects/1/milestones?state=active"))))
                                                  (= 1 (length (json (ask "/api/v4/projects/1/milestones?state=closed")))))))
  ;; commit statuses: what an outside program says about a commit
  (let ((sha (jget (json (ask "/api/v4/projects/1/repository/commits/master")) "id")))
    (check "a status set" (let ((status (json (ask (format nil "/api/v4/projects/1/statuses/~a" sha) :method :post :token *token*
                                                       :content "state=success&name=ci/lint&target_url=https://ci.example.org/1&description=clean" :content-type "application/x-www-form-urlencoded"))))
                            (and (equal "success" (jget status "status")) (equal "ci/lint" (jget status "name")) (equal sha (jget status "sha")))))
    (check "a second report under the name replaces the first"
           (progn (ask (format nil "/api/v4/projects/1/statuses/~a" sha) :method :post :token *token*
                       :content "state=failed&name=ci/lint" :content-type "application/x-www-form-urlencoded")
                  (let ((statuses (json (ask (format nil "/api/v4/projects/1/repository/commits/~a/statuses" sha)))))
                    (and (= 1 (length statuses)) (equal "failed" (jget (aref statuses 0) "status"))))))
    (check "a bad state is refused" (eql 400 (nth-value 1 (ask (format nil "/api/v4/projects/1/statuses/~a" sha) :method :post :token *token*
                                                               :content "state=meh" :content-type "application/x-www-form-urlencoded"))))
    (check "a stranger may not set a status" (eql 403 (nth-value 1 (ask (format nil "/api/v4/projects/1/statuses/~a" sha) :method :post :token *second-token*
                                                                         :content "state=success" :content-type "application/x-www-form-urlencoded"))))
    (check "the commit page shows the check" (let ((page (ask (format nil "/tester/smoke/-/commit/~a" sha)))) (and (search "ci/lint" page) (search "Checks" page)))))
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
                                     :actions (list (list :action :create :path "pushed.txt" :content (format nil "pushed~%"))
                                                    (list :action :create :path "ci.sexp"
                                                          :content (format nil "(:pipeline :stages (:test) :jobs ((:check :stage :test :image \"alpine:3\" :script (\"echo hello\"))))~%")))))
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

;;; The client side: the pushed project fetched back through Ingot's
;;; own client over the forge's transport, and a project imported from
;;; the forge itself, which answers in GitLab's shapes.

(let ((fetched (ingot:init-repository (merge-pathnames "fetched-repo.git/" *scratch*)))
      (transport (uiop:symbol-call :anvil :http-git-transport (format nil "~a/tester/pushed.git" *base*))))
  (multiple-value-bind (set deleted received)
      (handler-case (ingot:fetch! fetched transport)
        (error (e) (problem "fetch: ~a" e) (values nil nil 0)))
    (check "a fetch through Ingot's client brings the push back" (and (= 1 (length set)) (null deleted) (= received 4)))
    (check "the fetched tree has the pushed file"
           (ignore-errors (search "pushed" (ingot:octets-to-string (ingot:blob-at fetched (ingot:head-oid fetched) "pushed.txt")))))
    (check "a second fetch brings nothing"
           (multiple-value-bind (set deleted received) (ingot:fetch! fetched transport)
             (and (null set) (null deleted) (zerop received))))))

(let ((reports (handler-case (uiop:symbol-call :anvil :import-from-gitlab! *base* :token *token* :projects '("tester/smoke") :into "imported")
                 (error (e) (problem "import: ~a" e) nil))))
  (check "the import reports the project" (equal "imported/smoke" (getf (first reports) :project)))
  (check "the imported project's page" (eql 200 (nth-value 1 (ask "/imported/smoke"))))
  (check "the imported repository carries the merged file"
         (search "# Notes" (ask "/api/v4/projects/imported%2Fsmoke/repository/files/NOTES.md/raw?ref=master")))
  (let ((issue (json (ask "/api/v4/projects/imported%2Fsmoke/issues/1"))))
    (check "the imported issue keeps its number and its state" (and (eql 1 (jget issue "iid")) (equal "closed" (jget issue "state"))))
    (check "the imported issue's note came along" (eql 1 (jget issue "user_notes_count"))))
  (check "the imported merge request is merged"
         (equal "merged" (jget (json (ask "/api/v4/projects/imported%2Fsmoke/merge_requests/1")) "state")))
  (check "importing again adds nothing"
         (let ((again (ignore-errors (uiop:symbol-call :anvil :import-from-gitlab! *base* :token *token* :projects '("tester/smoke") :into "imported"))))
           (and again (eql 0 (getf (first again) :issues)) (eql 0 (getf (first again) :merge-requests))))))

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

;;; Read-only: the same forge with *read-only?* on between requests.
;;; Browsing and fetching answer; every change over HTTP is refused
;;; whoever asks, the controls are greyed, and the REPL still writes.

(defun read-only! (on?)
  (setf (symbol-value (find-symbol "*READ-ONLY?*" :anvil)) on?))

(defun refused? (status body &key (expected 403))
  (and (eql status expected) (stringp body) (search "read-only" body) t))

(defun form-request (path content &rest keys)
  (apply #'ask path :method :post :content content :content-type "application/x-www-form-urlencoded" keys))

(check "off: the sign-in form is not greyed" (not (search "fieldset class=\"read-only\"" (ask "/users/sign_in"))))
(read-only! t)

;; reading
(check "read-only: the home page" (eql 200 (nth-value 1 (ask "/"))))
(check "read-only: explore" (eql 200 (nth-value 1 (ask "/explore"))))
(check "read-only: the project page" (eql 200 (nth-value 1 (ask "/tester/smoke"))))
(check "read-only: the blob page" (eql 200 (nth-value 1 (ask "/tester/smoke/-/blob/master/README.md"))))
(check "read-only: the issue page" (eql 200 (nth-value 1 (ask "/tester/smoke/-/issues/1"))))
(check "read-only: the API reads" (equal "tester/smoke" (jget (json (ask "/api/v4/projects/tester%2Fsmoke")) "path_with_namespace")))
(check "read-only: the API reads with a token" (equal "tester" (jget (json (ask "/api/v4/user" :token *token*)) "username")))
(check "read-only: an archive" (let ((octets (ask-octets "/tester/smoke/-/archive/master/smoke-master.tar.gz")))
                                 (and (vectorp octets) (= (aref octets 0) #x1f))))
(let ((fetched (ingot:init-repository (merge-pathnames "read-only-fetch.git/" *scratch*))))
  (check "read-only: a fetch over the smart protocol"
         (handler-case (plusp (nth-value 2 (ingot:fetch! fetched (uiop:symbol-call :anvil :http-git-transport (format nil "~a/tester/smoke.git" *base*)))))
           (error (e) (problem "read-only fetch: ~a" e)))))

;; the web's changes, refused as a page
(multiple-value-bind (body status headers) (form-request "/users/sign_in" "login=tester&password=password-of-tester")
  (check "read-only: sign-in refused, no session" (and (refused? status body) (search "<h1>Read-only for now</h1>" body)
                                                       (null (assoc :set-cookie headers)))))
(multiple-value-bind (body status) (form-request "/users/sign_up" "username=newcomer&name=N&email=n%40example.test&password=password-of-newcomer&password_confirmation=password-of-newcomer")
  (check "read-only: sign-up refused" (and (refused? status body) (null (uiop:symbol-call :anvil :find-user "newcomer")))))
(multiple-value-bind (body status) (form-request "/-/profile/tokens" "name=t&scopes=api" :cookie? t)
  (check "read-only: a token refused, signed in" (refused? status body)))
(multiple-value-bind (body status) (form-request "/projects/new" "name=Nope&path=nope&visibility=public" :cookie? t)
  (check "read-only: a web project refused" (and (refused? status body) (null (uiop:symbol-call :anvil :find-project "tester/nope")))))
(multiple-value-bind (body status) (form-request "/tester/smoke/-/issues/new" "title=Nope" :cookie? t)
  (check "read-only: a web issue refused" (refused? status body)))
(multiple-value-bind (body status) (form-request "/tester/smoke/-/issues/1/notes" "body=Nope" :cookie? t)
  (check "read-only: a comment refused" (refused? status body)))
(multiple-value-bind (body status) (form-request "/tester/smoke/-/edit/master/README.md" "content=x&message=x" :cookie? t)
  (check "read-only: a web edit refused" (refused? status body)))
(multiple-value-bind (body status) (form-request "/tester/smoke/-/settings" "name=Renamed" :cookie? t)
  (check "read-only: settings refused" (refused? status body)))
(multiple-value-bind (body status) (ask "/tester/smoke/-/deployments" :cookie? t)
  (check "read-only: the deployments page refused" (refused? status body)))
(check "read-only: the README is as it was" (search "Edited on the web" (ask "/api/v4/projects/1/repository/files/README.md/raw?ref=master")))

;; the API's writes and OAuth, refused as JSON
(multiple-value-bind (body status) (form-request "/api/v4/projects" "name=Nope&path=nope2" :token *token*)
  (check "read-only: an API project refused" (and (refused? status body) (jget (json body) "message"))))
(multiple-value-bind (body status) (ask "/api/v4/projects/1/issues" :method :post :token *token* :content-type "application/json" :content "{\"title\":\"Nope\"}")
  (check "read-only: an API issue refused" (refused? status body)))
(multiple-value-bind (body status) (ask "/api/v4/projects/1/issues/1" :method :put :token *token*
                                                                       :content "state_event=reopen" :content-type "application/x-www-form-urlencoded")
  (check "read-only: an API PUT refused" (and (refused? status body)
                                               (equal "closed" (jget (json (ask "/api/v4/projects/1/issues/1")) "state")))))
(check "read-only: an API DELETE refused" (eql 403 (nth-value 1 (ask "/api/v4/projects/1/repository/branches/feature" :method :delete :token *token*))))
(multiple-value-bind (body status) (form-request "/oauth/token" "grant_type=authorization_code&code=x&redirect_uri=x&client_id=x")
  (check "read-only: no OAuth token" (and (refused? status body) (equal "access_denied" (jget (json body) "error")))))
(check "read-only: the OAuth consent refused" (refused? (nth-value 1 (ask "/oauth/authorize?client_id=x" :cookie? t)) (ask "/oauth/authorize?client_id=x" :cookie? t)))

;; a push with an administrator's credentials, refused at both steps
(let ((master (jget (json (ask "/api/v4/projects/1/repository/branches/master")) "commit")))
  (multiple-value-bind (body status) (ask "/tester/smoke.git/info/refs?service=git-receive-pack" :basic (cons "tester" "password-of-tester"))
    (check "read-only: the push advertisement refused, as text" (and (refused? status body) (not (search "report-status" body)))))
  (multiple-value-bind (answer status)
      (ask-octets "/tester/smoke.git/git-receive-pack" :method :post :basic (cons "tester" *token*)
                                                        :content (ingot:string-to-octets "0000") :content-type "application/x-git-receive-pack-request")
    (check "read-only: a receive-pack refused, not a 500" (and (eql status 403) (search (ingot:string-to-octets "read-only") answer))))
  (check "read-only: master did not move" (equal (jget master "id") (jget (jget (json (ask "/api/v4/projects/1/repository/branches/master")) "commit") "id"))))

;; the controls, greyed and not hidden
(let ((sign-in (ask "/users/sign_in"))
      (home (ask "/"))
      (issue (ask "/tester/smoke/-/issues/1" :cookie? t))
      (blob (ask "/tester/smoke/-/blob/master/README.md" :cookie? t)))
  (check "read-only: the sign-in form is there, greyed"
         (and (search "<fieldset class=\"read-only\" disabled=\"disabled\" title=\"This forge is read-only for now.\">" sign-in)
              (cl-ppcre:scan "name=['\"]password['\"]" sign-in)))
  (check "read-only: the bar's Sign in is greyed, not a link"
         (and (cl-ppcre:scan "aria-disabled=['\"]true['\"][^>]*>Sign in</span>" home)
              (not (cl-ppcre:scan "href=['\"]/users/sign_in" home))))
  (check "read-only: the comment form is there, greyed, signed in"
         (and (cl-ppcre:scan "name=['\"]body['\"]" issue) (search "fieldset class=\"read-only\" disabled" issue)))
  (let ((forms (cl-ppcre:all-matches-as-strings "<form [^>]*method=['\"]post['\"][^>]*>" issue))
        (greyed (cl-ppcre:count-matches "fieldset class=\"read-only\" disabled" issue)))
    (unless (check "read-only: every POST form on the issue page is greyed" (and forms (= (length forms) greyed)))
      (format t "      ~d greyed; the forms:~%~{        ~a~%~}" greyed forms)))
  (check "read-only: the Edit button is greyed" (and (search ">Edit</span>" blob) (not (search "/-/edit/master/README.md" blob)))))

;; the REPL still writes: a user, a commit, a mirror refresh, an import
(check "read-only: make-user! at the REPL"
       (getf (uiop:symbol-call :anvil :make-user! "operator" "Operator" "operator@example.test" "password-of-operator") :id))
(let* ((project (uiop:symbol-call :anvil :find-project "tester/smoke"))
       (repository (uiop:symbol-call :anvil :project-repository project)))
  (ingot:make-commit! repository :branch "master" :author (ingot:make-identity "Operator" "operator@example.test") :message "From the REPL"
                                 :actions (list (list :action :create :path "operator.txt" :content (format nil "operator~%"))))
  (uiop:symbol-call :anvil :refresh-project-view! project))
(check "read-only: a mirror refresh at the REPL"
       (handler-case
           (multiple-value-bind (set deleted received)
               (uiop:symbol-call :anvil :fetch-repository! (uiop:symbol-call :anvil :find-project "imported/smoke") (format nil "~a/tester/smoke.git" *base*))
             (declare (ignore deleted))
             (and set (plusp received)
                  (search "operator" (ask "/api/v4/projects/imported%2Fsmoke/repository/files/operator.txt/raw?ref=master"))))
         (error (e) (problem "read-only mirror refresh: ~a" e))))
(check "read-only: pull-mirrors! at the REPL brings an imported project up to date"
       (handler-case
           (let* ((project (uiop:symbol-call :anvil :find-project "imported/smoke"))
                  (repository (uiop:symbol-call :anvil :project-repository (uiop:symbol-call :anvil :find-project "tester/smoke"))))
             (ingot:make-commit! repository :branch "master" :author (ingot:make-identity "Operator" "operator@example.test") :message "For the mirror"
                                            :actions (list (list :action :create :path "mirrored.txt" :content (format nil "mirrored~%"))))
             (and (uiop:symbol-call :anvil :project-pull-mirror project)
                  (progn (uiop:symbol-call :anvil :pull-mirrors!) t)
                  (search "mirrored" (ask "/api/v4/projects/imported%2Fsmoke/repository/files/mirrored.txt/raw?ref=master"))
                  (getf (getf (uiop:symbol-call :anvil :find-project "imported/smoke") :mirror) :last-ok?)
                  (equal "finished" (jget (json (ask "/api/v4/projects/imported%2Fsmoke")) "import_status"))))
         (error (e) (problem "pull-mirrors!: ~a" e))))
(check "read-only: an import at the REPL"
       (let ((reports (ignore-errors (uiop:symbol-call :anvil :import-from-gitlab! *base* :token *token* :projects '("tester/pushed") :into "readonly-import"))))
         (and (equal "readonly-import/pushed" (getf (first reports) :project))
              (eql 200 (nth-value 1 (ask "/readonly-import/pushed"))))))

;; a read-only forge serves the public, so its importer brings only what
;; the public sees there, whatever the token can see; one part failing
;; leaves the rest of the import going
(let ((reports (ignore-errors (uiop:symbol-call :anvil :import-from-gitlab! *base* :token *token* :projects '("tester/secret") :into "readonly-import"))))
  (check "read-only: the importer leaves a private project out"
         (and (getf (first reports) :skipped)
              (null (uiop:symbol-call :anvil :find-project "readonly-import/secret")))))
(let ((reports (ignore-errors (uiop:symbol-call :anvil :import-from-gitlab! *base* :token *token* :projects '("tester/secret") :into "readonly-private"
                                                                          :public-only? nil))))
  (check "read-only: :public-only? nil brings it, private"
         (let ((project (uiop:symbol-call :anvil :find-project "readonly-private/secret")))
           (and (null (getf (first reports) :skipped)) project (eq :private (getf project :visibility))))))
(multiple-value-bind (project report)
    (handler-case
        (uiop:symbol-call :anvil :import-gitlab-project!
                          (uiop:symbol-call :anvil :make-gitlab-client *base* :token *token*)
                          (uiop:symbol-call :anvil :json-table "id" 99999 "path" "ghost" "name" "ghost" "visibility" "public"
                                            "issues_access_level" "enabled" "merge_requests_access_level" "enabled"
                                            "namespace" (uiop:symbol-call :anvil :json-table "full_path" "readonly-import"))
                          :repository? nil)
      (error (e) (problem "an import part failing: ~a" e) (values nil nil)))
  (check "read-only: a part that fails is reported and the import goes on"
         (and project (getf report :issues-error) (getf report :merge-requests-error))))

(read-only! nil)
(check "off again: the API writes" (integerp (jget (json (ask "/api/v4/projects/1/issues" :method :post :token *token* :content-type "application/json"
                                                                                  :content "{\"title\":\"After read-only\"}"))
                                                   "iid")))
(check "off again: a push advertisement" (search "report-status" (ask "/tester/smoke.git/info/refs?service=git-receive-pack" :basic (cons "tester" *token*))))

;; a pull mirror through the API, GitLab's way: mirror and import_url, then mirror/pull
(let* ((url (format nil "~a/tester/pushed.git" *base*))
       (json (json (ask "/api/v4/projects/readonly-import%2Fpushed" :method :put :token *token* :content-type "application/json"
                        :content (format nil "{\"mirror\":true,\"import_url\":\"~a\"}" url)))))
  (check "a pull mirror set through the API" (equal url (jget json "import_url")))
  (check "POST mirror/pull answers 200" (eql 200 (nth-value 1 (ask "/api/v4/projects/readonly-import%2Fpushed/mirror/pull" :method :post :token *token*))))
  (check "mirror=false clears it"
         (not (equal url (jget (json (ask "/api/v4/projects/readonly-import%2Fpushed" :method :put :token *token* :content-type "application/json"
                                          :content "{\"mirror\":false}"))
                              "import_url")))))

;;; The deployment door, against a house that is not there

(check "the profile report finds the files missing"
       (let ((report (uiop:symbol-call :anvil :profile-report (uiop:symbol-call :anvil :find-project "tester/smoke"))))
         (and (not (getf report :image?)) (getf report :faults))))

;;; Visits: what is counted, and the day's summary posted

(defparameter *browser* "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0 Safari/537.36")

(defun browse (path &key (agent *browser*) forwarded (person? t))
  "GET PATH as AGENT, from the address FORWARDED says when it is given;
with a browser's navigation headers when PERSON?."
  (nth-value 1 (ignore-errors
                (net.aserve.client:do-http-request (format nil "~a~a" *base* path)
                  :user-agent agent :keep-alive nil :timeout 60
                  :headers (append (when forwarded (list (cons "X-Forwarded-For" forwarded)))
                                   (when person? (list (cons "Sec-Fetch-Mode" "navigate")
                                                       (cons "Sec-Fetch-Dest" "document")
                                                       (cons "Accept-Language" "en-US,en;q=0.9"))))))))

(defun today-visits () (first (uiop:symbol-call :anvil :visits-summary)))

(let ((before (today-visits)))
  (browse "/explore" :forwarded "203.0.113.7")
  (browse "/tester/smoke" :forwarded "203.0.113.7")
  ;; the proxy's entry is the last one: a first entry the client sent does not make a new visitor
  (browse "/tester/smoke" :forwarded "198.51.100.99, 203.0.113.7")
  (browse "/tester/smoke" :forwarded "203.0.113.8")
  (browse "/tester/smoke" :agent "curl/8.5.0")
  ;; a browser's name without a browser's way of asking: a scraper in disguise
  (browse "/tester/smoke/-/blob/master/README.md" :forwarded "203.0.113.9" :person? nil)
  (browse "/static/anvil.css")
  (browse "/tester/smoke.git/info/refs?service=git-upload-pack" :agent "git/2.43.0")
  (browse "/api/v4/version")
  (let ((after (today-visits)))
    (flet ((more (key) (- (getf after key) (getf before key))))
      (check "visits: four page views by a browser" (= 4 (more :views)))
      (check "visits: two visitors, the forwarded address being the proxy's entry" (= 2 (more :visitors)))
      (check "visits: the project's views" (>= (or (cdr (assoc "tester/smoke" (getf after :projects) :test #'equal)) 0) 3))
      (check "visits: a robot apart" (= 1 (more :robots)))
      (check "visits: a browser's name without a browser's asking, apart" (= 1 (more :unverified)))
      (check "visits: a clone" (= 1 (more :fetches)))
      (check "visits: an API call" (= 1 (more :api))))))

(defvar *posted* nil "The summaries the capture door took: (title . text).")

(net.aserve:publish :path "/ntfy-capture" :server (first (symbol-value (find-symbol "*PUBLISHED-SERVERS*" :anvil)))
                    :function (lambda (req ent)
                                (push (cons (net.aserve:header-slot-value req :title)
                                            (net.aserve:get-request-body req :external-format :utf-8))
                                      *posted*)
                                (net.aserve:with-http-response (req ent :content-type "text/plain")
                                  (net.aserve:with-http-body (req ent) (write-string "ok" net.html.generator:*html-stream*)))))

(setf (symbol-value (find-symbol "*VISITS-NOTIFY-URL*" :anvil)) (format nil "~a/ntfy-capture" *base*))
(let ((today (getf (today-visits) :date)))
  ;; a new day: today is finished and its summary waits; housekeeping posts it
  (bt:with-lock-held ((symbol-value (find-symbol "*VISITS-LOCK*" :anvil)))
    (uiop:symbol-call :anvil :roll-visits! "2099-01-01"))
  ;; read the finished days themselves: visits-summary would roll 2099 back to today
  (check "visits: the finished day kept"
         (equal today (getf (first (symbol-value (find-symbol "*VISIT-DAYS*" :anvil))) :date)))
  (let ((reports (symbol-value (find-symbol "*VISIT-REPORTS*" :anvil))))
    (check "visits: its summary waiting" (equal today (getf (first reports) :date)))
    (handler-case (uiop:symbol-call :anvil :post-visit-report (first reports))
      (error (e) (problem "visits: posting: ~a" e))))
  (let ((post (find-if (lambda (p) (search today (or (car p) ""))) *posted*)))
    (check "visits: the summary posted with its title" post)
    (check "visits: the summary's text"
           (and post (search "page views by" (cdr post)) (search "tester/smoke" (cdr post))
                (search "Clones and fetches: " (cdr post))
                (search "did not look like a person's: " (cdr post)))))
  ;; housekeeping posts what waits (today's, and 2099's as the date rolls back) and clears it
  (handler-case (uiop:symbol-call :anvil :visits-housekeeping!)
    (error (e) (problem "visits: housekeeping: ~a" e)))
  (check "visits: housekeeping posts the waiting summaries and clears them"
         (and (null (symbol-value (find-symbol "*VISIT-REPORTS*" :anvil)))
              (find-if (lambda (p) (search "2099-01-01" (or (car p) ""))) *posted*))))
(check "visits: written to the data directory"
       (progn (uiop:symbol-call :anvil :save-visits!)
              (probe-file (merge-pathnames "visits.sexp" *scratch*))))

(ignore-errors (uiop:symbol-call :anvil :stop!))
(ignore-errors (uiop:delete-directory-tree *scratch* :validate t))
(finish)
