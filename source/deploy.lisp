;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :anvil)

;;
;; Deployment: a project into a hosting house -- common-lisp.net's lab,
;; which checks it, reviews it and puts it up at <name>.common-lisp.app.
;; The forge already knows who is a Maintainer, so the Deploy button
;; posts the request to the lab's hosting/request door with the shared
;; secret and no OAuth round trip; the request's state is read from
;; hosting/state.  A :deploy job in a pipeline does the same at the end
;; of a green run.  Before either, the profile is read in process when
;; Monocle is loaded: image.sexp and monetize.sexp from the tree, their
;; faults listed, so an author sees what is missing before pressing
;; anything.
;;

(defun deploy-house (key)
  (find (if (stringp key) (intern (string-upcase key) :keyword) key) *deploy-houses* :key (lambda (h) (getf h :key))))

(defun house-secret (house)
  (or (read-secret (getf house :secret-file) :min-length 8)
      (getf house :secret)))

(defun house-offered? (house)
  (and house (getf house :lab-url) (house-secret house) t))

(defun house-door (house name)
  (format nil "~a/api/hosting/~a" (string-right-trim "/" (getf house :lab-url)) name))

;;; The profile, read from the tree

(defun monocle-loaded? () (and (find-package :monocle) t))

(defun profile-report (project &optional sha)
  "A plist: :sha, :image? :monetize? (the files are there), :faults (a
list of strings; nil when both read clean), :checked? (Monocle was
there to check), :name (the application's name from monetize.sexp)."
  (let* ((repository (project-repository project))
         (sha (or sha (ingot:resolve-commit repository (getf project :default-branch))))
         (image (and sha (ingot:blob-at repository sha "image.sexp")))
         (monetize (and sha (ingot:blob-at repository sha "monetize.sexp")))
         (faults nil) (name nil) (checked? nil))
    (when (and sha (monocle-loaded?) (or image monetize))
      (let ((dir (merge-pathnames (format nil "anvil-profile-~d-~a/" (getf project :id) (random 1000000)) (uiop:temporary-directory))))
        (ensure-directories-exist dir)
        (unwind-protect
             (handler-case
                 (progn
                   (setq checked? t)
                   (if image
                       (progn (ingot:write-file-octets (merge-pathnames "image.sexp" dir) image)
                              (multiple-value-bind (form reason) (uiop:symbol-call :monocle :read-image-spec (merge-pathnames "image.sexp" dir))
                                (if form
                                    (setq faults (append faults (uiop:symbol-call :monocle :image-faults form
                                                                                  (mapcar #'car (ingot:tree-paths repository (ingot:commit-tree (ingot:read-commit repository sha)))))))
                                    (push (or reason "image.sexp does not read.") faults))))
                       (push "No image.sexp at the root of the repository (rule 1)." faults))
                   (if monetize
                       (progn (ingot:write-file-octets (merge-pathnames "monetize.sexp" dir) monetize)
                              (multiple-value-bind (manifest reason) (uiop:symbol-call :monocle :read-manifest (merge-pathnames "monetize.sexp" dir))
                                (if manifest
                                    (progn (setq faults (append faults (uiop:symbol-call :monocle :manifest-faults manifest)))
                                           (setq name (ignore-errors (uiop:symbol-call :monocle :manifest-name manifest))))
                                    (push (or reason "monetize.sexp does not read.") faults))))
                       (push "No monetize.sexp at the root of the repository (rule 5)." faults)))
               (error (e) (push (format nil "The profile could not be checked: ~a" e) faults)))
          (ignore-errors (uiop:delete-directory-tree dir :validate t)))))
    (unless (monocle-loaded?)
      (unless image (push "No image.sexp at the root of the repository (rule 1)." faults))
      (unless monetize (push "No monetize.sexp at the root of the repository (rule 5)." faults)))
    (unless name
      (setq name (and monetize (ignore-errors
                                (let ((form (with-standard-io-syntax
                                              (let ((*read-eval* nil) (*package* (find-package :keyword)))
                                                (read-from-string (ingot:octets-to-string monetize))))))
                                  (and (consp form) (getf (rest form) :name)))))))
    (list :sha sha :image? (and image t) :monetize? (and monetize t) :faults (reverse faults) :checked? checked? :name name)))

;;; Requests to the house

(defun project-deployments (project)
  (records :deployment (lambda (d) (eql (getf d :project-id) (getf project :id)))))

(defun request-hosting! (project user &key house-key sha session)
  "Ask the house to host PROJECT at SHA (the default branch's tip when
not given).  Answers the deployment record.  Signals http-error when
the house refuses or is not reachable."
  (let* ((house (or (deploy-house (or house-key (getf (getf project :deploy) :house) (getf (first *deploy-houses*) :key)))
                    (bad-request "No deploy house is configured on this forge.")))
         (repository (project-repository project))
         (sha (or sha (ingot:resolve-commit repository (getf project :default-branch))
                  (bad-request "The project has no commits."))))
    (unless (house-offered? house) (http-error 503 "The house ~a is not reachable from here (no secret or no address)." (getf house :name)))
    (unless (eq (getf project :visibility) :public)
      (bad-request "The house hosts public projects only: it clones what anyone can read."))
    (multiple-value-bind (answer status)
        (handler-case
            (http-post-json (house-door house "request")
                            (json-string (json-table "project_id" (getf project :id)
                                                     "project" (getf project :path-with-namespace)
                                                     "sha" sha
                                                     "web_url" (project-web-url project)
                                                     "clone_url" (project-clone-url project)
                                                     "session" session
                                                     "requester" (and user (getf user :username))))
                            :headers (list (cons "X-Hosting-Secret" (house-secret house))) :timeout 30)
          (error (e) (http-error 502 "The house did not answer: ~a" e)))
      (let ((json (and (stringp answer) (ignore-errors (parse-json answer)))))
        (unless (and (eql status 200) (hash-table-p json) (json-get json "id"))
          (http-error 502 "The house refused the request (~a): ~a" status (or (and json (json-get json "error")) answer)))
        (let ((deployment (save-record! (new-record :deployment :project-id (getf project :id) :house (getf house :key)
                                                                :sha sha :request-id (json-get json "id")
                                                                :state "queued" :url nil :name nil
                                                                :user-id (and user (getf user :id))
                                                                :history nil :checked (get-universal-time)))))
          (touch-project! project)
          deployment)))))

(defun refresh-deployment! (deployment)
  "Ask the house where the request stands; answers the updated record."
  (let ((house (deploy-house (getf deployment :house))))
    (if (not (and house (getf house :lab-url)))
        deployment
        (multiple-value-bind (json status)
            (handler-case (http-get-json (format nil "~a?id=~a" (house-door house "state") (getf deployment :request-id)) :timeout 20)
              (error () (values nil nil)))
          (if (and (eql status 200) (hash-table-p json))
              (update-record! deployment
                              :state (or (json-get json "state") (getf deployment :state))
                              :url (or (json-get json "url") (getf deployment :url))
                              :name (or (json-get json "name") (getf deployment :name))
                              :history (map 'list (lambda (h) (list :time (json-get h "time") :state (json-get h "state") :note (json-get h "note")))
                                            (or (json-get json "history") #()))
                              :reasons (let ((review (json-get json "review")) (check (json-get json "check")))
                                         (append (and (hash-table-p review) (json-get review "summary") (list (json-get review "summary")))
                                                 (and (hash-table-p check)
                                                      (remove-if-not (lambda (line) (and (stringp line) (starts-with? "FAIL" line)))
                                                                     (coerce (or (json-get check "lines") #()) 'list)))))
                              :checked (get-universal-time))
              (update-record! deployment :checked (get-universal-time)))))))

(defun deployment-final? (deployment)
  (member (getf deployment :state) '("live" "failed" "rejected" "error") :test #'equal))

(defun refresh-open-deployments! ()
  (dolist (deployment (records :deployment (lambda (d) (not (deployment-final? d)))))
    (when (> (- (get-universal-time) (or (getf deployment :checked) 0)) 20)
      (handler-case (refresh-deployment! deployment)
        (error (e) (log-line "deployment ~d: ~a" (getf deployment :id) e))))))

(defparameter *deployment-state-texts*
  '(("queued" . "waiting for the house")
    ("checking" . "being built and checked")
    ("reviewing" . "being read")
    ("approved" . "passed, going up")
    ("held" . "held for a person at the house")
    ("deploying" . "going up")
    ("live" . "live")
    ("failed" . "not hosted")
    ("rejected" . "rejected by a person")
    ("error" . "could not be put up")))

(defun deployment-state-text (state)
  (or (cdr (assoc state *deployment-state-texts* :test #'equal)) state))

;;; A deploy job

(defparameter *deploy-job-poll-seconds* 15)
(defparameter *deploy-job-wait-seconds* 1800)

(defun execute-deploy-job! (job pipeline project)
  (let* ((spec (getf job :spec))
         (house-key (getf (getf spec :deploy) :house))
         (house (deploy-house house-key))
         (user (record :user (getf pipeline :user-id))))
    (trace-line! job "Deploying ~a at ~a to ~a" (getf project :path-with-namespace) (subseq (getf pipeline :sha) 0 8)
                 (or (getf house :name) house-key))
    (let ((report (profile-report project (getf pipeline :sha))))
      (trace-line! job "Profile: image.sexp ~:[missing~;found~], monetize.sexp ~:[missing~;found~]~@[, application name ~a~]"
                   (getf report :image?) (getf report :monetize?) (getf report :name))
      (dolist (fault (getf report :faults)) (trace-line! job "  FAULT ~a" fault))
      (when (getf report :faults)
        (trace-line! job "The project does not meet the profile; nothing was sent to the house.")
        (set-job-status! (record :job (getf job :id)) :failed :failure-reason "profile faults")
        (return-from execute-deploy-job! nil)))
    (let ((deployment (handler-case (request-hosting! project user :house-key house-key :sha (getf pipeline :sha))
                        (error (e)
                          (trace-line! job "ERROR: ~a" e)
                          (set-job-status! (record :job (getf job :id)) :failed :failure-reason (princ-to-string e))
                          (return-from execute-deploy-job! nil)))))
      (trace-line! job "The house took the request as ~a; waiting for it." (getf deployment :request-id))
      (update-record! job :deployment-id (getf deployment :id))
      (let ((last-state nil) (waited 0))
        (loop
          (sleep *deploy-job-poll-seconds*)
          (incf waited *deploy-job-poll-seconds*)
          (setq deployment (refresh-deployment! (record :deployment (getf deployment :id))))
          (let ((state (getf deployment :state)))
            (unless (equal state last-state)
              (trace-line! job "~a: ~a" (iso-timestamp) (deployment-state-text state))
              (dolist (reason (getf deployment :reasons)) (trace-line! job "  ~a" reason))
              (setq last-state state))
            (cond ((equal state "live")
                   (trace-line! job "Live at ~a" (getf deployment :url))
                   (set-job-status! (record :job (getf job :id)) :success)
                   (return))
                  ((member state '("failed" "rejected" "error") :test #'equal)
                   (set-job-status! (record :job (getf job :id)) :failed :failure-reason (deployment-state-text state))
                   (return))
                  ((equal state "held")
                   (trace-line! job "Held for a person at the house: the job ends here; the Deployments page follows the request.")
                   (set-job-status! (record :job (getf job :id)) :success)
                   (return))
                  ((>= waited *deploy-job-wait-seconds*)
                   (trace-line! job "Still ~a after ~a; the job ends here and the Deployments page follows the request."
                                (deployment-state-text state) (duration-text waited))
                   (set-job-status! (record :job (getf job :id)) :success)
                   (return)))))))))

;;; After a green pipeline: deploy when the project asks for it

(defun deploy-on-green! (pipeline)
  "When the project's deploy settings say so and the pipeline is a
green one on the default branch with no :deploy job of its own, ask
the house."
  (let* ((project (record :project (getf pipeline :project-id)))
         (settings (and project (getf project :deploy))))
    (when (and settings (getf settings :on-green?)
               (eq (getf pipeline :status) :success)
               (equal (getf pipeline :branch) (getf project :default-branch))
               (notany (lambda (job) (eq (job-kind job) :deploy)) (pipeline-jobs pipeline)))
      (handler-case (request-hosting! project (record :user (getf pipeline :user-id)) :house-key (getf settings :house) :sha (getf pipeline :sha))
        (error (e) (log-line "deploy on green, ~a: ~a" (getf project :path-with-namespace) e))))))

(pushnew 'deploy-on-green! *pipeline-finished-hooks*)
