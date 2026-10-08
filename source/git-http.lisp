;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :anvil)

;;
;; Git over HTTP: /<namespace>/<project>.git/info/refs?service=...,
;; /git-upload-pack, /git-receive-pack, and the few dumb-protocol files
;; a client may still ask for.  Ingot does the protocol; this file does
;; who may, and the hooks after a push.
;;

(defun git-request? (rest)
  "Whether REST (the path after the project's) is one of git's."
  (or (string= rest "/info/refs") (string= rest "/git-upload-pack") (string= rest "/git-receive-pack")
      (string= rest "/HEAD") (starts-with? "/objects/" rest) (string= rest "/info/packs")))

(defun git-authenticate! (req ent project writing?)
  "Admit or refuse the git request: public reads need nobody; else
Basic credentials (a user and a password, or a token).  Answers true
when admitted; otherwise the response has been written."
  (cond
    ((and (not writing?) (can? :download-code project *current-user*)
          (token-allows? *current-token* :read-repository))
     t)
    ((and writing? *current-user* (token-allows? *current-token* :write-repository) (can? :push project *current-user*))
     t)
    ((null *current-user*)
     (net.aserve:with-http-response (req ent :response net.aserve:*response-unauthorized* :content-type "text/plain")
       (setf (net.aserve:reply-header-slot-value req :www-authenticate)
             (format nil "Basic realm=\"~a\"" *site-name*))
       (net.aserve:with-http-body (req ent)
         (write-string "Sign in with your password or a personal access token." (net.aserve:request-reply-stream req))))
     nil)
    (t (respond-text req ent (if writing? "You may not push to this project." "You may not read this project.") :status 403)
       nil)))

(defun handle-git-request (req ent project rest)
  "Serve the git request REST for PROJECT."
  (let* ((repository (project-repository project))
         (service (url-query-value req "service")))
    (cond
      ((string= rest "/info/refs")
       (let ((writing? (equal service "git-receive-pack")))
         (when (git-authenticate! req ent project writing?)
           (cond ((member service '("git-upload-pack" "git-receive-pack") :test #'equal)
                  (respond req ent (ingot:advertise-refs repository (if writing? :receive-pack :upload-pack))
                           :content-type (format nil "application/x-~a-advertisement" service)
                           :cache-control "no-cache"))
                 (t ;; the dumb protocol: the refs as text
                  (respond req ent (format nil "~{~a~%~}"
                                           (loop for (name . oid) in (ingot:refs repository) collect (format nil "~a	~a" oid name)))
                           :content-type "text/plain" :cache-control "no-cache"))))))
      ((string= rest "/git-upload-pack")
       (when (git-authenticate! req ent project nil)
         (let ((body (request-body-octets req)))
           (respond req ent (ingot:upload-pack repository (or body (ingot:make-octets 0)))
                    :content-type "application/x-git-upload-pack-result" :cache-control "no-cache"))))
      ((string= rest "/git-receive-pack")
       (when (git-authenticate! req ent project t)
         (let* ((body (request-body-octets req))
                (user *current-user*))
           (multiple-value-bind (response commands)
               (ingot:receive-pack repository (or body (ingot:make-octets 0))
                                   :check (lambda (command) (push-refusal project user repository command)))
             (after-push! project user commands)
             (respond req ent response :content-type "application/x-git-receive-pack-result" :cache-control "no-cache")))))
      ((string= rest "/HEAD")
       (when (git-authenticate! req ent project nil)
         (respond-text req ent (format nil "ref: ~a~%" (or (ingot:head-ref repository) "refs/heads/master")))))
      (t (not-found)))))

(defun push-refusal (project user repository command)
  "Nil when USER may make the ref change COMMAND asks, else why not."
  (let* ((name (ingot::ref-command-name command))
         (old (ingot::ref-command-old command))
         (new (ingot::ref-command-new command))
         (delete? (string= new ingot::*zero-oid*))
         (branch (and (starts-with? "refs/heads/" name) (subseq name 11)))
         (tag (and (starts-with? "refs/tags/" name) (subseq name 10))))
    (cond
      ((not (or branch tag)) "only branches and tags may be pushed")
      ((and branch (not (can-push-branch? project branch user)))
       (if (protected-branch? project branch) "protected branch: Maintainers only" "you may not push here"))
      ((and tag (not (can? :push project user))) "you may not push tags here")
      ((and tag (not delete?) (ingot:ref-value* repository name) (not (can? :maintain project user)))
       "tags are not moved; delete it first")
      ((and branch delete? (string= branch (getf project :default-branch))) "the default branch cannot be deleted")
      ((and branch delete? (protected-branch? project branch) (not (can? :maintain project user)))
       "protected branch: Maintainers may delete it")
      ((and branch (not delete?) (protected-branch? project branch)
            (not (ingot::fast-forward? repository old new)))
       "protected branch: not a fast-forward")
      ((and branch (not delete?) (not (protected-branch? project branch))
            (not (ingot::fast-forward? repository old new))
            (not (can? :push project user)))
       "non-fast-forward")
      (t nil))))
