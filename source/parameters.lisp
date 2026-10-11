;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :anvil)

;;
;; What a deployment sets.  Everything a site changes is a DEFVAR, so
;; that loading this file again into a running forge keeps the site's
;; settings; the tuning is DEFPARAMETER.  A deployment sets these from
;; its start-up file before calling initialize!.
;;

(defvar *site-name* "Anvil"
  "String. The forge's name, in titles and the bar.")

(defvar *site-url* "http://localhost:9080"
  "String. The forge's address as the public reaches it: clone
addresses, links in API answers and webhooks are made from it.")

(defvar *hosts* nil
  "List of host names the forge answers to, or nil for every host of the
server.  A development image shared with other applications names its
own (\"anvil.localhost\").")

(defvar *data-directory*
  (let ((candidates '("/state/anvil/" "/projects/.state/anvil/" "/tmp/anvil/")))
    (uiop:ensure-directory-pathname
     (or (find-if (lambda (dir) (uiop:directory-exists-p (uiop:pathname-parent-directory-pathname (pathname dir))))
                  candidates)
         "/tmp/anvil/")))
  "Pathname. Where everything the forge keeps lives: db/ (the records),
repositories/, ci/ (traces, artifacts, caches).")

(defvar *registration-open?* t
  "Boolean. Whether anyone may make an account at /users/sign_up.  Off,
an administrator makes accounts.")

(defvar *read-only?* nil
  "Boolean. Whether the forge only shows: anonymous browsing as ever,
and every mutation over HTTP (accounts, tokens, projects, issues, merge
requests, settings, the API's writes, OAuth, a push) refused, with its
controls greyed.  Functions called at the REPL still write, so an
operator can load and refresh a read-only forge.  Set when the
environment variable ANVIL_READ_ONLY is non-empty.")

(defvar *read-only-text* "This forge is read-only for now."
  "String. What a refusal says, and the title of a greyed control.")

(defun read-only-environment? ()
  "Whether the environment asks for a read-only forge: ANVIL_READ_ONLY
set and non-empty."
  (let ((flag (uiop:getenv "ANVIL_READ_ONLY")))
    (and flag (plusp (length flag)) t)))

(defvar *robots-txt*
  "# Project pages, issues, merge requests, groups: yes.  The history
# file by file at every commit, archives and the API: no -- a forge's
# address space is every commit times every path.
User-agent: *
Crawl-delay: 2
Disallow: /api/
Disallow: /search
Disallow: /users/
Disallow: /oauth/
Disallow: /admin
Disallow: /dashboard
Disallow: /-/
Disallow: /*.git/
Disallow: /*/-/commit/
Disallow: /*/-/commits/
Disallow: /*/-/compare
Disallow: /*/-/tree/
Disallow: /*/-/blob/
Disallow: /*/-/raw/
Disallow: /*/-/archive/
Disallow: /*/-/switch
Disallow: /*/-/edit/
Disallow: /*/-/new/
Disallow: /*/-/jobs/
Disallow: /*/-/pipelines
Disallow: /*/-/deployments
Disallow: /*/-/settings
"
  "String. What /robots.txt answers.")

(defvar *default-visibility* :public)

(defvar *default-branch-name* "master"
  "String. The default branch of a new project when nothing else says.")

(defvar *protected-branches-default* '(:default)
  "List. Which branches a new project protects: :default for its
default branch, else names.")

(defvar *admin-emails* nil
  "List of strings: the first user whose email is among these is made
an administrator at creation (a way to bootstrap a site).")

;;; The runner

(defvar *docker-socket* "/var/run/docker.sock"
  "String. The Docker daemon's Unix socket; or a \"tcp://host:port\"
address.  Nil switches the docker executor off.")

(defvar *runner-tags* '("docker")
  "List of strings: the tags this image's runner answers to.  A job
naming a tag not here waits for a runner that has it (there is none:
it is stuck), as on GitLab.  A job with no tags runs anywhere.")

(defvar *runner-parallelism* 2
  "Integer. Jobs run at once.")

(defvar *shell-executor?* nil
  "Boolean. Whether a job tagged `shell' may run on this image's own
host through a subprocess.  Off unless the site trusts its projects.")

(defvar *runner-memory-bytes* (* 2 1024 1024 1024)
  "Integer. The memory limit of a job's container.")

(defvar *job-timeout-seconds* 3600
  "Integer. A job running longer is killed.")

(defvar *registry-credentials* nil
  "Alist of (registry . (user . password)) for pulling private images.")

;;; Deployment houses: where Deploy sends a project

(defvar *deploy-houses* nil
  "List of plists, one per house a project may be deployed to:
  (:key :common-lisp.app :name \"common-lisp.app\"
   :lab-url \"https://lab.common-lisp.app/prompt-lab\"
   :secret-file #p\"/projects/.secrets/anvil-hosting-secret\"
   :domain \"common-lisp.app\")
The lab's hosting/request door is asked with the secret in
X-Hosting-Secret; hosting/state is polled for the request's state.")

;;; Tuning

(defparameter *session-days* 30)
(defparameter *per-page-default* 20)
(defparameter *per-page-max* 100)
(defparameter *max-request-body* (* 1024 1024 1024)
  "Bytes. The largest push or API body taken (a push of a whole
history: gendl's is 149 MB).")
(defparameter *trace-tail-bytes* (* 512 1024)
  "Bytes. How much of a trace the job page shows at once.")
(defparameter *markdown-limit* (* 512 1024))
(defparameter *blob-view-limit* (* 2 1024 1024)
  "Bytes. Files larger than this are offered raw, not shown.")
(defparameter *webhook-timeout* 15)
(defparameter *password-iterations* 60000)

(defvar *version* "0.1.0")
