;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :anvil)

;;
;; Visits: how much the forge is looked at, counted in its own process
;; as requests come in, and a summary of each day (UTC) posted where the
;; site asks (*visits-notify-url*, an ntfy topic's address).  No script,
;; no cookie, no third party.
;;
;; What is counted, for each day:
;;
;;  - page views: GETs of the forge's pages by a browser, not the
;;    static files, the API, git or robots.txt;
;;  - visitors: the different browsers among them, one browser on one
;;    address being one visitor.  They are told apart by a digest of the
;;    address and the browser's name taken with a secret made fresh each
;;    day; the digests and the secret go when the day ends, so nothing
;;    ties a visitor to the next day, and no address is written down;
;;  - views by project, for the day's most looked at;
;;  - clones and fetches: the advertisement that opens each one
;;    (info/refs?service=git-upload-pack), whoever asks;
;;  - API calls, whoever makes them;
;;  - robots and scripts: page GETs whose browser name says it is not a
;;    person (the same list common-lisp.net uses), counted apart;
;;  - unverified: page GETs that name a browser but do not ask the way a
;;    browser does when a person opens a page (Sec-Fetch-Mode: navigate,
;;    or Sec-Fetch-Dest: document, with an Accept-Language): scrapers
;;    wearing a browser's name, counted apart from the people.  A browser
;;    old enough to send no Sec-Fetch headers lands here too.
;;
;; The counts live in data/visits.sexp, written every minute while there
;; is anything new: the finished days, and the day in progress with its
;; digests and secret, so a restart neither forgets nor counts twice.
;;

(defparameter *visit-robot-agents*
  (ppcre:create-scanner
   "(?i)bot\\b|bot/|crawl|spider|slurp|archiver|scrape|scan|monitor|uptime|preview|lighthouse|headless|phantomjs|facebookexternalhit|embedly|whatsapp|curl|wget|httpie|python|go-http|java/|okhttp|libwww|perl|ruby|php|drakma|aserve|feed|rss|anvil/")
  "Scanner. User-Agent strings that are not people.")

(defparameter *visit-site-segments*
  '("static" "api" "oauth" "users" "-" "explore" "dashboard" "search" "help" "projects" "groups" "admin")
  "List of strings: first path segments that are the forge's own pages,
not a namespace, so a view of them names no project.")

(defparameter *visit-days-kept* 400
  "Integer. Finished days kept in data/visits.sexp.")

(defparameter *visit-top-projects* 5
  "Integer. Projects named in a day's summary.")

(defvar *visits-lock* (bt:make-lock "anvil visits"))
(defvar *visits-loaded?* nil "True once data/visits.sexp has been read into this image.")
(defvar *visits-dirty?* nil "True while there are counts the file has not seen.")
(defvar *visits-since* nil "ISO date of the first day counted.")
(defvar *visit-days* nil
  "List of plists, newest first, one per finished day: :date :views
:visitors :fetches :api :robots and :projects, an alist of (path
. views), most viewed first.")
(defvar *visit-reports* nil
  "List of finished days' plists not yet posted, oldest first.")

(defstruct (visit-day (:conc-name day-))
  (date nil)
  (salt nil)
  (views 0) (fetches 0) (api 0) (robots 0) (unverified 0)
  (projects (make-hash-table :test #'equal))   ; path -> views
  (seen (make-hash-table :test #'equal)))      ; digest -> t

(defvar *visit-today* nil "The visit-day being counted.")

(defun visits-file () (data-path "visits.sexp"))

(defun new-visit-day (date &key salt)
  (make-visit-day :date date :salt (or salt (random-hex 16))))

;;; Days

(defun finished-day (day)
  "The plist that keeps DAY once it is over."
  (list :date (day-date day)
        :views (day-views day)
        :visitors (hash-table-count (day-seen day))
        :fetches (day-fetches day)
        :api (day-api day)
        :robots (day-robots day)
        :unverified (day-unverified day)
        :projects (let ((pairs (project-views day)))
                    (subseq (sort pairs #'> :key #'cdr) 0 (min 20 (length pairs))))))

(defun project-views (day)
  "An alist of (path . views), DAY's views by project, in no order."
  (let ((pairs nil))
    (maphash (lambda (path n) (push (cons path n) pairs)) (day-projects day))
    pairs))

(defun seen-digests (day)
  "A list of DAY's visitor digests."
  (let ((digests nil))
    (maphash (lambda (digest seen?) (declare (ignore seen?)) (push digest digests)) (day-seen day))
    digests))

(defun roll-visits! (&optional (date (iso-date)))
  "Start the day DATE if it is not the one being counted: the day before
goes to the finished days, and to the reports when the site has asked
for them.  The caller holds the lock."
  (cond ((null *visit-today*)
         (setq *visit-today* (new-visit-day date)
               *visits-since* (or *visits-since* date)
               *visits-dirty?* t))
        ((not (equal date (day-date *visit-today*)))
         (let ((done (finished-day *visit-today*)))
           (push done *visit-days*)
           (when (> (length *visit-days*) *visit-days-kept*)
             (setq *visit-days* (subseq *visit-days* 0 *visit-days-kept*)))
           (when *visits-notify-url*
             (setq *visit-reports* (append *visit-reports* (list done)))))
         (setq *visit-today* (new-visit-day date)
               *visits-dirty?* t))))

;;; The file: one plist, written whole to a scratch file and renamed over
;;; the old one, so a crash mid-write leaves the last minute's numbers

(defun visits-plist ()
  (let ((day *visit-today*))
    (list :since *visits-since*
          :days *visit-days*
          :reports *visit-reports*
          :today (and day (list :date (day-date day) :salt (day-salt day)
                                :views (day-views day) :fetches (day-fetches day)
                                :api (day-api day) :robots (day-robots day)
                                :unverified (day-unverified day)
                                :projects (project-views day)
                                :seen (seen-digests day))))))

(defun load-visits! ()
  "Read data/visits.sexp, once per image; a missing or unreadable file
starts the counts afresh."
  (bt:with-lock-held (*visits-lock*)
    (unless *visits-loaded?*
      (let ((saved (ignore-errors (and (probe-file (visits-file)) (read-sexp-file (visits-file))))))
        (setq *visits-since* (getf saved :since)
              *visit-days* (getf saved :days)
              *visit-reports* (getf saved :reports)
              *visit-today* nil)
        (let ((today (getf saved :today)))
          (when (getf today :date)
            (let ((day (new-visit-day (getf today :date) :salt (getf today :salt))))
              (setf (day-views day) (or (getf today :views) 0)
                    (day-fetches day) (or (getf today :fetches) 0)
                    (day-api day) (or (getf today :api) 0)
                    (day-robots day) (or (getf today :robots) 0)
                    (day-unverified day) (or (getf today :unverified) 0))
              (dolist (pair (getf today :projects)) (setf (gethash (car pair) (day-projects day)) (cdr pair)))
              (dolist (digest (getf today :seen)) (setf (gethash digest (day-seen day)) t))
              (setq *visit-today* day))))
        (setq *visits-loaded?* t)
        (roll-visits!)))))

(defun save-visits! ()
  "Write data/visits.sexp when there is anything new."
  (let ((form (bt:with-lock-held (*visits-lock*)
                (when *visits-dirty?*
                  (setq *visits-dirty?* nil)
                  (visits-plist)))))
    (when form
      (let ((scratch (data-path "visits.sexp.new")))
        (handler-case
            (progn (write-sexp-file scratch form)
                   (uiop:rename-file-overwriting-target scratch (visits-file)))
          (error (e)
            (bt:with-lock-held (*visits-lock*) (setq *visits-dirty?* t))
            (log-line "visits: not saved: ~a" e)))))))

;;; Counting a request

(defun visitor-address (req)
  "The asker's address: the last X-Forwarded-For entry, the one the
proxy in front added (the ones before it are whatever the client sent),
else the socket's peer."
  (let ((forwarded (header req :x-forwarded-for)))
    (if forwarded
        (string-trim " " (car (last (uiop:split-string forwarded :separator ","))))
        (or (ignore-errors (socket:ipaddr-to-dotted (socket:remote-host (net.aserve:request-socket req))))
            "unknown"))))

(defun viewed-project (segments)
  "The namespace/project path a page's SEGMENTS show, or nil for the
forge's own pages."
  (when (and (second segments)
             (not (member (first segments) *visit-site-segments* :test #'string=))
             (not (string= (second segments) "-")))
    (format nil "~a/~a" (first segments)
            (let ((name (second segments)))
              (if (ends-with? ".git" name) (subseq name 0 (- (length name) 4)) name)))))

(defun opened-by-a-person? (req)
  "Whether REQ asks the way a browser does when a person opens a page: a
navigation (Sec-Fetch-Mode: navigate, or Sec-Fetch-Dest: document), with
the languages the person reads (Accept-Language)."
  (and (or (equalp (header req :sec-fetch-mode) "navigate")
           (equalp (header req :sec-fetch-dest) "document"))
       (header req :accept-language)
       t))

(defun note-request! (req)
  "Count REQ in today's figures, by what it is."
  (let* ((method (http-method req))
         (path (request-path req))
         (segments (path-segments path)))
    (flet ((bump (fn)
             (bt:with-lock-held (*visits-lock*)
               (roll-visits!)
               (funcall fn *visit-today*)
               (setq *visits-dirty?* t))))
      (cond ((and (eq method :get) (ends-with? "/info/refs" path)
                  (equal (url-query-value req "service") "git-upload-pack"))
             (bump (lambda (day) (incf (day-fetches day)))))
            ((equal (first segments) "api")
             (bump (lambda (day) (incf (day-api day)))))
            ((not (eq method :get)))
            ((or (equal (first segments) "static")
                 (member path '("/robots.txt" "/favicon.ico") :test #'string=)
                 (search ".git/" path)
                 (ends-with? ".git" path)))
            ((let ((agent (header req :user-agent)))
               (or (null agent) (ppcre:scan *visit-robot-agents* agent)))
             (bump (lambda (day) (incf (day-robots day)))))
            ((not (opened-by-a-person? req))
             (bump (lambda (day) (incf (day-unverified day)))))
            (t
             (let ((project (viewed-project segments))
                   (agent (header req :user-agent))
                   (address (visitor-address req)))
               (bump (lambda (day)
                       (incf (day-views day))
                       (setf (gethash (sha256-hex (format nil "~a|~a|~a" (day-salt day) address agent)) (day-seen day)) t)
                       (when project (incf (gethash project (day-projects day) 0)))))))))))

;;; The day's summary

(defun visit-report (day)
  "Values: the title and the text of the summary of DAY, a finished
day's plist."
  (let ((site (or (url-host *site-url*) *site-name*)))
    (values
     (format nil "~a, ~a: ~:d visitor~:p" site (getf day :date) (getf day :visitors))
     (with-output-to-string (out)
       (format out "~:d page view~:p by ~:d visitor~:p.~%" (getf day :views) (getf day :visitors))
       (format out "Clones and fetches: ~:d.  API calls: ~:d.~%" (getf day :fetches) (getf day :api))
       (let ((top (subseq (getf day :projects) 0 (min *visit-top-projects* (length (getf day :projects))))))
         (when top
           (format out "Most viewed: ~{~a~^, ~}.~%"
                   (mapcar (lambda (pair) (format nil "~a ~:d" (car pair) (cdr pair))) top))))
       (format out "Browser-like requests that did not look like a person's: ~:d.~%" (or (getf day :unverified) 0))
       (format out "Robots and scripts that said so: ~:d." (getf day :robots))))))

(defun post-visit-report (day)
  "Post DAY's summary to *visits-notify-url*.  True when it was taken."
  (multiple-value-bind (title text) (visit-report day)
    (multiple-value-bind (answer status)
        (ignore-errors
         (net.aserve.client:do-http-request *visits-notify-url*
           :method :post :content text :content-type "text/plain; charset=utf-8"
           :headers (list (cons "User-Agent" (format nil "anvil/~a" *version*))
                          (cons "Title" title)
                          (cons "Tags" "bar_chart"))
           :external-format :utf-8 :timeout 30 :keep-alive nil
           :ssl-args (let ((host (url-host *visits-notify-url*))) (and host (list :server-name host)))))
      (declare (ignore answer))
      (and (integerp status) (<= 200 status 299)))))

(defun visits-housekeeping! ()
  "Every minute: read the file the first time, start a new day when the
date has changed (even with nobody asking), write what is new, and post
the summaries waiting.  A summary that cannot be posted waits for the
next minute; one more than a week old is dropped."
  (load-visits!)
  (bt:with-lock-held (*visits-lock*) (roll-visits!))
  (save-visits!)
  (do () ((not (post-next-visit-report!))))
  (save-visits!))

(defun post-next-visit-report! ()
  "Post the oldest summary waiting, or drop it when it is too old to
matter.  True when one went, so the next may follow; false when none is
waiting or the one waiting could not be posted."
  (let ((day (bt:with-lock-held (*visits-lock*) (first *visit-reports*))))
    (cond ((null day) nil)
          ((not *visits-notify-url*)
           (bt:with-lock-held (*visits-lock*) (setq *visit-reports* nil *visits-dirty?* t))
           nil)
          ((post-visit-report day)
           (log-line "visits: the summary of ~a posted" (getf day :date))
           (bt:with-lock-held (*visits-lock*) (pop *visit-reports*) (setq *visits-dirty?* t))
           t)
          ((let ((time (parse-iso-timestamp (format nil "~aT00:00:00Z" (getf day :date)))))
             (and time (> (- (get-universal-time) time) (* 8 24 3600))))
           (log-line "visits: the summary of ~a dropped, never taken" (getf day :date))
           (bt:with-lock-held (*visits-lock*) (pop *visit-reports*) (setq *visits-dirty?* t))
           t)
          (t nil))))

(defun visits-summary (&key (days 7))
  "At the REPL: today's figures so far and the last DAYS finished days,
as plists, newest first."
  (load-visits!)
  (bt:with-lock-held (*visits-lock*)
    (roll-visits!)
    (cons (finished-day *visit-today*)
          (subseq *visit-days* 0 (min days (length *visit-days*))))))
