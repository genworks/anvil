;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :anvil)

;;
;; Markdown to HTML (3bmd, with fenced code blocks), then a pass that
;; keeps only the tags and attributes a README or a comment needs:
;; what a stranger wrote is never put on a page as it came.  Relative
;; links in a README are made to point into the repository.
;;

(defparameter *allowed-tags*
  '("p" "br" "hr" "h1" "h2" "h3" "h4" "h5" "h6" "blockquote" "pre" "code" "em" "strong" "b" "i" "u" "s" "del"
    "ul" "ol" "li" "dl" "dt" "dd" "a" "img" "table" "thead" "tbody" "tr" "th" "td" "sup" "sub" "kbd"
    "details" "summary" "span" "div" "input"))

(defparameter *allowed-attributes*
  '(("a" "href" "title") ("img" "src" "alt" "title" "width" "height") ("code" "class") ("pre" "class")
    ("th" "align") ("td" "align") ("input" "type" "checked" "disabled") ("span" "class") ("div" "class")
    ("ol" "start") ("li" "class")))

(defun render-markdown-raw (text)
  (let ((text (if (> (length text) *markdown-limit*) (subseq text 0 *markdown-limit*) text)))
    (handler-case
        (progn
          (let ((blocks (find-symbol "*CODE-BLOCKS*" :3bmd-code-blocks)))
            (when blocks (setf (symbol-value blocks) t)))
          (with-output-to-string (out)
            (3bmd:parse-string-and-print-to-stream text out)))
      (error () (format nil "<pre>~a</pre>" (html-escape text))))))

(defun safe-url? (url)
  "A link target that is not a script: http, https, mailto, a relative
path, or a fragment."
  (let ((url (trim url)))
    (or (zerop (length url))
        (char= (char url 0) #\#) (char= (char url 0) #\/) (char= (char url 0) #\.)
        (starts-with? "http://" url) (starts-with? "https://" url) (starts-with? "mailto:" url)
        (not (position #\: (subseq url 0 (min (length url) 12)))))))

(defun resolve-relative (url base-url)
  "URL made absolute within the site when it is relative to BASE-URL."
  (cond ((or (null base-url) (zerop (length url)) (position #\: url) (char= (char url 0) #\/) (char= (char url 0) #\#)) url)
        (t (let ((clean (if (starts-with? "./" url) (subseq url 2) url)))
             (strcat base-url "/" clean)))))

(defun sanitize-html (html &key base-url raw-base-url)
  "HTML with every tag not in *allowed-tags* removed (its text kept) and
every attribute not allowed for its tag dropped; links to scripts
dropped; relative links resolved."
  (let ((out (make-string-output-stream))
        (at 0) (n (length html)))
    (loop while (< at n)
          do (let ((lt (position #\< html :start at)))
               (cond ((null lt) (write-string html out :start at) (setq at n))
                     (t (write-string html out :start at :end lt)
                        (let ((gt (position #\> html :start lt)))
                          (cond ((null gt) (write-string (html-escape (subseq html lt)) out) (setq at n))
                                (t (write-string (sanitize-tag (subseq html lt (1+ gt)) base-url raw-base-url) out)
                                   (setq at (1+ gt)))))))))
    (get-output-stream-string out)))

(defun sanitize-tag (tag base-url raw-base-url)
  "TAG (\"<a href=...>\") kept, trimmed or dropped."
  (multiple-value-bind (match groups) (ppcre:scan-to-strings "^<(/?)([a-zA-Z][a-zA-Z0-9]*)(.*?)(/?)>$" tag :sharedp nil)
    (cond ((null match) (if (starts-with? "<!--" tag) "" (html-escape tag)))
          (t (let* ((closing? (string= (aref groups 0) "/"))
                    (name (string-downcase (aref groups 1)))
                    (attributes (aref groups 2))
                    (self-closing? (string= (aref groups 3) "/")))
               (cond ((not (member name *allowed-tags* :test #'string=)) "")
                     (closing? (format nil "</~a>" name))
                     (t (let ((allowed (cdr (assoc name *allowed-attributes* :test #'string=)))
                              (kept nil))
                          (ppcre:do-register-groups (attr value1 value2 value3)
                              ("([a-zA-Z-]+)\\s*=\\s*(?:\"([^\"]*)\"|'([^']*)'|([^\\s\"'>]+))" attributes)
                            (let ((attr (string-downcase attr))
                                  (value (or value1 value2 value3 "")))
                              (when (member attr allowed :test #'string=)
                                (cond ((member attr '("href" "src") :test #'string=)
                                       (when (safe-url? value)
                                         (push (cons attr (resolve-relative value (if (string= attr "src") raw-base-url base-url))) kept)))
                                      ((and (string= name "input") (string= attr "type") (not (string-equal value "checkbox"))))
                                      (t (push (cons attr value) kept))))))
                          (when (string= name "input") (push (cons "disabled" "disabled") kept))
                          (when (and (string= name "a") (assoc "href" kept :test #'string=)
                                     (starts-with? "http" (cdr (assoc "href" kept :test #'string=))))
                            (push (cons "rel" "nofollow noopener") kept))
                          (format nil "<~a~{ ~a~}~:[~;/~]>" name
                                  (mapcar (lambda (pair) (format nil "~a=\"~a\"" (car pair) (html-escape (cdr pair)))) (reverse kept))
                                  self-closing?)))))))))

(defun markdown-html (text &key base-url raw-base-url)
  "TEXT (Markdown) as safe HTML.  BASE-URL is where a relative link
points (the directory of the file as a blob address); RAW-BASE-URL the
same for an image."
  (sanitize-html (render-markdown-raw (or text "")) :base-url base-url :raw-base-url raw-base-url))

(defun markdown-file? (path)
  (let ((lower (string-downcase path)))
    (or (ends-with? ".md" lower) (ends-with? ".markdown" lower) (ends-with? ".mkd" lower))))

(defun readme-entry (entries)
  "The README among tree ENTRIES, preferring Markdown."
  (or (find-if (lambda (e) (and (string-equal (pathname-name (ingot:tree-entry-name e)) "readme")
                                (markdown-file? (ingot:tree-entry-name e))))
               entries)
      (find-if (lambda (e) (string-equal (ingot:tree-entry-name e) "README")) entries)
      (find-if (lambda (e) (starts-with? "readme" (string-downcase (ingot:tree-entry-name e)))) entries)))
