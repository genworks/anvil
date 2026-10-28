;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :anvil)

;;
;; What the forge knows about the code in a tree, from the tree alone:
;; which languages it is written in (by file name, as Linguist and
;; GitLab's /languages count them: programming and markup languages,
;; not prose or data, and not what is vendored), and where a word
;; occurs in it (the search).
;;

;;; Languages

(defparameter *languages*
  ;; name, color (Linguist's), kind, then extensions and whole file names
  '(("Common Lisp" "#3fb68b" :programming ".lisp" ".lsp" ".cl" ".asd" ".gendl" ".gdl")
    ("Emacs Lisp" "#c065db" :programming ".el")
    ("Scheme" "#1e4aec" :programming ".scm" ".ss" ".sld")
    ("Clojure" "#db5855" :programming ".clj" ".cljs" ".cljc" ".edn")
    ("Racket" "#3c5caa" :programming ".rkt")
    ("C" "#555555" :programming ".c" ".h")
    ("C++" "#f34b7d" :programming ".cpp" ".cc" ".cxx" ".hpp" ".hh")
    ("C#" "#178600" :programming ".cs")
    ("Objective-C" "#438eff" :programming ".m")
    ("Python" "#3572A5" :programming ".py")
    ("JavaScript" "#f1e05a" :programming ".js" ".mjs" ".cjs")
    ("TypeScript" "#3178c6" :programming ".ts" ".tsx")
    ("Ruby" "#701516" :programming ".rb")
    ("Perl" "#0298c3" :programming ".pl" ".pm")
    ("PHP" "#4F5D95" :programming ".php")
    ("Java" "#b07219" :programming ".java")
    ("Kotlin" "#A97BFF" :programming ".kt")
    ("Scala" "#c22d40" :programming ".scala")
    ("Go" "#00ADD8" :programming ".go")
    ("Rust" "#dea584" :programming ".rs")
    ("Swift" "#F05138" :programming ".swift")
    ("Haskell" "#5e5086" :programming ".hs")
    ("OCaml" "#ef7a08" :programming ".ml" ".mli")
    ("Erlang" "#B83998" :programming ".erl")
    ("Elixir" "#6e4a7e" :programming ".ex" ".exs")
    ("Lua" "#000080" :programming ".lua")
    ("R" "#198CE7" :programming ".r")
    ("Julia" "#a270ba" :programming ".jl")
    ("Dart" "#00B4AB" :programming ".dart")
    ("Zig" "#ec915c" :programming ".zig")
    ("Nim" "#ffc200" :programming ".nim")
    ("Fortran" "#4d41b1" :programming ".f" ".f90" ".f95")
    ("Assembly" "#6E4C13" :programming ".s" ".asm")
    ("Shell" "#89e051" :programming ".sh" ".bash" ".zsh")
    ("PowerShell" "#012456" :programming ".ps1")
    ("Vim Script" "#199f4b" :programming ".vim")
    ("Nix" "#7e7eff" :programming ".nix")
    ("SQL" "#e38c00" :programming ".sql")
    ("Makefile" "#427819" :programming ".mk" "makefile" "gnumakefile")
    ("Dockerfile" "#384d54" :programming "dockerfile" "containerfile")
    ("CMake" "#DA3434" :programming ".cmake" "cmakelists.txt")
    ("HTML" "#e34c26" :markup ".html" ".htm")
    ("CSS" "#563d7c" :markup ".css")
    ("SCSS" "#c6538c" :markup ".scss")
    ("TeX" "#3D6117" :markup ".tex" ".sty" ".cls")
    ("Markdown" "#083fa1" :prose ".md" ".markdown")
    ("Org" "#77aa99" :prose ".org")
    ("reStructuredText" "#141414" :prose ".rst")
    ("YAML" "#cb171e" :data ".yml" ".yaml")
    ("JSON" "#292929" :data ".json")
    ("TOML" "#9c4221" :data ".toml")
    ("XML" "#0060ac" :data ".xml" ".xsd" ".xsl"))
  "The languages the forge names, Linguist's names and colors: for
each, the kind (only :programming and :markup are counted, as GitLab
counts them), and the file extensions and whole names that mean it.")

(defparameter *vendored-segments* '("node_modules" "vendor" "third_party" "third-party" "dist" "bower_components" ".yarn")
  "Directory names whose contents are somebody else's, left out of the languages.")

(defun language-of (path)
  "The *languages* entry PATH's name says it is written in, or nil."
  (let* ((name (string-downcase (ingot:last-path-part path)))
         (dot (position #\. name :from-end t))
         (extension (and dot (subseq name dot))))
    (find-if (lambda (entry)
               (let ((marks (cdddr entry)))
                 (or (member name marks :test #'string=)
                     (and extension (member extension marks :test #'string=)))))
             *languages*)))

(defun vendored-path? (path)
  (let ((lower (string-downcase path)))
    (or (some (lambda (segment) (search (format nil "/~a/" segment) (strcat "/" lower))) *vendored-segments*)
        (some (lambda (suffix) (ends-with? suffix lower)) '(".min.js" ".min.css" ".map")))))

(defun tree-languages (repository tree-oid)
  "An alist of (name color . bytes) for the programming and markup
languages of the files under TREE-OID, the largest first.  Every blob
counted is read for its size; a project view keeps the answer."
  (let ((totals (make-hash-table :test #'equal)))
    (loop for (path . entry) in (ingot:tree-paths repository tree-oid)
          for language = (and (member (ingot:tree-entry-kind entry) '(:blob :executable))
                              (not (vendored-path? path))
                              (language-of path))
          when (and language (member (third language) '(:programming :markup)))
            do (let ((object (ingot:find-object repository (ingot:tree-entry-oid entry))))
                 (when object
                   (incf (gethash (first language) totals 0) (ingot:object-size object)))))
    (let ((rows nil))
      (maphash (lambda (name bytes)
                 (when (plusp bytes)
                   (push (list* name (second (assoc name *languages* :test #'string=)) bytes) rows)))
               totals)
      (sort rows #'> :key #'cddr))))

(defun language-percentages (languages)
  "LANGUAGES (tree-languages' answer) as (name . percent), percents to
two decimals, as GitLab's /languages answers."
  (let ((total (reduce #'+ languages :key #'cddr :initial-value 0)))
    (if (zerop total)
        nil
        (mapcar (lambda (row) (cons (first row) (/ (round (* 10000 (cddr row)) total) 100.0))) languages))))

;;; Search

(defparameter *search-blob-limit* (* 1024 1024)
  "Files larger than this are not searched.")

(defun search-blobs (repository commit-oid query &key (limit 100))
  "Where QUERY occurs, letter case aside, in the text files of the tree
at COMMIT-OID: a list of (:path ... :line ... :text ...) in tree order,
LIMIT of them at most, one entry per matching line."
  (let ((found nil) (count 0)
        (tree (ingot:commit-tree (ingot:read-commit repository commit-oid))))
    (when (plusp (length (trim query)))
      (block walk
        (loop for (path . entry) in (ingot:tree-paths repository tree)
              when (member (ingot:tree-entry-kind entry) '(:blob :executable))
                do (let ((object (ingot:find-object repository (ingot:tree-entry-oid entry))))
                     (when (and object (<= (ingot:object-size object) *search-blob-limit*)
                                (not (ingot:binary-octets? (ingot:object-data object))))
                       (loop for line in (ingot::split-lines (ingot:octets-to-string (ingot:object-data object)))
                             for n from 1
                             when (search query line :test #'char-equal)
                               do (push (list :path path :line n :text line) found)
                                  (when (>= (incf count) limit) (return-from walk))))))))
    (nreverse found)))

(defun search-things (project kind query)
  "The issues or merge requests (KIND) of PROJECT whose title or
description holds QUERY, letter case aside."
  (remove-if-not (lambda (thing)
                   (or (search query (getf thing :title) :test #'char-equal)
                       (search query (or (getf thing :description) "") :test #'char-equal)))
                 (if (eq kind :issue) (project-issues project :state :all) (project-merge-requests project :state :all))))

(defun search-commits (repository commit-oid query &key (limit 50))
  "The commits reachable from COMMIT-OID whose message holds QUERY."
  (let ((found nil))
    (dolist (commit (ingot:log-commits repository commit-oid :limit 2000))
      (when (search query (ingot:commit-message commit) :test #'char-equal)
        (push commit found)
        (when (>= (length found) limit) (return))))
    (nreverse found)))
