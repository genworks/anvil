;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :anvil)

;;
;; The store: every record the forge keeps apart from the repositories.
;; A record is a plist with :kind and :id, kept in memory by kind and
;; id and written whole to <data>/db/<kind>/<id>.sexp when it changes.
;; At start every file is read.  Records are treated as values: a
;; change makes a new plist and saves it (`update-record!'); readers
;; holding the old one see the old one.
;;
;; Secondary indexes (a user by name, a project by path) are rebuilt
;; from the records at load and kept by `index-record!' on every save.
;;

(defvar *tables* (make-hash-table :test #'eq) "kind -> (id -> record)")
(defvar *indexes* (make-hash-table :test #'equal) "(kind . index-name) -> (key -> record)")
(defvar *index-definitions* nil "List of (kind name key-function)")
(defvar *counters* (make-hash-table :test #'equal))
(defvar *store-lock* (bt:make-recursive-lock "anvil store"))
(defvar *store-loaded?* nil)

(defmacro with-store (() &body body)
  `(bt:with-recursive-lock-held (*store-lock*) ,@body))

(defun data-path (relative)
  (merge-pathnames relative *data-directory*))

(defun kind-directory (kind)
  (data-path (format nil "db/~(~a~)/" kind)))

(defun record-file (kind id)
  (merge-pathnames (format nil "~a.sexp" id) (kind-directory kind)))

(defun table (kind)
  (or (gethash kind *tables*)
      (setf (gethash kind *tables*) (make-hash-table :test #'eql))))

(defun define-index (kind name key-function)
  "Index records of KIND under NAME by KEY-FUNCTION (a record -> a key
compared with equal, or nil for none)."
  (setf *index-definitions* (remove-if (lambda (d) (and (eq (first d) kind) (eq (second d) name))) *index-definitions*))
  (push (list kind name key-function) *index-definitions*)
  (setf (gethash (cons kind name) *indexes*) (make-hash-table :test #'equal)))

(defun index-table (kind name)
  (or (gethash (cons kind name) *indexes*)
      (error "No index ~a on ~a." name kind)))

(defun index-record! (record &optional old)
  (let ((kind (getf record :kind)))
    (dolist (definition *index-definitions*)
      (when (eq (first definition) kind)
        (let ((table (index-table kind (second definition)))
              (old-key (and old (funcall (third definition) old)))
              (new-key (funcall (third definition) record)))
          (when (and old-key (not (equal old-key new-key)))
            (remhash old-key table))
          (when new-key (setf (gethash new-key table) record)))))))

(defun unindex-record! (record)
  (let ((kind (getf record :kind)))
    (dolist (definition *index-definitions*)
      (when (eq (first definition) kind)
        (let ((key (funcall (third definition) record)))
          (when key (remhash key (index-table kind (second definition)))))))))

(defun lookup (kind index key)
  "The record of KIND whose INDEX key is KEY, or nil."
  (and key (with-store () (gethash key (index-table kind index)))))

;;; Reading and writing files

(defun write-sexp-file (file form)
  (ingot:write-file-octets
   file
   (ingot:string-to-octets
    (with-standard-io-syntax
      (let ((*print-readably* nil) (*print-pretty* t) (*print-right-margin* 100)
            (*package* (find-package :keyword)))
        (prin1-to-string form))))))

(defun read-sexp-file (file)
  (with-standard-io-syntax
    (let ((*read-eval* nil) (*package* (find-package :keyword)))
      (with-open-file (in file :external-format :utf-8)
        (read in nil nil)))))

(defun save-record! (record)
  "Keep RECORD (a plist with :kind and :id) in memory and on disk;
answers it."
  (let ((kind (getf record :kind)) (id (getf record :id)))
    (unless (and kind id) (error "A record needs a :kind and an :id: ~s" record))
    (with-store ()
      (let ((old (gethash id (table kind))))
        (setf (gethash id (table kind)) record)
        (index-record! record old))
      (write-sexp-file (record-file kind id) record))
    record))

(defun delete-record! (record)
  (let ((kind (getf record :kind)) (id (getf record :id)))
    (with-store ()
      (remhash id (table kind))
      (unindex-record! record)
      (let ((file (probe-file (record-file kind id))))
        (when file (delete-file file))))
    t))

(defun record (kind id)
  "The record of KIND with ID, or nil."
  (and (integerp id) (with-store () (gethash id (table kind)))))

(defun records (kind &optional predicate)
  "Every record of KIND (those PREDICATE accepts), newest id first."
  (with-store ()
    (let ((found nil))
      (maphash (lambda (id record)
                 (declare (ignore id))
                 (when (or (null predicate) (funcall predicate record))
                   (push record found)))
               (table kind))
      (sort found #'> :key (lambda (r) (getf r :id))))))

(defun find-record (kind predicate)
  (with-store ()
    (block found
      (maphash (lambda (id record) (declare (ignore id)) (when (funcall predicate record) (return-from found record)))
               (table kind))
      nil)))

(defun count-records (kind &optional predicate)
  (length (records kind predicate)))

(defun update-record! (record &rest keys-and-values)
  "A copy of RECORD with KEYS-AND-VALUES set, saved; answers the copy.
The record in the table is read again first, so two updaters of
different fields do not lose each other's."
  (with-store ()
    (let* ((current (or (record (getf record :kind) (getf record :id)) record))
           (new (copy-list current)))
      (loop for (key value) on keys-and-values by #'cddr
            do (setf (getf new key) value))
      (setf (getf new :updated) (get-universal-time))
      (save-record! new))))

;;; Counters

(defun counters-file () (data-path "db/counters.sexp"))

(defun next-id (name)
  "The next number in the sequence NAME (a keyword, or a string for a
project's own sequences), kept across restarts."
  (with-store ()
    (let ((value (1+ (gethash name *counters* 0))))
      (setf (gethash name *counters*) value)
      (let ((all nil))
        (maphash (lambda (k v) (push (list k v) all)) *counters*)
        (write-sexp-file (counters-file) all))
      value)))

(defun ensure-counter! (name value)
  "The sequence NAME continues above VALUE from now on: an import keeps
the numbers things had.  Answers the counter."
  (with-store ()
    (when (< (gethash name *counters* 0) value)
      (setf (gethash name *counters*) value)
      (let ((all nil))
        (maphash (lambda (k v) (push (list k v) all)) *counters*)
        (write-sexp-file (counters-file) all)))
    (gethash name *counters*)))

(defun new-record (kind &rest keys-and-values)
  "A fresh record of KIND with an id of its own and :created, not yet
saved."
  (list* :kind kind :id (next-id kind) :created (get-universal-time) keys-and-values))

;;; Loading

(defun load-store! ()
  "Read every record under <data>/db/ into memory; safe to call again
(the tables are emptied first)."
  (with-store ()
    (clrhash *tables*)
    (maphash (lambda (key table) (declare (ignore key)) (clrhash table)) *indexes*)
    (clrhash *counters*)
    (let ((db (data-path "db/")))
      (ensure-directories-exist db)
      (when (probe-file (counters-file))
        (dolist (entry (ignore-errors (read-sexp-file (counters-file))))
          (setf (gethash (first entry) *counters*) (second entry))))
      (dolist (dir (uiop:subdirectories db))
        (let ((kind (intern (string-upcase (car (last (pathname-directory dir)))) :keyword)))
          (dolist (file (uiop:directory-files dir "*.sexp"))
            (let ((record (ignore-errors (read-sexp-file file))))
              (when (and (consp record) (getf record :id))
                (setf (gethash (getf record :id) (table kind)) record)
                (index-record! record)))))))
    (setq *store-loaded?* t)
    (let ((counts nil))
      (maphash (lambda (kind table) (push (cons kind (hash-table-count table)) counts)) *tables*)
      counts)))
