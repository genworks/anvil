;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :anvil)

;;
;; A YAML reader for CI files: block mappings and sequences, flow
;; collections, plain, quoted, literal (|) and folded (>) scalars,
;; anchors, aliases and the merge key (<<).  It refuses, with a line
;; number, what it does not understand (tags such as !reference,
;; complex keys, multiple documents) rather than guess.
;;
;; Mappings come back as `ymap' structures (their pairs in file order,
;; which is the order of jobs), sequences as lists, strings as strings,
;; numbers as numbers, booleans as t and :false, null as :null.
;;

(defstruct (ymap (:constructor make-ymap (&optional pairs)))
  (pairs nil))

(defun yget (map key &optional default)
  "The value under KEY in MAP (a ymap), or DEFAULT; :null counts as absent."
  (if (ymap-p map)
      (let ((pair (assoc key (ymap-pairs map) :test #'equal)))
        (if (and pair (not (eq (cdr pair) :null))) (cdr pair) default))
      default))

(defun yhas? (map key)
  (and (ymap-p map) (assoc key (ymap-pairs map) :test #'equal) t))

(defun ykeys (map) (and (ymap-p map) (mapcar #'car (ymap-pairs map))))

(defun yset (map key value)
  "MAP with KEY set to VALUE (in place; keeps the order of the keys)."
  (let ((pair (assoc key (ymap-pairs map) :test #'equal)))
    (if pair (setf (cdr pair) value) (setf (ymap-pairs map) (append (ymap-pairs map) (list (cons key value)))))
    map))

(define-condition yaml-error (error)
  ((line :initarg :line :reader yaml-error-line)
   (text :initarg :text :reader yaml-error-text))
  (:report (lambda (c s) (format s "YAML, line ~a: ~a" (yaml-error-line c) (yaml-error-text c)))))

(defun yaml-error (line control &rest args)
  (error 'yaml-error :line line :text (apply #'format nil control args)))

;;; Lines

(defstruct yline number indent text)

(defun strip-comment (text)
  "TEXT without a trailing comment: a # that begins the text or follows
a space and is not inside quotes."
  (let ((in-single nil) (in-double nil))
    (loop for i from 0 below (length text)
          for c = (char text i)
          do (cond (in-double (cond ((char= c #\\) (incf i)) ((char= c #\") (setq in-double nil))))
                   (in-single (when (char= c #\') (setq in-single nil)))
                   ((char= c #\") (setq in-double t))
                   ((char= c #\') (when (or (zerop i) (not (alphanumericp (char text (1- i))))) (setq in-single t)))
                   ((and (char= c #\#) (or (zerop i) (member (char text (1- i)) '(#\Space #\Tab))))
                    (return-from strip-comment (string-right-trim '(#\Space #\Tab) (subseq text 0 i))))))
    (string-right-trim '(#\Space #\Tab) text)))

(defun yaml-lines (text)
  "The lines of TEXT as ylines, tabs refused, with their raw text (the
comments are stripped where a line is used, since a block scalar keeps
them)."
  (let ((lines nil) (number 0))
    (dolist (raw (uiop:split-string text :separator '(#\Newline)))
      (incf number)
      (let* ((raw (string-right-trim '(#\Return) raw))
             (indent (or (position-if-not (lambda (c) (char= c #\Space)) raw) (length raw))))
        (when (and (< indent (length raw)) (char= (char raw indent) #\Tab))
          (yaml-error number "a tab in the indentation"))
        (push (make-yline :number number :indent indent :text raw) lines)))
    (coerce (nreverse lines) 'simple-vector)))

;;; The parser: a cursor over the lines

(defvar *ylines* nil)
(defvar *yat* 0)
(defvar *yanchors* nil)

(defun ycurrent () (and (< *yat* (length *ylines*)) (svref *ylines* *yat*)))

(defun yline-content (line) (strip-comment (string-left-trim " " (yline-text line))))

(defun blank-line? (line) (zerop (length (yline-content line))))

(defun skip-blank-lines ()
  (loop while (and (ycurrent) (blank-line? (ycurrent))) do (incf *yat*)))

(defun parse-yaml (text)
  "The document in TEXT as Lisp data (see the top of this file)."
  (let ((*ylines* (yaml-lines text)) (*yat* 0) (*yanchors* nil))
    ;; a document marker, and a %YAML directive, are skipped
    (loop while (and (ycurrent)
                     (let ((content (yline-content (ycurrent))))
                       (or (zerop (length content)) (string= content "---") (starts-with? "%" content))))
          do (incf *yat*))
    (if (null (ycurrent))
        (make-ymap)
        (let ((value (parse-node (yline-indent (ycurrent)))))
          (skip-blank-lines)
          (when (ycurrent)
            (let ((content (yline-content (ycurrent))))
              (unless (or (string= content "...") (string= content "---"))
                (yaml-error (yline-number (ycurrent)) "unexpected text: ~a" content))))
          value))))

(defun sequence-item? (content)
  (or (string= content "-") (starts-with? "- " content)))

(defun mapping-key-end (content)
  "The index of the colon that ends a mapping key in CONTENT, or nil:
a colon followed by a space or the end, outside quotes and brackets."
  (cond ((zerop (length content)) nil)
        ((char= (char content 0) #\")
         (let ((close (quoted-end content 0 #\")))
           (and close (< (1+ close) (length content)) (char= (char content (1+ close)) #\:)
                (or (= (+ close 2) (length content)) (char= (char content (+ close 2)) #\Space))
                (1+ close))))
        ((char= (char content 0) #\')
         (let ((close (quoted-end content 0 #\')))
           (and close (< (1+ close) (length content)) (char= (char content (1+ close)) #\:)
                (or (= (+ close 2) (length content)) (char= (char content (+ close 2)) #\Space))
                (1+ close))))
        ((member (char content 0) '(#\[ #\{ #\&  #\* #\!))
         ;; an anchor before a key: &x key: value
         (if (char= (char content 0) #\&)
             (let ((space (position #\Space content)))
               (and space (mapping-key-end (subseq content (1+ space)))
                    (+ (1+ space) (mapping-key-end (subseq content (1+ space))))))
             nil))
        (t (loop for i from 0 below (length content)
                 for c = (char content i)
                 when (and (char= c #\:) (or (= (1+ i) (length content)) (char= (char content (1+ i)) #\Space)))
                   return i
                 when (char= c #\#) return nil))))

(defun quoted-end (text start quote)
  "The index of the quote closing the one at START, or nil."
  (loop with i = (1+ start)
        while (< i (length text))
        do (let ((c (char text i)))
             (cond ((and (char= quote #\") (char= c #\\)) (incf i 2))
                   ((char= c quote)
                    (if (and (char= quote #\') (< (1+ i) (length text)) (char= (char text (1+ i)) #\'))
                        (incf i 2)
                        (return i)))
                   (t (incf i))))))

(defun parse-node (indent)
  "The node that begins at the current line, which is indented INDENT."
  (skip-blank-lines)
  (let* ((line (ycurrent))
         (content (and line (yline-content line))))
    (cond ((null line) :null)
          ((< (yline-indent line) indent) :null)
          ((sequence-item? content) (parse-block-sequence (yline-indent line)))
          ((mapping-key-end content) (parse-block-mapping (yline-indent line)))
          (t (parse-scalar-lines indent)))))

(defun parse-block-sequence (indent)
  (let ((items nil))
    (loop
      (skip-blank-lines)
      (let ((line (ycurrent)))
        (when (or (null line) (/= (yline-indent line) indent) (not (sequence-item? (yline-content line))))
          (return))
        (let* ((content (yline-content line))
               (rest (string-left-trim " " (subseq content 1))))
          (cond ((zerop (length rest))
                 (incf *yat*)
                 (push (parse-node (1+ indent)) items))
                (t
                 ;; the item begins on this line: rewrite the line as if indented past the dash
                 (let ((dash-indent (+ indent 1 (- (length (subseq content 1)) (length rest)))))
                   (setf (svref *ylines* *yat*)
                         (make-yline :number (yline-number line) :indent dash-indent
                                     :text (strcat (make-string dash-indent :initial-element #\Space) rest)))
                   (push (parse-node dash-indent) items)))))))
    (nreverse items)))

(defun parse-block-mapping (indent)
  (let ((map (make-ymap)))
    (loop
      (skip-blank-lines)
      (let ((line (ycurrent)))
        (when (or (null line) (/= (yline-indent line) indent)) (return))
        (let* ((content (yline-content line))
               (colon (mapping-key-end content)))
          (when (sequence-item? content) (return))
          (unless colon (yaml-error (yline-number line) "expected a key: ~a" content))
          (let* ((key-text (subseq content 0 colon))
                 (anchor nil)
                 (key-text (if (char= (char key-text 0) #\&)
                               (let ((space (position #\Space key-text)))
                                 (setq anchor (subseq key-text 1 space))
                                 (string-left-trim " " (subseq key-text space)))
                               key-text))
                 (key (parse-plain-key key-text (yline-number line)))
                 (rest (string-left-trim " " (subseq content (1+ colon))))
                 (value nil))
            (cond ((zerop (length rest))
                   (incf *yat*)
                   (skip-blank-lines)
                   (let ((next (ycurrent)))
                     (setq value
                           (cond ((null next) :null)
                                 ;; a sequence may sit at the key's own indent
                                 ((and (= (yline-indent next) indent) (sequence-item? (yline-content next)))
                                  (parse-block-sequence indent))
                                 ((> (yline-indent next) indent) (parse-node (yline-indent next)))
                                 (t :null)))))
                  (t (setq value (parse-inline-value rest line indent))))
            (when anchor (push (cons anchor value) *yanchors*))
            (cond ((equal key "<<") (merge-into! map value (yline-number line)))
                  (t (yset map key value)))))))
    map))

(defun merge-into! (map value line)
  "The merge key: VALUE (a ymap, or a list of them) folded into MAP;
keys MAP has keep their own values."
  (dolist (source (if (listp value) value (list value)))
    (unless (ymap-p source) (yaml-error line "<< takes a mapping or a list of them"))
    (dolist (pair (ymap-pairs source))
      (unless (yhas? map (car pair))
        (yset map (car pair) (cdr pair))))))

(defun parse-plain-key (text line)
  (let ((text (string-trim " " text)))
    (cond ((zerop (length text)) (yaml-error line "an empty key"))
          ((char= (char text 0) #\") (parse-double-quoted text line))
          ((char= (char text 0) #\') (parse-single-quoted text line))
          ((char= (char text 0) #\?) (yaml-error line "complex keys (?) are not supported"))
          (t text))))

(defun parse-inline-value (rest line indent)
  "The value written after `key: ' or `- ' on LINE: a flow collection,
a quoted scalar, a block scalar's header, an alias, an anchor, or a
plain scalar (which may continue on the lines below)."
  (let ((number (yline-number line)))
    (cond
      ((char= (char rest 0) #\&)
       (let* ((space (position #\Space rest))
              (anchor (subseq rest 1 space)))
         (cond (space
                (let ((value (parse-inline-value (string-left-trim " " (subseq rest space)) line indent)))
                  (push (cons anchor value) *yanchors*)
                  value))
               (t (incf *yat*)
                  (let ((value (parse-node (1+ indent))))
                    (push (cons anchor value) *yanchors*)
                    value)))))
      ((char= (char rest 0) #\*)
       (incf *yat*)
       (let ((found (assoc (subseq rest 1) *yanchors* :test #'string=)))
         (unless found (yaml-error number "no anchor ~a" (subseq rest 1)))
         (cdr found)))
      ((char= (char rest 0) #\!)
       (yaml-error number "tags (~a) are not supported" (subseq rest 0 (or (position #\Space rest) (length rest)))))
      ((or (char= (char rest 0) #\|) (char= (char rest 0) #\>))
       (incf *yat*)
       (parse-block-scalar rest indent))
      ((char= (char rest 0) #\[) (incf *yat*) (parse-flow (strcat rest (continuation-lines indent)) number))
      ((char= (char rest 0) #\{) (incf *yat*) (parse-flow (strcat rest (continuation-lines indent)) number))
      ((char= (char rest 0) #\") (incf *yat*) (parse-double-quoted (strcat rest (continuation-lines indent)) number))
      ((char= (char rest 0) #\') (incf *yat*) (parse-single-quoted (strcat rest (continuation-lines indent)) number))
      (t (incf *yat*)
         (plain-scalar-value (strcat rest (continuation-lines indent :fold t)))))))

(defun continuation-lines (indent &key fold)
  "The lines after the current one that continue a scalar: indented
more than INDENT and not a key or an item at that depth.  Joined with a
space when FOLD, else as they are."
  (let ((out (make-string-output-stream)))
    (loop
      (let ((line (ycurrent)))
        (when (or (null line) (blank-line? line)
                  (<= (yline-indent line) indent))
          (return))
        (let ((content (yline-content line)))
          (when (or (mapping-key-end content) (sequence-item? content))
            (return))
          (write-string (if fold " " " ") out)
          (write-string content out)
          (incf *yat*))))
    (get-output-stream-string out)))

(defun parse-scalar-lines (indent)
  "A scalar standing on lines of its own (after a key with nothing on
its line): the lines at INDENT or more, folded."
  (let ((line (ycurrent)))
    (incf *yat*)
    (let ((text (strcat (yline-content line) (continuation-lines (1- indent) :fold t))))
      (cond ((char= (char text 0) #\") (parse-double-quoted text (yline-number line)))
            ((char= (char text 0) #\') (parse-single-quoted text (yline-number line)))
            ((char= (char text 0) #\[) (parse-flow text (yline-number line)))
            ((char= (char text 0) #\{) (parse-flow text (yline-number line)))
            (t (plain-scalar-value text))))))

(defun parse-block-scalar (header indent)
  "A literal (|) or folded (>) scalar: the lines indented more than
INDENT that follow, with the chomping the header asks."
  (let* ((literal? (char= (char header 0) #\|))
         (chomp (cond ((find #\- header) :strip) ((find #\+ header) :keep) (t :clip)))
         (lines nil) (block-indent nil))
    (loop
      (let ((line (ycurrent)))
        (when (null line) (return))
        (let ((raw (yline-text line)))
          (cond ((zerop (length (string-trim " " raw)))
                 (push "" lines) (incf *yat*))
                ((<= (yline-indent line) indent) (return))
                (t (unless block-indent (setq block-indent (yline-indent line)))
                   (when (< (yline-indent line) block-indent) (return))
                   (push (subseq raw block-indent) lines)
                   (incf *yat*))))))
    (setq lines (nreverse lines))
    ;; trailing blank lines are the chomping's business
    (let* ((trailing (loop for l in (reverse lines) while (string= l "") count t))
           (body (subseq lines 0 (- (length lines) trailing)))
           (text (if literal?
                     (format nil "~{~a~^~%~}" body)
                     (fold-lines body))))
      (ecase chomp
        (:strip text)
        (:clip (if body (strcat text (string #\Newline)) ""))
        (:keep (strcat text (make-string (1+ trailing) :initial-element #\Newline)))))))

(defun fold-lines (lines)
  "Folded scalar: lines joined by spaces, a blank line a newline, a more-
indented line kept as it is."
  (with-output-to-string (out)
    (let ((previous nil))
      (dolist (line lines)
        (cond ((string= line "") (write-char #\Newline out) (setq previous :blank))
              ((and (plusp (length line)) (char= (char line 0) #\Space))
               (when (and previous (not (eq previous :blank))) (write-char #\Newline out))
               (write-string line out) (setq previous :indented))
              (t (when (and previous (not (eq previous :blank))) (write-char #\Space out))
                 (write-string line out) (setq previous :text)))))))

;;; Scalars

(defun parse-double-quoted (text line)
  (let ((close (quoted-end text 0 #\")))
    (unless close (yaml-error line "an unclosed string: ~a" text))
    (unless (zerop (length (string-trim " " (subseq text (1+ close)))))
      (yaml-error line "text after a string: ~a" text))
    (unescape-double (subseq text 1 close))))

(defun unescape-double (text)
  (with-output-to-string (out)
    (loop with i = 0
          while (< i (length text))
          do (let ((c (char text i)))
               (if (and (char= c #\\) (< (1+ i) (length text)))
                   (let ((e (char text (1+ i))))
                     (incf i 2)
                     (case e
                       (#\n (write-char #\Newline out)) (#\t (write-char #\Tab out)) (#\r (write-char #\Return out))
                       (#\0 (write-char (code-char 0) out)) (#\" (write-char #\" out)) (#\\ (write-char #\\ out))
                       (#\/ (write-char #\/ out)) (#\Space (write-char #\Space out))
                       (#\x (write-char (code-char (parse-integer text :start i :end (+ i 2) :radix 16)) out) (incf i 2))
                       (#\u (write-char (code-char (parse-integer text :start i :end (+ i 4) :radix 16)) out) (incf i 4))
                       (t (write-char #\\ out) (write-char e out))))
                   (progn (write-char c out) (incf i)))))))

(defun parse-single-quoted (text line)
  (let ((close (quoted-end text 0 #\')))
    (unless close (yaml-error line "an unclosed string: ~a" text))
    (unless (zerop (length (string-trim " " (subseq text (1+ close)))))
      (yaml-error line "text after a string: ~a" text))
    (ppcre:regex-replace-all "''" (subseq text 1 close) "'")))

(defun plain-scalar-value (text)
  "The typed value of a plain scalar: null, booleans, numbers, else the
string."
  (let ((text (string-trim " " text)))
    (cond ((or (string= text "") (string= text "~") (string-equal text "null")) :null)
          ((member text '("true" "yes" "on") :test #'string-equal) t)
          ((member text '("false" "no" "off") :test #'string-equal) :false)
          ((ppcre:scan "^[-+]?[0-9]+$" text) (parse-integer text))
          ((ppcre:scan "^0x[0-9a-fA-F]+$" text) (parse-integer text :start 2 :radix 16))
          ((ppcre:scan "^[-+]?([0-9]+\\.[0-9]*|\\.[0-9]+)([eE][-+]?[0-9]+)?$" text)
           (or (ignore-errors (let ((*read-default-float-format* 'double-float)) (read-from-string text))) text))
          (t text))))

;;; Flow collections: [a, b, {c: d}]

(defun parse-flow (text line)
  (multiple-value-bind (value at) (parse-flow-value text 0 line)
    (let ((rest (string-trim " " (subseq text at))))
      (unless (zerop (length rest)) (yaml-error line "text after a flow collection: ~a" rest)))
    value))

(defun skip-spaces (text at)
  (or (position-if-not (lambda (c) (member c '(#\Space #\Tab #\Newline))) text :start at) (length text)))

(defun parse-flow-value (text at line)
  "Two values: the flow value beginning at AT in TEXT, and the index after."
  (let ((at (skip-spaces text at)))
    (when (>= at (length text)) (yaml-error line "an unfinished flow collection"))
    (case (char text at)
      (#\[ (let ((items nil) (at (skip-spaces text (1+ at))))
             (loop
               (when (and (< at (length text)) (char= (char text at) #\])) (return (values (nreverse items) (1+ at))))
               (multiple-value-bind (item next) (parse-flow-value text at line)
                 (push item items)
                 (setq at (skip-spaces text next))
                 (cond ((>= at (length text)) (yaml-error line "an unclosed ["))
                       ((char= (char text at) #\,) (setq at (skip-spaces text (1+ at))))
                       ((char= (char text at) #\]))
                       (t (yaml-error line "expected , or ] in a flow sequence")))))))
      (#\{ (let ((map (make-ymap)) (at (skip-spaces text (1+ at))))
             (loop
               (when (and (< at (length text)) (char= (char text at) #\})) (return (values map (1+ at))))
               (multiple-value-bind (key next) (parse-flow-value text at line)
                 (setq at (skip-spaces text next))
                 (unless (and (< at (length text)) (char= (char text at) #\:)) (yaml-error line "expected : in a flow mapping"))
                 (multiple-value-bind (value after) (parse-flow-value text (1+ at) line)
                   (yset map (if (stringp key) key (princ-to-string key)) value)
                   (setq at (skip-spaces text after))
                   (cond ((>= at (length text)) (yaml-error line "an unclosed {"))
                         ((char= (char text at) #\,) (setq at (skip-spaces text (1+ at))))
                         ((char= (char text at) #\}))
                         (t (yaml-error line "expected , or } in a flow mapping"))))))))
      (#\" (let ((close (quoted-end text at #\")))
             (unless close (yaml-error line "an unclosed string"))
             (values (unescape-double (subseq text (1+ at) close)) (1+ close))))
      (#\' (let ((close (quoted-end text at #\')))
             (unless close (yaml-error line "an unclosed string"))
             (values (ppcre:regex-replace-all "''" (subseq text (1+ at) close) "'") (1+ close))))
      (#\* (let ((end (or (position-if (lambda (c) (member c '(#\, #\] #\} #\Space))) text :start at) (length text))))
             (let ((found (assoc (subseq text (1+ at) end) *yanchors* :test #'string=)))
               (unless found (yaml-error line "no anchor ~a" (subseq text (1+ at) end)))
               (values (cdr found) end))))
      (t (let ((end (or (position-if (lambda (c) (member c '(#\, #\] #\}))) text :start at) (length text))))
           ;; a key inside a flow mapping ends at a colon followed by a space
           (let ((colon (loop for i from at below end
                              when (and (char= (char text i) #\:) (or (= (1+ i) end) (char= (char text (1+ i)) #\Space)))
                                return i)))
             (when colon (setq end colon)))
           (values (plain-scalar-value (subseq text at end)) end))))))

;;; Conveniences for the CI reader

(defun ylist (value)
  "VALUE as a list: a list stays one, :null is none, anything else is
wrapped."
  (cond ((eq value :null) nil)
        ((null value) nil)
        ((listp value) value)
        (t (list value))))

(defun ystring (value)
  (cond ((stringp value) value)
        ((eq value :null) nil)
        ((eq value t) "true")
        ((eq value :false) "false")
        ((numberp value) (princ-to-string value))
        (t nil)))

(defun ytrue? (value) (and value (not (eq value :false)) (not (eq value :null))))
