;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(defpackage #:bison2buffalo
  (:use #:common-lisp #:s-xml)
  (:export #:read-grammar))

(in-package #:bison2buffalo)


;;; ------ Terminals --------

(defun bison-terminals-xml (in)
  (let* ((ps (make-instance 'xml-parser-state
			    :seed (cons '() '()) 
			    :new-element-hook #'bison-terminals-xml-new-element-hook))
	 (pr (start-parse-xml in ps))
	 (ts (car pr))
	 (prec (cdr pr))
	 (max (loop for (a b c) in prec maximize a))
	 (vec (make-array max :initial-element '())))
    (loop for (d b c) in prec
	 for a = (decf d)
       do (if (aref vec a)
	      (setf (aref vec a) (nconc (aref vec a) (list c)))
	      (setf (aref vec a) (list b c))))
    `((:terminals ,ts)
      (:precedence ,(coerce (nreverse vec) 'list)))))

(defun bison-terminals-xml-new-element-hook (name attributes seed)
  (case name
    (:|terminal|
      (let ((name (strip-string (cdr (assoc :|name| attributes))))
	    (assoc (cdr (assoc :|assoc| attributes)))
	    (prec (cdr (assoc :|prec| attributes))))
	(unless (member name '("error" "$end") :test #'string=)
	  (push (intern name) (car seed)))
	(let ((pre (if prec (parse-integer prec)))
	      (ass (if assoc
		       (intern (string-upcase assoc) 'keyword))))
		       ;(cond
			; ((string= assoc "precedence") :nonassoc)
			 ;(t )))))
	  (if pre
	      (push (list pre ass (intern name)) (cdr seed))))))
    (:|transition|
      (let ((assoc (cdr (assoc :|symbol| attributes))))
	(if (string= assoc "error")
	    (pushnew '|error| (car seed))))))
  seed)

(defun strip-string (s)
  (remove-if (lambda (x) (or (char= x #\')
			     (char= x #\"))) s))


;;; ------ Rules --------

(defun bison-rules-xml (in)
  (let ((ps (make-instance 'xml-parser-state
				  :seed (list '() '() '() '() '()) 
				  :new-element-hook #'bison-rules-xml-new-element-hook
                                  :finish-element-hook #'bison-rules-xml-finish-element-hook
				  :text-hook #'bison-rules-xml-text-hook)))
    (let ((seed (start-parse-xml in ps)))
      (cons `(:start-symbol ,(fifth seed)) (nreverse (fourth seed))))))

(defun bison-rules-xml-new-element-hook (name attributes seed)
  (case name
    (:|rule|
      (let ((prec (cdr (assoc :|percent_prec| attributes))))
	(setf (second seed) (if prec (list (intern prec) :%prec))
	      (first seed) name)))
    (:|rhs|
      (setf (first seed) name))
    (:|lhs|
      (setf (first seed) name))
    (:|symbol|
      (setf (first seed) name)))
  seed)

(defun bison-rules-xml-finish-element-hook (name attributes parent-seed seed)
  (declare (ignore attributes))
  (case name
    (:|rule|
      (if (string= (symbol-name (third seed)) "$accept")
	  (setf (fifth seed) (second (second seed)))
	  (push (list (third seed) (nreverse (second seed))) (fourth seed)))))
  (setf (first seed) (first parent-seed))
  seed)

(defun bison-rules-xml-text-hook (string seed)
  (case (first seed)
    (:|lhs|
      (setf (third seed) (intern (strip-string string))))
    (:|symbol|
      (push (intern (strip-string string)) (second seed))))
  seed)


;;; ------ The main function --------

(defun read-grammar (name file &optional (stream t))
  (let (terminals productions)
    (setf terminals (with-open-file (in file) (bison-terminals-xml in))
	  productions (with-open-file (in file) (bison-rules-xml in)))
    (pprint
     `(define-grammar ,name
       ,@terminals
       ,@productions) stream)))



