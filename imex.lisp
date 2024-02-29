;; Copyright (c) 2015 by Frank Huttner
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(in-package #:buffalo)

;;; Buffalo grammar to POSIX yacc

;; Concatenate _(n), if str already exists, with increasing n until a
;; string is created not previously in the hash-table
(defun print-create-unique-string (str strings-hash-table)
  (declare (type string str))
  (let ((i 0))
    (labels ((leave (str)
	       (setf (gethash str strings-hash-table) str)
	       str)
	     (not-exist-p (str)
	       (if (nth-value 1 (gethash str strings-hash-table))
		   nil
		   str))
	     (modify (str)
	       (incf i)
	       (concatenate 'string str (format nil "_~d" i))))
      (if (not-exist-p str)
	  (leave str)
	  (loop
	     for mod-str = (modify str)
	     until (not-exist-p mod-str)
	     finally (return (leave mod-str)))))))

;; Just convert everything not alphanumeric, "." or underscore to underscore
;; It has to start with a nonnumeric character
(defun print-prepare-printable-string (symbl func)
  (if symbl
      (let ((str (coerce
		  (loop for char across (funcall func (string symbl))
		     if (or
			 (digit-char-p char)
			 (alpha-char-p char)
			 (char= #\_ char)
			 (char= #\. char)) collect char
		     else
		     collect #\_) 'string)))
	(if (digit-char-p (aref str 0))
	    (concatenate 'string "_" str)
	    str))
      "(empty)"))

;; called with a Terminal for symb
;; in case string is just one non alphabetic symbol like "+" no token
;; should be created (return value nil)
(defun print-prepare-printable-terminal-string (symbl symbols-hash-table strings-hash-table)
    (let ((str (string symbl)))
      (flet ((entry (str)
	       (setf (gethash symbl symbols-hash-table) str)))
	(cond
	  ((and (= (length str) 1) (not (alpha-char-p (aref str 0))))
	   (if (or
		(char= #\\ (aref str 0))
		(char= #\" (aref str 0)))
	       (entry (concatenate 'string "\'\\" str "\'"))
	       (entry (concatenate 'string "\'" str "\'")))
	   nil)
	  (t
	   (entry
	    (print-create-unique-string
	     (print-prepare-printable-string symbl #'string-upcase) strings-hash-table)))))))

(defun print-symbol-to-string (symbol symbols-hash-table strings-hash-table)
  (let ((str (gethash symbol symbols-hash-table)))
    (or
     str
     (setf (gethash symbol symbols-hash-table)
	   (print-create-unique-string (print-prepare-printable-string symbol #'string-downcase) strings-hash-table)))))

;; No validation of the grammar is done. Use make-parser for this.
;; Every symbol not found in grammar-terminals is assumed to be as non-terminal
;; and printed lowercase.
;; Caution: Number of expected conflicts is a parameter of make-grammar and
;; define-grammar. You have to insert '%expect n' with n being the number of
;; shift/reduce-conflict to be expected
(defun print-grammar-in-yacc-format (grammar)
  (declare (type grammar grammar))
  (let ((symbols-hash-table (make-hash-table :test 'eq))
	;; Traces printable strings
	(strings-hash-table (make-hash-table :test 'equal))
	(text (make-string-output-stream)))
    (let ((terminals
	   (loop for terminal in (grammar-terminals grammar)
	      if (print-prepare-printable-terminal-string terminal symbols-hash-table strings-hash-table) collect it))
	  (start-symbol
	   (print-symbol-to-string (car (production-derives (car (grammar-productions grammar)))) symbols-hash-table strings-hash-table)))
      ;; Tokens
      (when terminals
	(format text "~%%token ~{~<~%%token ~1,80:; ~A~>~^ ~}" terminals))
      ;; Precedence definitions
      (loop for precedence in (reverse (grammar-precedence grammar))
	 for symbls = (loop for symbol in (cdr precedence)
			 collect (print-symbol-to-string symbol symbols-hash-table strings-hash-table))
	 do
	   (let* ((str (format nil "~a~(~a~)~a" "~%%~(~a~) ~{~<~%%" (car precedence) " ~1,80:; ~A~>~^ ~}")))
	     (format text "~?" str (list (car precedence) symbls))))
	   ;; (format text "~%%~(~a~) ~{~<~%%token ~1,80:; ~A~>~^ ~}" (car precedence) symbls))
      ;; Start symbol
      (format text "~%~%%start ~A~%" start-symbol)
      (format text "~%%%~%")
      ;; Productions
      (loop for production in (cdr (grammar-productions grammar))
	 for head = (production-symbol production)
	 and last-head = nil then head
	 for head-print = (print-symbol-to-string head symbols-hash-table strings-hash-table)
	 for production-print = (loop for symbol in (production-derives production)
				   collect (print-symbol-to-string symbol symbols-hash-table strings-hash-table))
	 do
	   (if (eq head last-head)
	       (format text "~%~vT |" (length head-print))
	       (format text "~:[~%~; ;~%~%~]~a :" last-head head-print))
	   (format text " ~{ ~a~}" production-print)
	   (when (production-prec production)
	     (format text " %prec ~a" (print-symbol-to-string (production-prec production) symbols-hash-table strings-hash-table))))
      (format text " ;~%~%")
      (values
       (get-output-stream-string text)
       (loop for v being the hash-values in symbols-hash-table using (hash-key k)
	  collect (cons v k))))))


;;; Import of POSIX yacc

(defun split-sequence (string char)
  (let ((end (length string)))
    (loop for start = 0 then (1+ stop)
       for stop = (min (or (position char string :start start) end)
		       end)
       if (< start stop) collect (subseq string start stop)
       until (= stop end))))

(defun strip-string (s)
  (remove-if (lambda (x) (or (char= x #\')
			     (char= x #\"))) s))


(defun lisplex (filename &optional name-alist)
  (let ((tokenstream
	 (with-open-file (in filename :direction :input)
	   (loop for line = (read-line in nil nil)
	      while line
	      collect (let* ((a (split-sequence line #\Space))
			     (token (intern (string-upcase (car a)) "KEYWORD"))
			     (val (cadr a)))
			(list
			 token
			 (if (or (eq token :ID)
				 (eq token :STRING_LITERAL))
			     (or (cdr (assoc val name-alist :test #'equal))
				 ;(warn "Symbol ~a not found. Interning." val)
				 (intern (strip-string val)))
			     val)))))))
    (lambda () (values-list (pop tokenstream)))))


(defun build-grammar-from-yacc (name file &optional name-alist)
  (let ((addtokens '())
	;; create a automaton structure that represents the current state of the
	;; finite state machine used for parsing including the symbol and
	;; value stacks
	(automaton (make-automaton))
	(precs '()))
    (flet ((precedence (type symbls)
	     (push (cons type symbls) precs)
	     (values))
	   (typ (type)
	     (declare (ignore type))
	     ;; The value of type, e.g. a string %left, could be used as easily
	     ;; But the keyword symbol is known to sit on the top of the symbols
	     ;; stack (because it a shift action on it that moved the machine here),
	     ;; it can be used conveniently
	     (first (automaton-symbol-stack automaton)))
	   (second-arg (&rest rest) (second rest))
	   (p13 (&rest rest) (list (first rest) (third rest)))
	   (c13 (&rest rest) (cons (first rest) (third rest))))
      (let* ((parser
	      (def-parser
		(:start-symbol program)
		(:terminals (:token :left :right :nonassoc
				    :start :%% :id :string_literal :|;| :|\|| :|:|))
		(program
		 (tokendefs start productions))
		(tokendefs
		 ()
		 (tokendef tokendefs #'append))
		(tokendef
		 (:token tokens #'second-arg)
		 (precedence symbols #'precedence))
		(tokens
		 ()
		 (:id tokens #'cons))
		(precedence
		 (:left #'typ)
		 (:right #'typ)
		 (:nonassoc #'typ))
		(start
		 (:start :id :%%))
		(productions
		 (production)
		 (production productions #'cons))
		(production
		 (:id :|:| alternatives :|;| #'p13))
		(alternatives
		 (symbols)
		 (symbols :|\|| alternatives #'c13))
		(symbols
		 ()
		 (symbol symbols #'cons))
		(symbol
		 (:string_literal (lambda (s) (pushnew s addtokens) s))
		 :id)))
	     (ast (parse-with-lexer (lisplex file name-alist) parser automaton))
	     (tokens (first ast))
	     (start (second ast))
	     (rules (third ast)))
	`(define-grammar ,name
	   (:start-symbol ,(cadr start))
	   (:precedence ,precs)
	   (:terminals ,(union addtokens tokens))
	   ,@(loop for rule in rules
		collect `(,(car rule) ,@(cadr rule))))))))


(let (name-list file name)
  (defun write-to-yacc-file (filename grammar)
    (setf file filename)
    (setf name (grammar-name grammar))
    (with-open-file (file filename :direction :output :if-exists :supersede)
      (multiple-value-bind (a b) (print-grammar-in-yacc-format grammar)
	(setf name-list b)
	(format file "~a" a))))
  (defun read-from-yacc-file (file)
    (build-grammar-from-yacc name file name-list)))
