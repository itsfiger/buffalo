;; Copyright (c) 2015 by Frank Huttner

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;; The software contains code from cl-yacc Copyright (c) 2005-2009 by
;; Juliusz Chroboczek under the same licence.


(defpackage #:buffalo
  (:use #:common-lisp #:SparseSet #:queue)
  (:export #:make-production #:make-grammar #:make-parser #:parse-with-lexer
           #:define-grammar #:define-parser #:def-parser #:make-lr1-parser
	   #:make-automaton
	   #:automaton-state-stack #:automaton-symbol-stack #:automaton-val-stack
           #:buffalo-compile-warning #:conflict-warning #:conflict-summary-warning
           #:buffalo-runtime-error #:buffalo-parse-error #:buffalo-parse-error-terminal
           #:buffalo-parse-error-value #:buffalo-parse-error-expected-terminals
	   #:buffalo-grammar-error #:buffalo-muffable-warning
	   #:print-grammar-in-yacc-format #:build-grammar-from-yacc
	   #:write-to-yacc-file #:read-from-yacc-file))

(in-package #:buffalo)

(defun required-argument () (error "A required argument was not supplied"))


;;; Conditions

(define-condition buffalo-compile-warning (warning)
  ())

(define-condition conflict-warning (buffalo-compile-warning simple-warning)
  ((text :initarg :text :reader text))
  (:report (lambda (w stream)
             (format stream "~A~%"
		     (text w)))))

(define-condition conflict-summary-warning (buffalo-compile-warning)
  ((shift-reduce :initarg :shift-reduce
                 :reader conflict-summary-warning-shift-reduce)
   (reduce-reduce :initarg :reduce-reduce
                  :reader conflict-summary-warning-reduce-reduce))
  (:report (lambda (w stream)
             (format stream "~D Shift/Reduce, ~D Reduce/Reduce conflicts"
                     (conflict-summary-warning-shift-reduce w)
                     (conflict-summary-warning-reduce-reduce w)))))

(define-condition buffalo-muffable-warning (buffalo-compile-warning)
  ((text :initarg :text :reader text))
  (:report (lambda (w stream)
             (format stream "~A~%"
                     (text w)))))


(define-condition buffalo-runtime-error (error)
  ())

(define-condition buffalo-parse-error (buffalo-runtime-error)
  ((terminal :initarg :terminal :reader buffalo-parse-error-terminal)
   (value :initarg :value :reader buffalo-parse-error-value)
   (expected-terminals :initarg :expected-terminals
                       :reader buffalo-parse-error-expected-terminals))
  (:report (lambda (e stream)
             (format stream "Unexpected terminal ~S (value ~S). ~@:_~
                             Expected one of: ~S"
                     (buffalo-parse-error-terminal e)
                     (buffalo-parse-error-value e)
                     (buffalo-parse-error-expected-terminals e)))))

(define-condition buffalo-grammar-error (error)
  ((text :initarg :text :reader grammar-error-text))
  (:report (lambda (w stream)
             (format stream "~A~%"
                     (grammar-error-text w)))))

;;; Productions

(defstruct (production
             (:constructor make-production (symbol derives
						   &key action prec))
             (:print-function print-production))
  (symbol (required-argument) :type symbol)
  (derives (required-argument) :type list)
  (action #'list)
  (prec nil :type symbol))

(defun print-production (p s d)
  (declare (type production p) (stream s) (ignore d))
  (print-unreadable-object (p s :type t)
    (format s "~S -> ~{~S~^ ~}" (production-symbol p) (production-derives p))))

(defun parse-production (form)
  (let ((symbol (car form))
        (productions '()))
    (dolist (stuff (cdr form))
      (cond
        ((and (symbolp stuff) (not (null stuff)))
         (push (make-production symbol (list stuff)
                                :action #'identity)
               productions))
        ((listp stuff)
         (let* ((prec (when (eq :%prec (car stuff))
			(unless (symbolp (second stuff))
			  (error "Malformed precedence production ~S" stuff))
			(second stuff)))
		(stuff (if prec
			   (cddr stuff)
			   stuff))
		(l (car (last stuff))))
           (let ((rhs (if (symbolp l) stuff (butlast stuff)))
                 (action (if (symbolp l) #'list l)))
             (push (make-production symbol rhs :action action :prec prec)
                   productions))))
        (t (error "Unexpected production ~S" stuff))))
    productions))


 ;;; Grammars

(defparameter *grammar-keywords*
  '(:muffle-warnings :muffle-conflicts :print-derives-epsilon
    :print-states :print-includes :print-first-terminals
    :canonical-lr1 :force-check :print-nonterminal-derivations
    :print-state-sentential-forms))

(defstruct (grammar (:constructor %make-grammar))
  (name nil)
  (terminals '() :type list)
  (precedence '() :type list)
  (productions '() :type list)
  (no-terminals 0 :type fixnum)
  (no-nonterminals 0 :type fixnum)
  (used-terminals 0 :type fixnum)
  (used-nonterminals 0 :type fixnum))

(defun make-grammar(&key name (start-symbol (required-argument))
                    terminals precedence productions)
  (declare (symbol name start-symbol) (list terminals productions))
  (setq productions
        (cons (make-production 's-prime (list start-symbol '-eof-)
                               :action #'identity)
              productions))
  (%make-grammar :name name :terminals terminals :precedence precedence
                 :productions productions))

(defun parse-grammar (forms)
  (let ((options '()) (make-options '()) (productions '()))
    (dolist (form forms)
      (cond
        ((member (car form) *grammar-keywords*)
         (unless (null (cddr form))
           (error "Malformed option ~S" form))
         (push (car form) make-options)
         (push (cadr form) make-options))
        ((keywordp (car form))
         (unless (null (cddr form))
           (error "Malformed option ~S" form))
         (push (car form) options)
         (push (cadr form) options))
        ((symbolp (car form))
         (setq productions (nconc (parse-production form) productions)))
        (t
         (error "Unexpected grammar production ~S" form))))
    (values (nreverse options) (nreverse make-options)
            (nreverse productions))))


;;; User Interface

(defmacro define-grammar (name &body body)
  `(defparameter ,name
     (def-grammar ,@body)))

(defmacro def-grammar (&body body)
  "DEFINE-GRAMMAR NAME OPTION... PRODUCTION...
PRODUCTION ::= (SYMBOL RHS...)
RHS ::= SYMBOL | (SYMBOL... [ACTION])
Defines the special variable NAME to be a grammar.  Options are as in
MAKE-GRAMMAR."
    `(multiple-value-bind (options make-options productions) (parse-grammar ',body)
       (unless (null make-options)
	 (warn "DEFINE-GRAMMAR ignores options ~S" make-options))
       (apply #'make-grammar
	      :name 'grammar
	      :productions productions
	      options)))


(defun make-parser (grammar &rest options)
  (multiple-value-bind (parser statistic)
      (apply #'number-grammar grammar options)
    (values
     (cons
      (coerce (mapcar #'production-action (grammar-productions grammar)) 'vector)
      parser)
     statistic)))

(defmacro define-parser (name &body body)
  `(defparameter ,name
     (def-parser ,@body)))

(defmacro def-parser (&body body)
  "DEFINE-GRAMMAR NAME OPTION... PRODUCTION...
PRODUCTION ::= (SYMBOL RHS...)
RHS ::= SYMBOL | (SYMBOL... [ACTION])
Defines the special variable NAME to be a parser.  Options are as in
MAKE-GRAMMAR and MAKE-PARSER."
  (multiple-value-bind (options make-options productions) (parse-grammar body)
    (let* ((grammar (apply #'make-grammar
			   :name 'name
			   :productions productions
			   options))
	   (parser-1 (apply #'number-grammar grammar make-options))
	   (parser+1
	    (loop for production in (grammar-productions grammar)
	       for action = (production-action production)
	       collect
		 (cond
		   ((eq action #'list) '(function list) )
		   ((eq action #'identity) '(function identity))
		   (t action)))))
      `(cons
	(vector ,@parser+1)
	',parser-1))))

;;; LR1 interface

(defun make-lr1-parser (grammar &rest options)
  (apply #'make-parser grammar :canonical-lr1 t options))


;;; Parser automaton

(defstruct automaton
  (state-stack '(0) :type list)
  (symbol-stack '() :type list)
  (val-stack '() :type list)
  (symbol nil :type symbol)
  (value nil :type t))


(defun parse-with-lexer (lexer parser &optional (a (make-automaton)))
"Parse the stream of symbols provided by LEXER using PARSER.
LEXER is a function of no arguments returning a symbol and a semantic value,
and should return (VALUES NIL NIL) when the end of input is reached.
Handle BUFFALO-PARSE-ERROR to provide custom error reporting."
  (declare (type (function () (values symbol t)) lexer))
  (declare (type list parser))
  (destructuring-bind
	(function-array rule-array head-array action-array goto-array accept-state)
      parser
    (flet ((action (i a)
             (declare (type symbol a))
             (cdr (assoc a (aref action-array i))))
           (goto (i a)
             (declare (type symbol a))
             (cdr (assoc a (aref goto-array i))))
	   (next-symbol ()
	     (multiple-value-bind (s v) (funcall lexer)
	       (setf (automaton-symbol a) (or s '-eof-)
		     (automaton-value a) v)))
	   (pop-stack ()
	     (pop (automaton-state-stack a))
	     (pop (automaton-symbol-stack a))
	     (pop (automaton-val-stack a)))
	   (push-stack (val state symbol)
	     (push val (automaton-val-stack a))
	     (push state (automaton-state-stack a))
	     (push symbol (automaton-symbol-stack a))))
      (next-symbol)
      (loop
	 (let* ((state (car (automaton-state-stack a)))
		(action (action state (automaton-symbol a))))
	   (cond
	     ((= state accept-state) ; accept-action
	      ;; we have come here through an shift on -eof- (= nil)
	      ;; this state and the state before have to be popped before
	      (pop-stack)
	      (return (pop-stack)))
	     ((not action)
	      (error (make-condition
		      'buffalo-parse-error
		      :terminal (if (eq (automaton-symbol a) '-eof-) nil (automaton-symbol a))
		      :value (automaton-value a)
		      :expected-terminals
		      (mapcan
		       #'(lambda (e)
			   (and (cdr e)
				(list
				 (if (eq (car e) '-eof-)
				     nil
				     (car e)))))
		       (aref action-array state)))))
	     ((> action 0) ; shift-action
	      (push-stack (automaton-value a) action (automaton-symbol a))
	      (next-symbol))
	     ((< action 0) ; reduce-action
	      (let* ((rule (- action))
		     (n (aref rule-array rule)) ; Length of production rhs
		     (val* (apply (aref function-array rule)
				  (nreverse (subseq (automaton-val-stack a) 0 n))))
		     (symbol* (aref head-array rule))
		     (s* (progn
			   (loop repeat (aref rule-array rule) do (pop-stack))
			   (car (automaton-state-stack a)))))
		(push-stack val* (goto s* symbol*) symbol*)))))))))
