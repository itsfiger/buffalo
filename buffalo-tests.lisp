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
;;
;; The software contains code from cl-yacc Copyright (c) 2005-2009 by Juliusz Chroboczek
;; under the same licence.


(defpackage #:buffalo-tests
  (:export #:tests)
  (:use #:cl #:buffalo))

(in-package #:buffalo-tests)

(defmacro expect (form expected)
  (let ((value (gensym "VALUE")) (e-value (gensym "EXPECTED")))
    `(let ((,value ,form) (,e-value ,expected))
      (unless (equal ,value ,e-value)
        (error "Test failed: ~S yielded ~S, expected ~S"
               ',form ,value ,e-value)))))

(defmacro expect-condition (class &body body)
  (let ((name (gensym "EXPECT-CONDITION")))
  `(block ,name
    (handler-case (progn ,@body)
      (,class () (return-from ,name nil)))
    (error "~S didn't signal a ~S" ',body ',class))))

;;;; Tests of the low-level interface, which is not meant for human beings.
;;;; See the high-level interface below if you are looking for examples.

;;; A trivial lexer

(defun list-lexer (list)
  #'(lambda ()
      (let ((x (pop list)))
        (values x x))))

;;; A few grammars from the Dragon book

(defun make-grammar-4-31 ()
  (make-grammar :terminals '(+ * id lb rb)
                :start-symbol 'e
                :productions
                (list (make-production 'e '(e + tt))
                      (make-production 'e '(tt) :action #'identity)
                      (make-production 'tt '(tt * f))
                      (make-production 'tt '(f) :action #'identity)
                      (make-production 'f '(lb e rb))
                      (make-production 'f '(id) :action #'identity))))

;; This grammar is LALR(1) but not SLR

(defun make-grammar-4-20 ()
  (make-grammar :terminals '(id * =)
                :start-symbol 's
                :productions
                (list (make-production 's '(l = r))
                      (make-production 's '(r))
                      (make-production 'l '(* r))
                      (make-production 'l '(id))
                      (make-production 'r '(l)))))

(defun make-grammar-4-21 ()
  (make-grammar :terminals '(c d)
                :start-symbol 's
                :productions
                (list (make-production 's '(cc cc))
                      (make-production 'cc '(c cc))
                      (make-production 'cc '(d)))))

;;; Epsilon-reductions on the left and right side

(defun make-grammar-epsilon-left ()
  (make-grammar :terminals '(id)
                :start-symbol 's
                :productions
                (list (make-production 's '())
                      (make-production 's '(s id)))))

(defun make-grammar-epsilon-right ()
  (make-grammar :terminals '(id)
                :start-symbol 's
                :productions
                (list (make-production 's '())
                      (make-production 's '(id s)))))

(defun tests-low ()
  (let ((parser-4-31 (make-parser (make-grammar-4-31)))
        (parser-4-20 (make-parser (make-grammar-4-20)))
        (parser-4-21 (make-parser (make-grammar-4-21)))
        (parser-epsilon-left (make-parser (make-grammar-epsilon-left)))
        (parser-epsilon-right (make-parser (make-grammar-epsilon-right))))
    (flet ((parse (parser list) (parse-with-lexer (list-lexer list) parser)))
      (expect (parse parser-4-31 '(lb id + id * id rb))
              '(lb (id + (id * id)) rb))
      (expect (parse parser-4-31 '(lb id * id + id rb))
              '(lb ((id * id) + id) rb))
      (expect (parse parser-4-20 '(* id = * * id))
              '((* ((id))) = ((* ((* ((id))))))))
      (expect (parse parser-4-21 '(c d c d))
              '((c (d)) (c (d))))
      (expect (parse parser-epsilon-left '()) '())
      (expect (parse parser-epsilon-left '(id)) '(nil id))
      (expect (parse parser-epsilon-left '(id id)) '((nil id) id))
      (expect (parse parser-epsilon-right '()) '())
      (expect (parse parser-epsilon-right '(id)) '(id nil))
      (expect (parse parser-epsilon-right '(id id)) '(id (id nil)))
      t)))


;;;; Tests of the high-level interface

(defun digitp (c) (member c '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0)))

(defun simple-lexer (stream)
  (let ((c (read-char stream nil nil)))
    (cond
      ((null c) (values nil nil))
      ((member c '(#\Space #\Tab #\Newline)) (simple-lexer stream))
      ((member c '(#\+ #\- #\* #\^ #\= #\/ #\( #\)))
       (let ((v (intern (string c))))
         (values v v)))
      ((digitp c)
       (let ((buffer (make-array 10 :element-type 'character
                                 :fill-pointer 0)))
         (do ((c c (read-char stream nil nil)))
             ((or (null c) (not (digitp c)))
              (unless (null c) (unread-char c stream))
              (values 'int (read-from-string buffer)))
           (vector-push-extend c buffer))))
      ((alpha-char-p c)
       (let ((buffer (make-array 10 :element-type 'character
                                 :fill-pointer 0)))
         (do ((c c (read-char stream nil nil)))
             ((or (null c) (not (alphanumericp c)))
              (unless (null c) (unread-char c stream))
              (values 'id (copy-seq buffer)))
           (vector-push-extend c buffer))))
      (t (error "Lexing error")))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun k-2-3 (a b c) (declare (ignore a c)) b))

(define-parser *left-expression-parser*
  (:start-symbol expression)
  (:terminals (int id + - * / |(| |)|))
  (expression
   (expression + term)
   (expression - term)
   term)
  (term
   (term * factor)
   (term / factor)
   factor)
  (factor
   id
   int
   (|(| expression |)| #'k-2-3)))

(define-parser *ambiguous-expression-parser*
  (:start-symbol expression)
  (:terminals (int id + - * / |(| |)|))
  (:muffle-conflicts (16 0))
  (expression
   (expression + expression)
   (expression - expression)
   (expression * expression)
   (expression / expression)
   id
   int
   (|(| expression |)| #'k-2-3)))

(define-parser *precedence-left-expression-parser*
  (:start-symbol expression)
  (:terminals (int id + - * / |(| |)|))
  (:precedence ((:left * /) (:left + -)))
  (expression
   (expression + expression)
   (expression - expression)
   (expression * expression)
   (expression / expression)
   id
   int
   (|(| expression |)| #'k-2-3)))

(define-parser *precedence-right-expression-parser*
  (:start-symbol expression)
  (:terminals (int id + - * / |(| |)|))
  (:precedence ((:right * /) (:right + -)))

  (expression
   (expression + expression)
   (expression - expression)
   (expression * expression)
   (expression / expression)
   id
   int
   (|(| expression |)| #'k-2-3)))

(define-parser *precedence-nonassoc-expression-parser*
  (:start-symbol expression)
  (:terminals (int id + - * / |(| |)|))
  (:precedence ((:nonassoc * /) (:nonassoc + -)))

  (expression
   (expression + expression)
   (expression - expression)
   (expression * expression)
   (expression / expression)
   id
   int
   (|(| expression |)| #'k-2-3)))

(define-parser *precedence-context-expression-parser*
  (:start-symbol expression)
  (:terminals (int id uminus ^ + - * / |(| |)|))
  (:precedence ((:right ^) (:left uminus)
		(:left * /) (:left + -)))
  (expression
   (expression + expression)
   (expression - expression)
   (expression * expression)
   (expression / expression)
   (:%prec uminus - expression)
   (expression ^ expression)
   id
   int
   (|(| expression |)| #'k-2-3)))

(defun tests-hi ()
  (flet ((parse (parser e) 
           (with-input-from-string (s e)
             (parse-with-lexer #'(lambda () (simple-lexer s)) parser))))
    (let ((*package* (find-package '#:buffalo-tests)))
      (let ((e "(x+3)+y*z") (v '(("x" + 3) + ("y" * "z"))))
        (expect (parse *left-expression-parser* e) v)
        (expect (parse *precedence-left-expression-parser* e) v)
        (expect (parse *precedence-right-expression-parser* e) v)
        (expect (parse *precedence-nonassoc-expression-parser* e) v))
      (let ((e "x+5/3*(12+y)/3+z"))
        (let ((v '(("x" + (((5 / 3) * (12 + "y")) / 3)) + "z")))
          (expect (parse *left-expression-parser* e) v)
          (expect (parse *precedence-left-expression-parser* e) v))
        (let ((v '("x" + ((5 / (3 * ((12 + "y") / 3))) + "z"))))
          (expect (parse *precedence-right-expression-parser* e) v))
        (let ((v '("x" + (5 / (3 * ((12 + "y") / (3 + "z")))))))
          (expect (parse *ambiguous-expression-parser* e) v))
        (expect-condition buffalo-parse-error
          (parse *precedence-nonassoc-expression-parser* e)))
      (expect (parse *precedence-context-expression-parser* "-3^5*2")
	      '((- (3 ^ 5)) * 2))
      (expect (parse *precedence-context-expression-parser* "-3*5*2")
	      '(((- 3) * 5) * 2))
      (dolist (e '("5/3*(" "5/3)"))
        (expect-condition buffalo-parse-error
          (parse *left-expression-parser* e))
        (expect-condition buffalo-parse-error
          (parse *ambiguous-expression-parser* e))
        (expect-condition buffalo-parse-error
          (parse *precedence-left-expression-parser* e))
        (expect-condition buffalo-parse-error
          (parse *precedence-right-expression-parser* e)))))
  t)

;;; Grammar Check Tests
(define-grammar *check-duplicate-terminals*
  (:start-symbol expression)
  (:terminals (int id + - id * / |(| |)|))
  (:precedence ((:left * /) (:left + -)))
  (expression
   (expression + expression)
   (expression - expression)
   (expression * expression)
   (expression / expression)
   id
   int
   (|(| expression |)|)))

(define-grammar *check-terminal-start-symbol*
  (:start-symbol int)
  (:terminals (int id + - * / |(| |)|))
  (:precedence ((:left * /) (:left + -)))
  (expression
   (expression + expression)
   (expression - expression)
   (expression * expression)
   (expression / expression)
   id
   int
   (|(| expression |)|)))

(define-grammar *check-terminal-starts-production*
  (:start-symbol expression)
  (:terminals (int id + - * / |(| |)|))
  (:precedence ((:left * /) (:left + -)))
  (expression
   (expression - expression)
   (expression * expression)
   (expression / expression)
   id
   int
   (|(| expression |)|))
  (int
   (expression + expression)))

(define-grammar *check-identical-rules*
  (:start-symbol expression)
  (:terminals (int id + - * / |(| |)|))
  (:precedence ((:left * /) (:left + -)))
  (expression
   (expression + expression)
   (expression - expression)
   (expression * expression)
   (expression + expression)
   id
   int
   (|(| expression |)|)))

(define-grammar *check-precedence-symbols*
  (:start-symbol expression)
  (:terminals (int id + - * / |(| |)|))
  (:precedence ((:left * / expression) (:left + lb)))
  (expression
   (expression + expression)
   (expression - expression)
   (expression * expression)
   (expression + expression)
   id
   int
   (|(| expression |)|)))

(define-grammar *check-empty-language*
	   (:start-symbol s)
	   (:terminals (c d))
	   (s
	    (cc cc))
	   (cc
	    (dd cc))
	   (dd
	    (cc)))

(defun tests-checks ()
  (expect-condition buffalo-grammar-error
    (make-parser *check-duplicate-terminals*))
  (expect-condition buffalo-grammar-error
    (make-parser *check-terminal-start-symbol*))
  (expect-condition buffalo-grammar-error
    (make-parser *check-terminal-starts-production*))
  (expect-condition buffalo-grammar-error
    (make-parser *check-identical-rules*))
  (expect-condition buffalo-grammar-error
    (make-parser *check-precedence-symbols*))
  (expect-condition buffalo-grammar-error
    (make-parser *check-empty-language*))
  t)

  
(defun tests ()
  (tests-low)
  (tests-hi)
  (tests-checks)
  t)


;; This grammar produces 2 Reduce/Reduce conflicts in cl-yacc
;; But it is ok both in buffalo and GNU Bison
(define-parser *cl-yacc-test-parser*
  (:start-symbol program)
  (:terminals (:t1 :t2))
  (program
   e1
   (e1 lst))
  (lst
   (e2 e1)
   (e2 e1 lst))
  (e2
   ee)
  (ee
   nil
   :t2)
  (e1
   :t1))
