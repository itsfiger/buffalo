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


(in-package #:buffalo-tests)

(export 'tests-lr1)

(defun make-grammar-4-44 ()
  "LR1 but not LALR1"
  (make-grammar :terminals '(a b c d e)
                :start-symbol 'l
                :productions
                (list (make-production 'l '(s))
		      (make-production 'l '(l s))
		      (make-production 's '(a AA d))
		      (make-production 's '(b BB d))
		      (make-production 's '(a BB e))
		      (make-production 's '(b AA e))
                      (make-production 'AA '(c))
                      (make-production 'BB '(c)))))


(defun tests-low-lr1 ()
  (let ((parser-4-44 (make-lr1-parser (make-grammar-4-44)))
	(parser-4-31 (make-lr1-parser (make-grammar-4-31)))
        (parser-4-20 (make-lr1-parser (make-grammar-4-20)))
        (parser-4-21 (make-lr1-parser (make-grammar-4-21)))
        (parser-epsilon-left (make-lr1-parser (make-grammar-epsilon-left)))
        (parser-epsilon-right (make-lr1-parser (make-grammar-epsilon-right))))
    (flet ((parse (parser list) (parse-with-lexer (list-lexer list) parser)))
      (expect (parse parser-4-44 '(a c d a c e b c d b c e))
              '(((((A (C) D)) (A (C) E)) (B (C) D)) (B (C) E)))
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

(define-parser *left-expression-parser-lr1*
  (:start-symbol expression)
  (:terminals (int id + - * / |(| |)|))
  (:canonical-lr1 t)
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

(define-parser *ambiguous-expression-parser-lr1*
  (:start-symbol expression)
  (:terminals (int id + - * / |(| |)|))
  (:muffle-conflicts (32 0))
  (:canonical-lr1 t)
  (expression
   (expression + expression)
   (expression - expression)
   (expression * expression)
   (expression / expression)
   id
   int
   (|(| expression |)| #'k-2-3)))

(define-parser *precedence-left-expression-parser-lr1*
  (:start-symbol expression)
  (:terminals (int id + - * / |(| |)|))
  (:precedence ((:left * /) (:left + -)))
  (:canonical-lr1 t)
  (expression
   (expression + expression)
   (expression - expression)
   (expression * expression)
   (expression / expression)
   id
   int
   (|(| expression |)| #'k-2-3)))

(define-parser *precedence-right-expression-parser-lr1*
  (:start-symbol expression)
  (:terminals (int id + - * / |(| |)|))
  (:precedence ((:right * /) (:right + -)))
  (:canonical-lr1 t)
  (expression
   (expression + expression)
   (expression - expression)
   (expression * expression)
   (expression / expression)
   id
   int
   (|(| expression |)| #'k-2-3)))

(define-parser *precedence-nonassoc-expression-parser-lr1*
  (:start-symbol expression)
  (:terminals (int id + - * / |(| |)|))
  (:precedence ((:nonassoc * /) (:nonassoc + -)))
  (:canonical-lr1 t)
  (expression
   (expression + expression)
   (expression - expression)
   (expression * expression)
   (expression / expression)
   id
   int
   (|(| expression |)| #'k-2-3)))

(define-parser *precedence-context-expression-parser-lr1*
  (:start-symbol expression)
  (:terminals (int id uminus ^ + - * / |(| |)|))
  (:precedence ((:right ^) (:left uminus)
		(:left * /) (:left + -)))
  (:canonical-lr1 t)
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

(defun tests-hi-lr1 ()
  (flet ((parse (parser e) 
           (with-input-from-string (s e)
             (parse-with-lexer #'(lambda () (simple-lexer s)) parser))))
    (let ((*package* (find-package '#:buffalo-tests)))
      (let ((e "(x+3)+y*z") (v '(("x" + 3) + ("y" * "z"))))
        (expect (parse *left-expression-parser-lr1* e) v)
        (expect (parse *precedence-left-expression-parser-lr1* e) v)
        (expect (parse *precedence-right-expression-parser-lr1* e) v)
        (expect (parse *precedence-nonassoc-expression-parser-lr1* e) v))
      (let ((e "x+5/3*(12+y)/3+z"))
        (let ((v '(("x" + (((5 / 3) * (12 + "y")) / 3)) + "z")))
          (expect (parse *left-expression-parser-lr1* e) v)
          (expect (parse *precedence-left-expression-parser-lr1* e) v))
        (let ((v '("x" + ((5 / (3 * ((12 + "y") / 3))) + "z"))))
          (expect (parse *precedence-right-expression-parser-lr1* e) v))
        (let ((v '("x" + (5 / (3 * ((12 + "y") / (3 + "z")))))))
          (expect (parse *ambiguous-expression-parser-lr1* e) v))
        (expect-condition buffalo-parse-error
          (parse *precedence-nonassoc-expression-parser-lr1* e)))
      (expect (parse *precedence-context-expression-parser-lr1* "-3^5*2")
	      '((- (3 ^ 5)) * 2))
      (expect (parse *precedence-context-expression-parser-lr1* "-3*5*2")
	      '(((- 3) * 5) * 2))
      (dolist (e '("5/3*(" "5/3)"))
        (expect-condition buffalo-parse-error
          (parse *left-expression-parser-lr1* e))
        (expect-condition buffalo-parse-error
          (parse *ambiguous-expression-parser-lr1* e))
        (expect-condition buffalo-parse-error
          (parse *precedence-left-expression-parser-lr1* e))
        (expect-condition buffalo-parse-error
          (parse *precedence-right-expression-parser-lr1* e)))))
  t)


(defun tests-lr1 ()
  (tests-low-lr1)
  (tests-hi-lr1)
  t)

