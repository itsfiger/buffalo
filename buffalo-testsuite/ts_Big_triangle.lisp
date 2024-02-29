(defpackage #:buffalo-testsuite
  (:export #:run-test #:export-to-file)
  (:use #:cl #:buffalo))

(in-package #:buffalo-testsuite)


;;; 315: torture.at:132     Big triangle

(defun big-triangle (n)
  (let* ((tokens
	  (loop repeat n for i from 1
	     collect
	       (make-symbol
		(concatenate 'string "T" (format nil "~d" i))))))
    (make-grammar
     :name 'big-triangle
     :start-symbol 'input
     :terminals (cons 'end tokens)
     :productions
     (cons
      (make-production 'input '(exp) :action nil)
      (cons
       (make-production 'input '(input exp) :action nil)
       (loop repeat (1+ n) for i from 0
	  collect
	    (make-production 'exp
			     (append
			      (subseq tokens 0 i)
			      '(end))
			     :action nil)))))))

(defun run-test ()
  (time
   (make-parser (big-triangle 2000)))
  (values))

(defun export-to-file ()
  (write-to-yacc-file "ts_Big_triangle.y" (big-triangle 2000)))
