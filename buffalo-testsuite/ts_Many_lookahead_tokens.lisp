(defpackage #:buffalo-testsuite
  (:export #:run-test #:export-to-file)
  (:use #:cl #:buffalo))

(in-package #:buffalo-testsuite)


;;; 317: torture.at:348     Many lookahead tokens

(defun many-lookahead-tokens (n)
  (let* ((tokens
	  (loop repeat n for i from 1
	     collect
	       (make-symbol
		(concatenate 'string "T" (format nil "~d" i)))))
	 (nonterminals
	  (loop repeat n for i from 1
	     collect
	       (make-symbol
		(concatenate 'string "N" (format nil "~d" i))))))
    (make-grammar
     :name 'many-lookahead-tokens
     :start-symbol 'input
     :terminals (cons 'token tokens)
     :productions
     (cons
      (make-production 'input '(exp) :action nil)
      (cons
       (make-production 'input '(input exp) :action nil)
       (loop for i from 1
	  for token in tokens
	  for nonterminal in nonterminals
	  collect
	    (make-production 'exp
			     (list nonterminal token))
	  collect
	    (make-production nonterminal '(token))))))))


(defun run-test ()
  (time
   (make-parser (many-lookahead-tokens 20000)))
  (values))

(defun export-to-file ()
  (write-to-yacc-file "ts_many-lookahead-tokens.y" (many-lookahead-tokens 20000)))
