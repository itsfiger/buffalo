(defpackage #:buffalo-testsuite
  (:export #:run-test #:export-to-file)
  (:use #:cl #:buffalo))

(in-package #:buffalo-testsuite)


;;; 121: reduce.at:349      Underivable Rules

;; Buffalo should remove such rules but only reports them on demand

(DEFINE-GRAMMAR BISON121
  (:START-SYMBOL |exp|)
  (:PRECEDENCE NIL)
  (:TERMINALS (|useful|))
  (|exp| (|useful|) (|underivable|))
  (|underivable| (|indirection|))
  (|indirection| (|underivable|)))


(defun run-test ()
  (time
   (make-parser bison121 :muffle-warnings nil))
  (values))

(defun export-to-file ()
  (write-to-yacc-file "ts_bison121.y" bison121))
