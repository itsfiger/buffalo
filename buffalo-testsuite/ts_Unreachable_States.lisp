(defpackage #:buffalo-testsuite
  (:export #:run-test #:export-to-file)
  (:use #:cl #:buffalo))

(in-package #:buffalo-testsuite)


;;; 164: conflicts.at:1327  Unreachable States After Conflict Resolution

;; /* S/R conflict resolved as reduce, so the state with item
;;  * (resolved_conflict: A . unreachable1) and all it transition successors are
;;  * unreachable, and the associated production is useless.  */

(DEFINE-GRAMMAR BISON164
  (:START-SYMBOL |start|)
  (:PRECEDENCE ((:LEFT A)))
  (:TERMINALS (A))
  (|start| (|resolved_conflict| A |reported_conflicts| A)
   (|reported_conflicts|))
  (|resolved_conflict| (A |unreachable1|) (A))
  ;; /* S/R conflict that need not be reported since it is unreachable because of
  ;;  * the previous conflict resolution.  Nonterminal unreachable1 and all its
  ;;  * productions are useless.  */
  (|unreachable1| (A |unreachable2|) NIL)
  (|unreachable2| NIL)
  (|reported_conflicts| (A) NIL))


(defun run-test ()
  (time
   (make-parser bison164 :muffle-warnings nil))
  (values))

(defun export-to-file ()
  (write-to-yacc-file "ts_bison164.y" bison164))
