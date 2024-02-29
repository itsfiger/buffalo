(in-package #:buffalo)

;;; A very simple example, using a parser from buffalo-tests
;;; It demonstrates how the semantic actions can be modified programatically
;;; even though the parser is defined by a macro at compile time

(defun parser-factory (plus times)
  (def-parser
    (:terminals (+ * id lb rb))
    (:start-symbol e)
    (e
     (e + tt (values plus))
     tt)
    (tt
     (tt * f (values times))
     f)
    (f
     (lb e rb (lambda (lb e rb)
		(declare (ignore lb rb)) e))
     id)))


(defun test-parser-factory ()
  (let ((list '(lb id + id * id rb)))
    (flet ((list-lexer (list)
	     (lambda ()
	       (let ((x (pop list)))
		 (values x x))))
	   (plus (a p b)
	     (declare (ignore p))
	     `(+ ,a ,b))
	   (times (a p b)
	     (declare (ignore p))
	     `(* ,a ,b)))
      (let ((a (parser-factory #'list #'list))
	    (b (parser-factory #'plus #'times)))
	;; print as infix
	(print
	 (parse-with-lexer (list-lexer list) a))
	;; print as prefix
	(print
	 (parse-with-lexer (list-lexer list) b))))))
