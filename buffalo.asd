;;; -*-Lisp-*-

(asdf:defsystem #:buffalo
  :name "buffalo"
  :description "An LALR(1) and canoncial LR(1) parser generator for Common Lisp"
  :author "Frank Huttner"
  :version "1.5"
  :licence "BSD"
  :depends-on (:SparseSet)
  :components ((:file "patricia")
	       (:file "bisect")
	       (:file "queue")
	       (:file "buffalo")
	       (:file "tarjan")
	       (:file "fib-heap")
	       (:file "knuth-dijkstra")
	       (:file "engine")
	       (:file "engine-lr1")
	       (:file "imex")
	       (:file "cgrammar")
	       (:file "buffalo-tests")
	       (:file "buffalo-tests-lr1")
  ))
