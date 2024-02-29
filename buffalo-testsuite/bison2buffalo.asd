;;; -*-Lisp-*-

(asdf:defsystem #:bison2buffalo
  :name "bison2buffalo"
  :description "Grammar translator from GNU Bison XML output to Common Lisp's buffalo"
  :author "Frank Huttner <frank.huttner@gmx.de>"
  :version "1.0"
  :licence "GPL"
  :depends-on (:s-xml)
  :components ((:file "bison2buffalo")))
