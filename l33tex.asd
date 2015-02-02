;;;; l33tex.asd

(asdf:defsystem #:l33tex
  :serial t
  :description "TeX reader for lisp"
  :author "Alexandr Popolitov <popolit@gmail.com>"
  :license "GPL"
  :depends-on (#:iterate #:cl-yaclyaml)
  :components ((:file "package")
	       (:file "utils")
               (:file "l33tex")
	       (:file "dvi-read")
	       (:file "dvi-write")))

