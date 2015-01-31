;;;; l33tex.asd

(asdf:defsystem #:l33tex
  :serial t
  :description "TeX reader for lisp"
  :author "Alexandr Popolitov <popolit@gmail.com>"
  :license "GPL"
  :depends-on (#:iterate)
  :components ((:file "package")
               (:file "l33tex")))

