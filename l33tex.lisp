;;;; l33tex.lisp

(in-package #:l33tex)

(defun joinl (joinee lst)
  (format nil (concatenate 'string "~{~a~^" joinee "~}") lst))
(defun join (joinee &rest lst)
  (joinl joinee lst))

(defun frnl (format-str &rest args)
  (apply #'format `(nil ,format-str ,@args)))
(defun fart (format-str &rest args)
  (apply #'format `(t ,format-str ,@args)))

(defun plain-tex-prereader (string)
  (join "" (string-right-trim '(#\space) string) #\return))
(defun empty-prereader (string)
  string)

(defparameter tex-prereader nil)

(defun install-plain-tex-prereader ()
  (setf tex-prereader #'plain-tex-prereader))
(defun disable-prereader ()
  (setf tex-prereader #'empty-prereader))
(defun install-custom-prereader (fun-pointer)
  (setf tex-prereader fun-pointer))