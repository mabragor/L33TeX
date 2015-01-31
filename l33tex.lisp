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
(defun uninstall-prereader ()
  (setf tex-prereader #'empty-prereader))
(defun install-custom-prereader (fun-pointer)
  (setf tex-prereader fun-pointer))


(defclass prereader ()
  ((stream :initarg :stream)))

(defgeneric next-iter (obj)
  (:documentation "Main iteration method"))

(define-condition stop-iteration (error)
  ()
  (:documentation "Raized when iterator is depleted"))

(defmethod next-iter ((obj prereader))
  (let ((line (handler-case (read-line (slot-value obj 'stream))
		(end-of-file () (error 'stop-iteration)))))
    (if tex-prereader
	(funcall tex-prereader line)
	line)))

(defun mk-prereader (stream)
  (make-instance 'prereader :stream stream))

(defun iter->list (x)
  (let ((res nil))
    (handler-case 
	(iter (while t)
	      (push (next-iter x) res))
      (stop-iteration () nil))
    (nreverse res)))


(defclass over-iter ()
  ((sub-iterator :initarg :sub-iterator)))

(defclass pushable-iterator (over-iter)
  ((pushed-ones :initform nil)))

(defun mk-pushable-iter (sub-iter)
  (make-instance 'pushable-iterator :sub-iterator sub-iter))

(defmethod next-iter ((obj pushable-iterator))
  (with-slots (pushed-ones sub-iterator) obj
    (if pushed-ones
	(pop pushed-ones)
	(next-iter sub-iterator))))

(defun push-iter (obj iter)
  (with-slots (pushed-ones) iter
    (push obj pushed-ones)))

(defclass simple-chars (over-iter)
  ((cur-string :initform nil)
   (cur-pos :initform 0)
   (cur-length :initform 0)))

(defun mk-simple-chars (sub-iter)
  (make-instance 'simple-chars :sub-iterator sub-iter))
  
(defmethod next-iter ((iter simple-chars))
  (with-slots (cur-string cur-pos cur-length sub-iterator) iter
    (when (not cur-string)
      (setf cur-pos 0
	    cur-string (next-iter sub-iterator))
      (setf cur-length (length cur-string)))
    ;; each string is assumed to have at least 1 char - the #\return or #\newline
    (let ((res (char cur-string cur-pos)))
      (if (equal cur-length (incf cur-pos))
	  (setf cur-string nil))
      res)))
      
(defun foo ()
  (with-open-file (stream "~/drafts/kauffman-in-a-nutshell/noeuds-d-enfants.tex")
    (iter->list (mk-pushable-iter (mk-simple-chars (mk-prereader stream))))))
	



;; (define-condition valuable-condition (error)
;;   ((value :initarg :value :reader valuable-value)))

;; (defun counter (n)
;;   (iter (for i from 0 to n)
;; 	(restart-case (error 'valuable-condition :value i)
;; 	  (next () nil))))

;; (defun return-condition ()
;;   (handler-bind ((valueable-condition
;; 		  (lambda (c)
;; 		    (return-from return-condition c))))
;;     (counter 10)))