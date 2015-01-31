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
	

(defclass teh-reader (over-iter)
  ((entry-point :initform (error "You should set the entry point, otherwise I won't work!"))))

(defparameter category-numbers
  '((:escape . 0)
    (:begin-group . 1)
    (:end-group . 2)
    (:math-shift . 3)
    (:alignment-tab . 4)
    (:end-of-line . 5)
    (:parameter . 6)
    (:superscript . 7)
    (:subscript . 8)
    (:ignored . 9)
    (:space . 10)
    (:letter . 11)
    (:other . 12)
    (:active . 13)
    (:comment . 14)
    (:invalid . 15)))
     
(defclass tex-token ()
  ((text :initarg :text :initform "")
   (cat :initarg :cat :initform 12)))

(defun other-token (char)
  (make-instance 'tex-token :text char :cat :other))

(defparameter teh-readtable (make-hash-table :test #'equal))

(defun get-char-reader (char)
  (gethash char teh-readtable))
(defun set-char-reader (char fun)
  (setf (gethash char teh-readtable) fun))

(defun get-char-cat-reader (char)
  (get-cat-reader (get-char-cat char)))

(defparameter char-cats (make-hash-table :test #'equal))

(defun get-char-cat (char)
  (gethash char char-cats :other))
(defun set-char-cat (char cat)
  (setf (gethash char char-cats) cat))

(defparameter cat-readtable (make-hash-table :test #'equal))

(defun get-cat-reader (cat)
  (gethash cat cat-readtable))
(defun set-cat-reader (cat fun)
  (setf (gethash cat cat-readtable) fun))

(defun other-cat-reader (char iter)
  (declare (ignore iter))
  (other-token char))

(defun vanilla-reader (iter)
  (with-slots (sub-iterator) iter
    (let ((new-char (next-iter sub-iterator)))
      (let ((char-reader (get-char-reader new-char)))
	(if (not char-reader)
	    (let ((cat-reader (get-char-cat-reader new-char)))
	      (if (not cat-reader)
		  (error "Every character is supposed to have a category, so we shouldn't be here!")
		  (funcall cat-reader new-char iter)))
	    (funcall char-reader new-char iter))))))


(defmethod next-iter ((iter teh-reader))
  (funcall (slot-value iter 'entry-point) iter))

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