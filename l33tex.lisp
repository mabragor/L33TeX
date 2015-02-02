;;;; l33tex.lisp

(in-package #:l33tex)

(defparameter texenv::endlinechar 13)

(defun plain-tex-prereader (string)
  (join "" (string-right-trim '(#\space) string)
	(if (not (and (<= 0 texenv::endlinechar)
		      (<= texenv::endlinechar 255)))
	    ""
	    (code-char texenv::endlinechar))))
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
   (cur-length :initform 0)
   (state :initform 2)))

(defun mk-simple-chars (sub-iter)
  (make-instance 'simple-chars :sub-iterator sub-iter))
  
(defmethod next-iter ((iter simple-chars))
  (with-slots (cur-string cur-pos cur-length sub-iterator state) iter
    (cond ((equal 2 state)
	   ;; Plug in a new string
	   (setf cur-pos 0
		 cur-string (next-iter sub-iterator))
	   (setf cur-length (length cur-string)
		 state 0)
	   (next-iter iter))
	  ((equal 1 state)
	   ;; Emit an end-of-line token
	   (setf state 2)
	   :end-of-line)
	  ((equal 0 state)
	   ;; Act as usual - emit next char
	   (if (equal 0 cur-length)
	       (progn (setf state 1)
		      (next-iter iter))
	       (let ((res (char cur-string cur-pos)))
		 (if (equal cur-length (incf cur-pos))
		     (setf state 1))
		 res))))))

(defclass char-mangler (over-iter)
  ())

(defun soft-next-iter (iter)
  (handler-case (next-iter iter)
    (stop-iteration () :stop-iteration)))

(defun mangle-single-char (char)
  (if (< (char-code char) 64)
      (code-char (+ 64 (char-code char)))
      (code-char (- (char-code char) 64))))

(defun lower-hex-char-p (char)
  (and (characterp char)
       (or (and (char<= #\a char) (char>= #\f char))
	   (and (char<= #\0 char) (char>= #\9 char)))))

(defun mangle-double-char (char1 char2)
  (code-char (parse-integer (format nil "~a~a" char1 char2)
			    :radix 16)))

(defun plain-tex-char-mangler (char iter)
  (format t "char is ~a~%" char)
  (if (eq :superscript (get-char-cat char))
      (let ((next-char (next-iter iter)))
	(format t "next char is ~a~%" char)
	(if (equal char next-char)
	    (let ((nn-char (soft-next-iter iter)))
	      (format t "next next char is ~a~%" char)
	      (if (eq :stop-iteration nn-char)
		  (progn (push-iter next-char iter)
			 char)
		  (if (lower-hex-char-p nn-char)
		      (let ((nnn-char (soft-next-iter iter)))
			(if (eq :stop-iteration nnn-char)
			    (mangle-single-char nn-char)
			    (if (lower-hex-char-p nnn-char)
				(mangle-double-char nn-char nnn-char)
				(progn (push-iter nnn-char iter)
				       (mangle-single-char nn-char)))))
		      (if (< (char-code nn-char) 127)
			  (mangle-single-char nn-char)
			  (progn (push-iter nn-char iter)
				 (push-iter next-char iter)
				 char)))))
	    (progn (push-iter next-char iter)
		   char)))
      char))

(defun empty-mangler (char iter)
  (declare (ignore iter))
  char)

(defparameter char-mangler nil)
(defun install-plain-tex-mangler ()
  (setf char-mangler #'plain-tex-char-mangler))
(defun uninstall-mangler ()
  (setf char-mangler #'empty-mangler))
(defun install-custom-mangler (fun-pointer)
  (setf char-mangler fun-pointer))
				

(defmethod next-iter ((iter char-mangler))
  (with-slots (sub-iterator) iter
    (let ((next-char (next-iter sub-iterator)))
      (funcall char-mangler next-char sub-iterator))))


(defun mk-char-mangler (sub-iter)
  (make-instance 'char-mangler :sub-iterator sub-iter))

(defun mk-reader-chain (stream)
  (mk-pushable-iter (mk-teh-reader
		     (mk-pushable-iter (mk-char-mangler
					(mk-pushable-iter (mk-simple-chars
							   (mk-prereader stream))))))))
      
(defun tex-tokenize-file (fname)
  ;; TODO: need to be able to save the current state of the reader chain
  ;; Better yet, encapsulate the properties of a reader chain inside it
  (install-plain-tex-reader-chain)
  (with-open-file (stream fname)
    (iter->list (mk-reader-chain stream))))

(defun tex-tokenize-string (string)
  (install-plain-tex-reader-chain)
  (iter->list (mk-reader-chain (make-string-input-stream string))))

(defun foo ()
  (tex-tokenize-file "~/drafts/kauffman-in-a-nutshell/noeuds-d-enfants.tex"))

(defclass teh-reader (over-iter)
  ((entry-point :initform (error "You should set the entry point, otherwise I won't work!")
		:initarg :entry-point)))

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

(defmethod print-object ((obj tex-token) stream)
  (if *print-readably*
      (error 'print-not-readable)
      (with-slots (text cat) obj
	(princ "#<tex-token" stream)
	(if (symbolp text)
	    (progn (princ " " stream)
		   (princ text stream)
		   (princ " " stream))
	    (progn (princ " " stream)
		   (princ cat stream)
		   (princ " " stream)
		   (princ text stream)
		   (princ " " stream)))
	(princ ">" stream))))
      

(defun mk-tex-token (text &optional (cat :undefined))
  (make-instance 'tex-token :text text :cat cat))

(defun other-token (char)
  (make-instance 'tex-token :text char :cat :other))

(defparameter teh-readtable (make-hash-table :test #'equal))

(defun get-char-reader (char)
  (gethash char teh-readtable))
(defun set-char-reader (char fun)
  (setf (gethash char teh-readtable) fun))
(defun clear-char-readtable ()
  (clrhash teh-readtable))


(defun get-char-cat-reader (char)
  (get-cat-reader (get-char-cat char)))

(defparameter char-cats (make-hash-table :test #'equal))

(defun get-char-cat (char)
  (gethash char char-cats :other))
(defun set-char-cat (char cat)
  (setf (gethash char char-cats) cat))
(defun clear-char-cats ()
  (clrhash char-cats))

(defparameter cat-readtable (make-hash-table :test #'equal))

(defun get-cat-reader (cat)
  (gethash cat cat-readtable))
(defun set-cat-reader (cat fun)
  (setf (gethash cat cat-readtable) fun))
(defun clear-cat-readtable ()
  (clrhash cat-readtable))

(defun default-cat-reader (char iter)
  (declare (ignore iter))
  (setf spacing-state :m)
  (mk-tex-token char (get-char-cat char)))

(defun ignored-cat-reader (char iter)
  (declare (ignore char))
  (next-iter iter))

;; (defun superscript-cat-reader (char iter)
;;   (with-slots (sub-iterator) iter

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun keywordicate (x)
    (intern (string x)
	    (find-package "KEYWORD"))))

(defun read-letter-sequence (char iter)
  (let ((res (list char))
	(next-char (next-iter iter)))
    (iter (while (eq :letter (get-char-cat next-char)))
	  (push next-char res)
	  (setf next-char (next-iter iter)))
    (push-iter next-char iter)
    (coerce (nreverse res) 'string)))

(defun escape-cat-reader (char iter)
  (declare (ignore char))
  (with-slots (sub-iterator) iter
    (let ((next-char (next-iter sub-iterator)))
      (cond ((eq :end-of-line next-char)
	     (push-iter next-char sub-iterator)
	     (mk-tex-token '||))
	    ((eq :letter (get-char-cat next-char))
	     (let ((sym (keywordicate (read-letter-sequence next-char sub-iterator))))
	       (setf spacing-state :s)
	       (mk-tex-token sym)))
	    (t (if (eq :space (get-char-cat next-char))
		   (setf spacing-state :s)
		   (setf spacing-state :m))
	       (mk-tex-token (keywordicate next-char)))))))
  

(defparameter spacing-state :n
  "The state of the reader w.r.t reading the spaces. Can be :N, :M or :S")

(defun space-cat-reader (char iter)
  (declare (ignore char))
  (cond ((or (eq :n spacing-state)
	     (eq :s spacing-state))
	 (next-iter iter))
	((eq :m spacing-state)
	 (setf spacing-state :s)
	 (mk-tex-token (code-char 32) :space))
	(t (error "Unknown SPACING-STATE: ~a" spacing-state))))

(defun invalid-cat-reader (char iter)
  (warn "Read an invalid character: ~a" char)
  (next-iter iter))

(defun comment-cat-reader (char iter)
  (declare (ignore char))
  (with-slots (sub-iterator) iter
    (iter (while (not (eq :end-of-line (next-iter sub-iterator)))))
    (push-iter :end-of-line sub-iterator)
    (next-iter iter)))

(defun end-of-line-cat-reader (char iter)
  (declare (ignore char))
  (with-slots (sub-iterator) iter
    (iter (while (not (eq :end-of-line (next-iter sub-iterator)))))
    (push-iter :end-of-line sub-iterator)
    (cond ((eq :n spacing-state) (mk-tex-token :par :undefined))
	  ((eq :m spacing-state) (mk-tex-token (code-char 32) :space))
	  ((eq :s spacing-state) (next-iter iter)))))

(defun eol-reader (char iter)
  (declare (ignore char))
  (setf spacing-state :n)
  (next-iter iter))

(defun vanilla-reader (iter)
  (with-slots (sub-iterator) iter
    (let ((new-char (next-iter sub-iterator)))
      (let ((char-reader (get-char-reader new-char)))
	(if (not char-reader)
	    (let ((cat-reader (get-char-cat-reader new-char)))
	      (if (not cat-reader)
		  (error "Every character is supposed to have a category, so we shouldn't be here! Char is ~a" new-char)
		  (funcall cat-reader new-char iter)))
	    (funcall char-reader new-char iter))))))


(defmethod next-iter ((iter teh-reader))
  (funcall (slot-value iter 'entry-point) iter))

(defun mk-teh-reader (sub-iter)
  (make-instance 'teh-reader :sub-iterator sub-iter :entry-point #'vanilla-reader))

;; OK, now I have to write installers for all these wonderful readers
;; But first, I need to write clearers.

(defun clear-reader-chain ()
  (uninstall-prereader)
  (uninstall-mangler)
  (clear-char-readtable)
  (clear-char-cats)
  (clear-cat-readtable))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun cat-reader-name (x)
    (intern (join "" (string x) "-CAT-READER"))))

(defun %install-plain-tex-reader-chain ()
  (install-plain-tex-prereader)
  (install-plain-tex-mangler)
  ;; install readers for char-categories
  (macrolet ((frob (&rest clauses)
	       `(progn ,@(mapcar (lambda (clause)
				   (if (consp clause)
				       `(set-cat-reader ,(keywordicate (car clause))
							#',(cat-reader-name (cadr clause)))
				       `(set-cat-reader ,(keywordicate clause) #',(cat-reader-name clause))))
				 clauses))))
    (frob (other default) (begin-group default) (end-group default) (math-shift default)
	  (alignment-tab default) (superscript default) (subscript default) (letter default)
	  (other default) (active default) (parameter default)
	  escape end-of-line ignored space comment invalid))
  ;; install reader for :END-OF-LINE - special KLUDGE case
  (set-char-reader :end-of-line #'eol-reader)
  ;; associate categories to characters
  (set-char-cat #\\ :escape)
  (set-char-cat #\{ :begin-group)
  (set-char-cat #\} :end-group)
  (set-char-cat #\$ :math-shift)
  (set-char-cat #\& :parameter)
  (set-char-cat #\return :end-of-line)
  (set-char-cat #\# :parameter)
  (set-char-cat #\^ :superscript)
  (set-char-cat #\_ :subscript)
  (set-char-cat (code-char 0) :ignored)
  (set-char-cat #\space :space)
  (set-char-cat #\~ :active)
  (set-char-cat #\% :comment)
  (set-char-cat (code-char 127) :invalid)
  (iter (for i from 65 to 90)
	(set-char-cat (code-char i) :letter))
  (iter (for i from 97 to 122)
	(set-char-cat (code-char i) :letter))
  ;; TODO: restoration of TeXENV variables
  )

(defun install-plain-tex-reader-chain ()
  (clear-reader-chain)
  (%install-plain-tex-reader-chain))


