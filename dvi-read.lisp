
(in-package #:l33tex)

;; OK, first let's try to obtain the POST byte of the dvi file

(defparameter magic-tail 223)

(defparameter dvi-opcodes (make-hash-table :test #'equal))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %populate-opcodes (spec)
    (destructuring-bind (start-code name . spec) spec
      (cond ((null spec) `(setf (gethash ,start-code dvi-opcodes) (list ,name #'no-reader)))
	    (t (let ((reader nil))
		 ;; (format t "car spec is ~a~%" (car spec))
		 (if (not (numberp (car spec)))
		     (progn ;; (format t "Imhere!~%")
			    (setf reader (car spec)
				  spec (cdr spec)))
		     (setf reader '#'no-reader))
		 ;; (format t "name ~a reader ~a spec ~a~%" name reader spec)
		 (if (null spec)
		     `(setf (gethash ,start-code dvi-opcodes) (list ,name ,reader))
		     `(progn ,@(iter (for cur-code from start-code)
				     (for i from (car spec) to (cadr spec))
				     (collect `(setf (gethash ,cur-code dvi-opcodes)
						     (list ,name ,reader ,i))))))))))))


(defmacro populate-opcodes (&body specs)
  `(progn ,@(mapcar #'%populate-opcodes specs)))

(defun read-unsigned-nbyte (n stream)
  (iter (for i from (1- n) downto 0)
	(format t "*** pos is ~a, i is ~a~%" (file-position stream) i)
	(summing (ash (read-byte stream) (* 8 i)))))

(defun read-unsigned-int (stream)
  (read-unsigned-nbyte 4 stream))



(defun pick-byte (stream)
  (let ((res (read-byte stream)))
    (file-position stream (- (file-position stream) 1))
    res))

(defun post-byte (fname)
  (with-open-file (stream fname :element-type '(unsigned-byte 8))
    (let ((len (file-length stream)))
      (file-position stream (1- len))
      (format t "file position is ~a~%" (file-position stream))
      (iter (while (equal magic-tail (pick-byte stream)))
	    (format t "file position is ~a~%" (file-position stream))
	    (file-position stream (- (file-position stream) 1)))
      (file-position stream (- (file-position stream) 4))
      (format t "** file position is ~a~%" (file-position stream))
      (file-position stream (read-unsigned-int stream))
      (format t "** file position is ~a~%" (file-position stream))
      (read-byte stream))))

(defun slurp-dvi-file (fname)
  (with-open-file (stream fname :element-type '(unsigned-byte 8))
    (read-until stream nil)))

(defun foo ()
  (post-byte "~/code/tex-examples/story.dvi"))


(defun ub-reader (stream opcode name lst)
  (declare (ignore opcode))
  (let ((char (read-unsigned-nbyte (car lst) stream)))
    (list name char)))

(defun sb-reader (stream opcode name lst)
  (declare (ignore opcode))
  (let ((val (read-signed-nbyte (car lst) stream)))
    (list name val)))


(defun no-reader (stream opcode name lst)
  (declare (ignore stream opcode))
  (cons name (copy-list lst)))

(defun read-signed-nbyte (n stream)
  (let ((val (read-unsigned-nbyte n stream)))
    (if (not (equal 0 (boole boole-and (ash 1 (1- (* 8 n))) val)))
	(- (expt 2 (* 8 n)) val)
	val)))

(defun read-signed-int (stream)
  (read-signed-nbyte 4 stream))

(defun double-sb-reader (stream opcode name lst)
  (declare (ignore lst opcode))
  (list name (read-signed-int stream) (read-signed-int stream)))

(defun bop-reader (stream opcode name lst)
  (declare (ignore opcode lst))
  (cons name 
	(iter (for i from 0 to 10)
	      (collect (read-unsigned-int stream)))))

(defun read-byte-array (size stream)
  (let ((buf (make-array (list size) :element-type '(unsigned-byte 8))))
    (read-sequence buf stream)
    buf))


(defun special-reader (stream opcode name lst)
  (declare (ignore opcode))
  (let ((size (read-unsigned-nbyte (car lst) stream)))
    (list name (read-byte-array size stream))))


(defun font-def-reader (stream opcode name lst)
  (declare (ignore opcode))
  (let ((k (read-unsigned-nbyte (car lst) stream))
	(checksum (read-unsigned-int stream))
	(scale (read-unsigned-int stream))
	(design-size (read-unsigned-int stream))
	(dir-length (read-unsigned-nbyte 1 stream))
	(fname-length (read-unsigned-nbyte 1 stream)))
    (let ((dir (read-byte-array dir-length stream))
	  (fname (read-byte-array fname-length stream)))
      (list name :id k :checksum checksum :scale scale :design-size design-size
	    :dir dir :file-name fname))))


(defun read-until (stream end-opcode)
  (let ((cur-opcode nil)
	(res nil))
    (iter (while t)
	  (setf cur-opcode (read-byte stream nil :eof))
	  (if (eq :eof cur-opcode)
	      (terminate)
	      (destructuring-bind (name reader . specs) (gethash cur-opcode dvi-opcodes)
		(when (eq end-opcode name)
		  (file-position stream (1- (file-position stream)))
		  (terminate))
		(push (funcall reader stream cur-opcode name specs) res))))
    (nreverse res)))
		    
(defun preamble-reader (stream opcode name lst)
  (declare (ignore opcode lst))
  (let ((version (read-unsigned-nbyte 1 stream))
	(num (read-unsigned-int stream))
	(den (read-unsigned-int stream))
	(mag (read-unsigned-int stream))
	(comment-size (read-unsigned-nbyte 1 stream)))
    (let ((comment (read-byte-array comment-size stream)))
      (list name :version version :num num :den den :mag mag :comment comment))))

(defun postamble-reader (stream opcode name lst)
  (declare (ignore lst opcode))
  (let ((p (read-unsigned-int stream))
	(num (read-unsigned-int stream))
	(den (read-unsigned-int stream))
	(mag (read-unsigned-int stream))
	(l (read-unsigned-int stream))
	(u (read-unsigned-int stream))
	(max-stack-size (read-unsigned-nbyte 2 stream))
	(pages (read-unsigned-nbyte 2 stream)))
    (list name :last-page-address p :num num :den den :mag mag
	  :max-height-plus-depth l :max-width u
	  :max-stack-size max-stack-size
	  :num-pages pages)))
	
(defun post-postamble-reader (stream opcode name lst)
  (declare (ignore lst opcode))
  (let ((postamble-pointer (read-unsigned-int stream))
	(version (read-unsigned-nbyte 1 stream)))
    (let ((num-magic 0))
      (iter (while t)
	    (let ((byte (read-byte stream nil :eof)))
	      (cond ((eq byte :eof) (terminate))
		    ((equal magic-tail byte) (incf num-magic))
		    (t (error "Wrong opcode found in post-postamble: ~a" byte)))))
      (list name :postamble-pointer postamble-pointer :version version :num-magic num-magic))))
			
	
(populate-opcodes 
  (0 :set-char 0 127)
  (128 :set #'ub-reader 1 4)
  (132 :set-rule #'double-sb-reader)
  (133 :put #'ub-reader 1 4)
  (137 :put-rule #'double-sb-reader)
  (138 :nop) (139 :bop #'bop-reader) (140 :eop)
  (141 :push) (142 :pop)
  (143 :right #'sb-reader 1 4)
  (147 :w #'sb-reader 0 4)
  (152 :x #'sb-reader 0 4)
  (157 :down #'sb-reader 1 4)
  (161 :y #'sb-reader 0 4)
  (166 :z #'sb-reader 0 4)
  (171 :fnt-num 0 63)
  (235 :fnt #'ub-reader 1 4)
  (239 :xxx #'special-reader 1 4)
  (243 :fnt-def #'font-def-reader 1 4)
  (247 :pre #'preamble-reader)
  (248 :post #'postamble-reader)
  (249 :post-post #'post-postamble-reader))
