
(in-package #:l33tex)

;; OK, first let's try to obtain the POST byte of the dvi file

(defparameter magic-tail 223)

(defparameter dvi-opcodes (make-hash-table :test #'equal))

(defparameter dvi-opcodes-assoc
  '((0 :set-char 0 127)
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
    (243 :fnt-def #'font-der-reader 1 4)
    (247 :pre #'preamble-reader)
    (248 :post #'postamble-reader)
    (249 :post-post #'post-postamble-reader)))

(defun clear-opcodes ()
  (clrhash dvi-opcodes))
(defun populate-opcodes ()
  (iter (for spec in dvi-opcodes-assoc)
	(if (equal 2 (length spec))
	    (setf (gethash (car spec) dvi-opcodes) (cadr spec))
	    (destructuring-bind (code name from to) spec
	      (iter (for cur-code from code)
		    (for i from from to to)
		    (setf (gethash cur-code dvi-opcodes)
			  (list name i)))))))

(defun read-unsigned-nbyte (n stream)
  (iter (for i from (1- n) downto 0)
	(format t "*** pos is ~a, i is ~a~%" (file-position stream) i)
	(summing (ash (read-byte stream) (* 8 i)))))

(defun read-integer (stream)
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
      (file-position stream (read-integer stream))
      (format t "** file position is ~a~%" (file-position stream))
      (read-byte stream))))

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
		    
