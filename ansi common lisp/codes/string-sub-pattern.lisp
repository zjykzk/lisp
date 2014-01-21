(defstruct buf
	vec (start -1) (used -1) (new -1) (end -1))

(defun bref (buf n)
	(svref (buf-vec buf)
				 (mod n (length (buf-vec buf)))))

(defun (setf bref) (val buf n)
	(setf (svref (buf-vec buf)
							 (mod n (length (buf-vec buf))))
				val))

(defun new-buf (len)
	(make-buf :vec (make-array len)))

(defun buf-insert (x b)
	(setf (bref b (incf (buf-end b))) x))

(defun buf-pop (b)
	(prog1
		(bref b (incf (buf-start b)))
		(setf (buf-used b) (buf-start b)
					(buf-new b) (buf-end b))))

(defun buf-next (b)
	(when (< (buf-used b) (buf-new b))
		(bref b (incf (buf-used b)))))

(defun buf-reset (b)
	(setf (buf-used b) (buf-start b)
				(buf-new b) (buf-end b)))

(defun buf-clear (b)
	(setf (buf-start b) -1 (buf-used b) -1
				(buf-new b) -1 (buf-end b) -1))

(defun buf-flush (b str)
	(do ((i (1+ (buf-used b)) (1+ i)))
		((> i (buf-end b)))
		(princ (bref b i) str)))

(defun file-subst (old new file1 file2)
	(with-open-file (in file1 :direction :input)
		(with-open-file (out file2 :direction :output
												 :if-exists :supersede)
			(stream-subst old new in out))))

;;;
;;; '.' match any character
;;; '\w' match any alphanumberic character
;;; '\d' match any digit character
(defun our-match (pattern pos ch)
	(case (char pattern pos)
		(#\. (values 1 t))
		(#\\
		 (case (char pattern (+ pos 1)) 
				(#\w (values 2 (alphanumericp ch)))
				(#\d (values 2 (and (alphanumericp ch) (not (alpha-char-p ch)))))
				(t (values 1 (char= #\\ ch)))))
		(t (values 1 (char= (char pattern pos) ch)))))

#|
 |(defun our-match (pattern pos ch)
 |  (format t "our-match : ~A ~A ~A ~A~%" pattern pos ch 
 |    (char= (char pattern pos) ch))
 |  (values 1 (char= (char pattern pos) ch)))
 |#

(defun test-our-match (pattern pos ch)
	(multiple-value-bind (forward-step matched) (our-match pattern pos ch)
		(format t "test match : ~A ~A ~%" forward-step matched)))

(defun stream-subst (old new in out)
	(let* ((pos 0)
				 (len (length old))
				 (buf (new-buf len))
				 (from-buf nil))
		(do ((c (read-char in nil :eof)
						(or (setf from-buf (buf-next buf))
								(read-char in nil :eof))))
			((eql c :eof))
			(multiple-value-bind (forward-step matched) (our-match old pos c)
				(cond (matched
								(incf pos forward-step)
								(cond ((= pos len)						; 3
											 (princ new out)
											 (setf pos 0)
											 (buf-clear buf))
											((not from-buf)				; 2
											 (buf-insert c buf))))
							((zerop pos)									; 1
							 (princ c out)
							 (when from-buf
								 (buf-pop buf)
								 (buf-reset buf)))
							(t														; 4
								(unless from-buf
									(buf-insert c buf))
								(princ (buf-pop buf) out)
								(buf-reset buf)
								(setf pos 0)))))
		(buf-flush buf out)))
