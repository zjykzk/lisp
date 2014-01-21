(defun ex7.1-lines (filename)
	(with-open-file (file filename :direction
												:input)
		(do ((line (read-line file nil :eof)
							 (read-line file nil :eof))
				 (ret))
			((eql line :eof) ret)
			(setf ret (append ret (list line))))))
