(defun most-so-far ()
	(let (max-so-far)
		(defun most-so-far0 (num)
			(if (not max-so-far)
				(setf max-so-far num)
				(setf max-so-far (max num max-so-far))))
		'most-so-far0))

;;; (setf mf (most-so-far0))
;;; (funcall mf 1)
;;; 1
;;; (funcall mf 111)
;;; 111
;;; (funcall mf 11)
;;; 111
