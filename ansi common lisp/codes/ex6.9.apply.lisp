(defun our-apply (fn &rest args)
	(progn
		(let ((*print-base* 8))
			(dolist (e args)
				(typecase e
					(number (princ e))
					(t))))
		(apply fn args)))
;;; (our-apply '+ 1 2 9)
;;; 1211
;;; 12
