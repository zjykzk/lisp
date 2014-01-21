(defun precedes-iter (c str)
	(let ((len (length str))
				ret
				prev)
		(dotimes (i len i)
			(if (and (> i 0) (char= c (char str i)))
				(setf ret (cons (char str (- i 1)) ret))
				nil)
			(setf prev (char str i)))
		ret))

(defun precedes-recursive (c str)
	(if (or (null str) (= 1 (length str)))
		nil
		(if (char= c (char str 1))
			(cons (char str 0) (precedes-recursive c (subseq str 1)))
			(precedes-recursive c (subseq str 1)))))

;;; (precedes-recursive #\a "abracadabra")
;;; (#\r #\c #\d #\r)
;;; (precedes-iter #\a "abracadabra")
;;; (#\r #\c #\d #\r)
