;;; (parse-date "16 Aug 1984")
;;; (16 8 1984)

(defun constituent (c)
	(and (graphic-char-p c)
			 (not (char= c #\ ))))

(defun tokens (str &key (test #'constituent) (start 0))
	(let ((p1 (position-if test str :start start)))
		(if p1
			(let ((p2 (position-if #'(lambda (c)
																 (not (funcall test c)))
														 str :start p1)))
				(cons (subseq str p1 p2)
							(if p2
								(tokens str :test test :start p2)
								nil)))
			nil)))

(defun parse-date (str)
	(let ((toks (tokens str 'constituent 0)))
		(list (parse-integer (first toks))
					(parse-month (second toks))
					(parse-integer (third toks)))))

(defun parse-month (str)
	(let ((p (position str months :test #'string-equal)))
		(if p
			(+ 1 p)
			nil)))

(defconstant months
						 #("jan" "feb" "mar" "apr" "may" "jun"
						 	 "jul" "aug" "sep" "otc" "nov" "dec"))
