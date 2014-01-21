(defun ele-count (e lst)
	(if (null lst)
		0
		(if (eql e (car lst))
			(+ 1 (ele-count e (cdr lst)))
			(ele-count e (cdr lst)))))

(defun unique-count (lst)
	(if (null lst)
		nil
		(let ((f (first lst))
					(s (second lst)))
			(if (equal f s)
				(unique-count (cdr lst))
				(cons f (unique-count (cdr lst)))))))

(defun occurrences (lst)
	(unique-count (sort (mapcar #'(lambda (n) (cons n (ele-count n lst))) lst)
				#'(lambda (e1 e2) (> (cdr e1) (cdr e2))))))
