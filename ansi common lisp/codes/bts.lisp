(defstruct (node (:print-function
									 (lambda (n s d)
									 	 (format s "#<~A>" (node-elt n)))))
	elt (l nil) (r nil))

#|
 |(defun bst-insert (obj bst <)
 |  (if (null bst)
 |    (make-node :elt obj)
 |    (let ((elt (node-elt bst)))
 |      (if (eql obj elt)
 |        bst
 |        (if (funcall < obj elt)
 |          (if (null (node-l elt))
 |            (setf (node-l elt) (make-node :elt obj))
 |            (bst-insert obj (node-l bst) <))
 |          (if (null (node-r elt))
 |            (setf (node-r elt) (make-node :elt obj))
 |            (bst-insert obj (node-r bst) <)))))))
 |#
(defun bst-insert (obj bst <)
	(if (null bst)
		(make-node :elt obj)
		(let ((elt (node-elt bst)))
			(if (eql obj elt)
				bst
				(if (funcall < obj elt)
					(make-node :elt elt
										 :l (bst-insert obj (node-l bst) <)
										 :r (node-r bst))
					(make-node :elt elt
										 :l (node-l bst)
										 :r (bst-insert obj (node-r bst) <)))))))

(defun bst-find (obj bst <)
	(if (null bst)
		nil
		(let ((elt (node-elt bst)))
			(if (eql obj elt)
				bst
				(if (funcall < elt obj)
					(bst-find obj (node-r bst) <)
					(bst-find obj (node-l bst) <))))))

(defun bst-min (bst)
	(and bst
			 (or (bst-min (node-l bst)) bst)))

(defun bst-max (bst)
	(and bst
			 (or (bst-max (node-r bst)) bst)))

(defun bst-remove (obj bst <)
	(if (null bst)
		nil
		(let ((elt (node-elt bst)))
			(if (eql obj elt)
				(percolate bst <)
				(if (funcall < obj elt)
					(make-node :elt elt
										 :l (bst-remove obj (node-l bst) <)
										 :r (node-r bst))
					(make-node :elt elt
										 :l (node-l bst)
										 :r (bst-remove obj (node-r bst) <)))))))

(defun percolate (bst <)
	(cond ((null (node-l bst))
				 (if (null (node-r bst))
				 	 nil
				 	 (rperc bst <)))
				((null (node-r bst)) (lperc bst <))
				(t (let ((lelt (node-elt (node-l bst)))
								 (relt (node-elt (node-r bst))))
						 (if (funcall < lelt relt)
						 	 (rperc bst <)
						 	 (lperc bst <))))))

(defun rperc (bst <)
	(make-node :elt (node-elt (node-r bst))
						 :r (percolate (node-r bst) <)
						 :l (node-l bst)))

(defun lperc (bst <)
	(make-node :elt (node-elt (node-l bst))
						 :r (node-r bst)
						 :l (percolate (node-l bst) <)))

(defun bst-traverse (fn bst)
	(when bst
		(bst-traverse fn (node-l bst))
		(funcall fn (node-elt bst))
		(bst-traverse fn (node-r bst))))
						 
;;; test
;;; (setf nums nil)
;;; (dolist (x '(5 8 4 2 1 9 6 7 3))
;;; 	(setf nums (bst-insert x nums #'<)))
;;; (bst-find 9 nums #'<)
;;; #<4>
;;;
;;; (bst-min nums)
;;; #<1>
;;;
;;; (bst-max nums)
;;; #<9>
;;;
;;; (setf n (bst-remove 2 nums))
;;; #<5>
;;;
;;; (bst-find 2 n #'<)
;;; NIL
;;;
;;; (bst-traverse #'princ nums)
;;; 123456789
;;; NIL
