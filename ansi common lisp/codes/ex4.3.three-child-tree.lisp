(defstruct (3node (:print-function
										(lambda (n s d)
											(format s "#<~A>" (3node-data n))))
						data
						(f nil)
						(s nil)
						(t nil)))

(defun 3tree-insert (obj tree <)
	(if (null tree)
		(make-3node :data obj)
		(let ((data (3node-data tree)))
			(cond ((eql data obj) tree)
						((funcall < obj data)
