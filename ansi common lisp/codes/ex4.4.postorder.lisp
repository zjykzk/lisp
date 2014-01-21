(load "bts.lisp")
(defun post-order (bst)
	(if (null bst)
		nil
		(append (post-order (node-r bst))
						(list (node-elt bst))
						(post-order (node-l bst)))))
