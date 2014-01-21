(defun copy-list-reduce (lst)
	(reduce #'(lambda (a b) (cons a b)) lst
					:from-end t
					:initial-value nil))

(defun reverse-reduce (lst)
	(reduce #'(lambda (a b) (cons b a)) lst
					:initial-value nil))
