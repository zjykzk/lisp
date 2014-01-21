;;; (let ((n 2)) (nth-expr n (/ 1 0) (+ 1 2) (/ 1 0)))
;;; 3
;;; 
(defmacro nth-expr (n expr &rest rst)
  `(case ,n
         (0 format("argument 'n' must be > 0 , n: ~A~%" n))
         (1 ,expr)
         ,@(let ((key 1))
             (mapcar #'(lambda (e) `(,(incf key) ,e)) rst))))
