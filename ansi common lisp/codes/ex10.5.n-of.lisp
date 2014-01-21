;;;; (let ((i 0) (n 4)) (n-of n (incf i)))
;;;; (1 2 3 4) 
;;;;
(defmacro n-of (n expr)
  (let ((sym (gensym)))
    `(let ((ret))
       (dotimes (,sym ,n ,sym)
         ; here double comma, since double backquote
         (setf ret (append ret `(,,expr)))) 
       ret)))
