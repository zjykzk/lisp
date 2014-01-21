(load "queue.lisp")

(defun enqueue-front (x q)
  (let ((n (list x)))
     (setf (cdr n) (car q))
     (setf (car q) n)
     n))
