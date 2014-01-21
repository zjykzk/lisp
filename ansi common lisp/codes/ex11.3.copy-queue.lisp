(load "queue.lisp")

(defun copy-queue (q)
  (let ((r (make-queue))
        (qq (mapcar #'(lambda (e) e) (car q))))
    (setf (cdr r) (last qq)
          (car r) qq)
    r))
