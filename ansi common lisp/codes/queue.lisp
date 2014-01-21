(defun make-queue () (cons nil nil))

(defun enqueue (obj q)
  (if (null (car q))
    (setf (cdr q) (setf (car q) (list obj)))
    (setf (cdr (cdr q)) (list obj)
          (cdr q) (cdr (cdr q))))
  (car q))

(defun dequeue (q)
  (pop (car q)))

;;;
;;; (setf q (make-queue))
;;; (NIL)
;;; (progn (enqueue 'a q) (enqueue 'b q) (enqueue 'c q))
;;; (A B C)
