(defun foo (x)
  (if (zerop x)
    0
    (+ 1 (foo (1- x)))))

(defun foo-tail-recursive (ret x)
  (if (zerop x)
    ret
    (foo-tail-recursive (+ ret x) (1- x))))
