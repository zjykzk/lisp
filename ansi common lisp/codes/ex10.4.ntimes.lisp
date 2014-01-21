(defmacro ex-ntimes (n &rest body)
  `(labels ((do-ntimes (n)
                       (if (<= n 0)
                         nil
                         (progn ,@body (do-ntimes (decf n))))))
     (do-ntimes ,n)))
