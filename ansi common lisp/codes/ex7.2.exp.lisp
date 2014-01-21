(defun ex7.2-exp (filename)
  (with-open-file (file filename :direction
                        :input)
    (do ((exp (read file nil :eof)
              (read file nil :eof))
         (ret))
      ((eql exp :eof) ret)
      (setf ret (append ret (list exp))))))
