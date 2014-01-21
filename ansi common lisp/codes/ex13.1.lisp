(declaim (optimize (speed 3)
                   (compilation-speed 0)
                   (safety 0)
                   (debug )))

(defun test-inline ()
  "test-inline")

(defun call-test-inline ()
  (format t "~A~%" (test-inline)))
;;; clisp does not support inline
