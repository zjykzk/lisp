;;;
;;; (our-if 1 (princ "t") (princ "f"))
;;; (our-if nil (princ "nil"))
(defmacro our-if (test then &optional else)
  `(cond (,test ,then)
         (t ,else)))
