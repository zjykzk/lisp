(defclass rectangle ()
  ((height :accessor rect-height
           :initarg :h
           :initform 0)
   (width :accessor rect-width
          :initarg :w
          :initform 0)))

(defclass circle ()
  ((radius :accessor circle-radius
           :initarg :r
           :initform 0)))

(defmethod area ((x rectangle))
  (* (rect-height x) (rect-width x)))

(defmethod area ((x circle))
  (* pi (expt (circle-radius x) 2)))

#|
 |(let ((r (make-instance 'rectangle))) 
 |  (setf (rect-height r) 2 
 |        (rect-width r) 3) 
 |  (area r))
 |
 |6
 |#
