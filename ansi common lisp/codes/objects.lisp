#|
 | ; single inheritance
 |(defun rget (prop obj)
 |  (multiple-value-bind (val in) (gethash prop obj)
 |    (if in
 |      (values val in)
 |      (let ((par (gethash :parent obj)))
 |        (and par (rget prop par))))))
 |#

#|
 |;;;; multiple inheritance
 |(defun rget (prop obj)
 |  (dolist (c (precedence obj))
 |    (multiple-value-bind (val in) (gethash prop c)
 |      (if in (return (values val in))))))
 |#

(defun precedence (obj)
	(labels ((traverse (x)
										 (cons x (mapcan #'traverse
										 								 (gethash :parents x)))))
		(delete-duplicates (traverse obj))))

(defun tell (obj message &rest args)
	(apply (rget message obj) obj args))

#|
> (setf circle-class (make-hash-table)
			our-circle (make-hash-table)
			(gethash :parent our-circle) circle-class
			(gethash 'radius our-circle) 2)
2

> (setf (gethash 'area circle-class)
			#'(lambda (x) (* pi (expt (rget 'radius x) 2))))
#... something

> (rget 'radius our-circle)
2
T

> (tell our-circle 'area)
12.566370614359172954L0
 |#

(defvar *objs* nil)

(defun parents (obj) (gethash :parents obj))

(defun (setf parents) (val obj)
	(prog1 (setf (gethash :parents obj) val)
		(make-precedence obj)))

(defun make-precedence (obj)
	(setf (gethash :preclist obj) (precedence obj))
	(dolist (x *objs*)
		(if (member obj (gethash :preclist x))
			(setf (gethash :preclist x) (precedence x)))))

(defun obj (&rest parents)
	(let ((obj (make-hash-table)))
		(push obj *objs*)
		(setf (parents obj) parents)
		obj))

#|
 | ; not suitable for instance
 |(defun rget (prop obj)
 |  (dolist (c (gethash :preclist obj))
 |    (multiple-value-bind (val in) (gethash prop c)
 |      (if n (return (values val in))))))
 |#

(defmacro defprop (name &optional meth?)
	`(progn
		 (defun ,name (obj &rest args)
		 	 ,(if meth?
		 	 		`(run-methods obj ',name args)
		 	 		`(rget ',name obj)))
		 (defun (setf ,name) (val obj)
		 	 (setf (gethash ',name obj) val))))

(defun run-methods (obj name args)
	(let ((meth (rget name obj)))
		(if meth
			(apply meth obj args)
			(error "No ~A method for ~A." name obj))))

(defmacro defmeth (name obj parms &rest body)
	(let ((gobj (gensym)))
		`(let ((,gobj ,obj))
			 (setf (gethash ',name ,gobj)
			 			 (labels ((next () (get-next ,gobj ',name)))
			 			 	 #'(lambda ,parms ,@body))))))

#|
 | ; not suitable for instance
 |(defun get-next (obj name)
 |  (some #'(lambda (x) (gethash namex))
 |        (cdr (gethash :preclist obj))))
 |#

(defun inst (parent)
	(let ((obj (make-hash-table)))
		(setf (gethash :parents obj) parent)
		obj))

(defun rget (prop obj)
	(let ((prec (gethash :preclist obj)))
		(if prec
			(dolist (c prec)
				(multiple-value-bind (val in) (gethash prop c)
					(if in (return (values val in)))))
			(multiple-value-bind (val in) (gethash prop obj)
				(if in
					(values val in)
					(rget prop (gethash :parents obj)))))))

(defun get-next (obj name)
	(let ((prec (gethash :preclist obj)))
		(if prec
			(some #'(lambda (x) (gethash name x))
						(cdr prec))
			(get-next (gethash :parents obj) name))))
