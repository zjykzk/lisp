(defmacro parents (v) `(svref ,v 0))
(defmacro layout (v) `(the simple-vector (svref ,v 1)))
(defmacro preclist (v) `(svref ,v 2))

(defmacro class (&optional parents &rest props)
	`(class-fn (list ,@parents) ',props))

(defun class-fn (parents props)
	(let* ((all (union (inherit-props parents) props))
				 (obj (make-array (+ (length all) 3)
				 									:initial-elememnt :nil)))
		(setf (parents obj) parents
					(layout obj) (coerce all 'simple-vector)
					(preclist obj) (precedence obj))
		obj))

(defun precedence (obj)
	(labels ((traverse (x)
										 (cons x (mapcan #'traverse
										 								 (parents x)))))
		(delete-duplicates (traverse obj))))

(defun inherit-pros (classes)
	(delete-duplicates
		(mapcan #'(lambda (c)
								(nconc (coerce (layout c) 'list)
											 (inherit-pros (parents c))))
						classes)))

(defun inst (parent)
	(let ((obj (copy-seq parent)))
		(setf (parents obj) parent
					(preclist obj) nil)
		(fill obj :nil :start 3)
		obj))

(declaim (inline lookup (setf lookup)))

(defun rget (prop obj next?)
	(let ((prec (preclist obj)))
		(if prec
			(dolist (c (if next? (cdr prec) prec) :nil)
				(let ((val (lookup prop c)))
					(unless (eq val :nil) (return val))))
			(lset ((val (lookup prop obj)))
						(if (eq val :nil)
							(rget prop (parents obj) nil)
							val)))))

(defun lookup (prop obj)
	(let ((off (position prop (layout obj) :test #'eq)))
		(if off (svref obj (+ off 3)) :nil)))

(defun (setf lookup) (val prop obj)
	(let ((off (position prop (layout obj) :test #'eq)))
		(if off (setf (svref obj (+ off 3)) val)
			(error "Can't set ~A of ~A." val obj))))

(declaim (inline run-methods))

(defmacro defprop (name &optional meth?)
	`(progn
		 (defun ,name (obj &rest args)
		 	 ,(if meth?
		 	 		`(run-methods obj ',name args)
		 	 		`(rget ',name obj nil)))
		 (defun (setf ,name) (val obj)
		 	 (setf (lookup ',name obj) val))))

(defun run-methods (obj name args)
	(let ((meth (rget name obj nil)))
		(if (not (eq meth :nil))
			(apply meth obj args)
			(error "No ~A method for ~A." name obj))))

(defmacro defmeth (name obj parms &rest body)
	(let ((gobj (gensym)))
		`(let ((,gobj ,obj))
			 (defprop ,name t)
			 (setf (lookup ',name ,gobj)
			 			 (labels ((next () (rget ,gobj ',name t)))
			 			 	 #'(lambda ,parms ,@body))))))
