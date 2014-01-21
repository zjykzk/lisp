(defun bin-finder (v obj)
	(let ((len (length v)))
		(and (not (zerop len))
				 (finder v obj 0 (- len 1)))))

(defun finder (v obj start end)
	(let ((range (- end start)))
		(if (zerop range)
			(if (eql obj (aref v start))
				obj
				nil)
			(let ((mid (+ start (/ range 2))))
				(let ((obj2 (aref v mid)))
					(if (eql obj (aref v mid))
						obj
						(if (> obj obj2)
							(finder v obj (+ 1 mid) end)
							(finder v obj start (- mid 1)))))))))

