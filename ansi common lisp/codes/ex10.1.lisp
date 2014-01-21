(setf x 'a)
(setf y 'b)
(setf z '(c d))

;;;; a
`(,z ,x z)
`(x ,y ,@z)
`((,@z ,x) z)
