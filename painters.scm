;2.44
(define (beside a b) 
  (string "(beside " a " " b ")")
)
(define (below a b)
  (string "(below " a " " b ")")
)

;2.45
(define (split a b)
  (lambda (painter n)
	(if (= n 0)
	    painter
	    (let ((smaller (right-split painter (- n 1))))
	      (a painter (b smaller smaller))))))

(define right-split (split beside below))
(define up-split (split below beside))

(define (right-split0 painter n)
  (if (= n 0)
    painter
    (let ((smaller (right-split painter (- n 1))))
      (beside painter (below smaller smaller)))))

(define (up-split0 painter n)
  (if (= n 0)
    painter
    (let ((smaller (up-split painter (- n 1))))
      (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
    painter
    (let ((up (up-split painter (- n 1)))
	  (right (right-split painter (- n 1))))
      (let ((top-left (beside up up))
	    (bottom-right (below right right))
	    (corner (corner-split painter (- n 1))))
	(beside (below painter top-left)
		(below bottom-right corner))))))

(corner-split 1 3)
