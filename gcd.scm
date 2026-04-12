(define (calc x y)
	(/
		(+ (/ x y) y)
		2
	)
)
(define (improve x y times)
	(if (= times 0)
		y
		(let ((new-guess (calc x y)))
			(display new-guess)
			(newline )
			(improve x new-guess (- times 1))
		)
	)
)


;;2.1
(define (make-rat n d) 
	(let ((m (* n d)))
		(if (> m 0)
			(cons (abs n) (abs d))
			(cons (* -1 (abs n)) (abs d))
		)
	)
)

(make-rat 1 2)
(make-rat -1 -2)
(make-rat 1 -2)
(make-rat -1 2)
