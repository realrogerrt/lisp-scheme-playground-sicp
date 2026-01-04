(define (list-get l i)
	(define (list-get-impl l i c)
		(if (= c i)
			(car l)
			(list-get-impl (cdr l) i (+ c 1))
		)
	)
	(list-get-impl l i 0)
)




(define myl (list 4 5 6))
(list-get myl 0)
(list-get myl 1)
(list-get myl 2)

(cons 1 (list 2 3))
(cons (list 2 3) 1)

(define (reverse-list l)
	;(define (iter left right)
	;	(if (null? left)
	;		right
	;		(iter (cdr left) (cons (car left) right))
	;	)
	;)
	;(iter l '())
	(if (= 1 (length l))
		l
		(append (reverse-list (cdr l)) (list (car l)))
	)
)

(display (reverse-list (list 1 2 3 4)))

(define (same-parity . els)
	(define (iter first remaining result)
		(if (null? remaining)
			result
			(let ((current (car remaining)))
				(if (= (remainder first 2) (remainder current 2))
					;(iter first (cdr remaining) (cons current result))
					(iter first (cdr remaining) (append result (list current)))
					(iter first (cdr remaining) result)
				)
			)
		)
	)
	(iter (car els) els '())
)

(same-parity 1 2 3 4 5)
(same-parity 2 3 4 5 6)

(define (for-each proc l)
	(if (null?  l)
		#f
		(let ((c (car l)))
			(proc c)
			(for-each proc (cdr l))
		)
	)
)

(for-each (lambda (x) (newline) (display x)) (list 123 4325 34324))

(define (uniques l)
	(define (unique-impl l r)
		(cond
			((= (length l) 0) r)
			(else (if (= (car r) (car l))
				(unique-impl (cdr l) r)
				(unique-impl (cdr l) (cons (car l) r))
				)
			)
		)
	)
	(unique-impl (cdr l) (list (car l)))
)

(display "uniques:")
(newline )
(uniques (list 1 2 2 3 3 3))


(define (fringe l)
	(if (pair? l)
		(if (null? (cdr l))
			(fringe (car l))
			(append (fringe (car l)) (fringe (cdr l)))
		)
		(list l)
	)
)

(define tree (list (list 1 2) (list (list 4 5) ) 5 (list 3 4)))
(fringe tree)
