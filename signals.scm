(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence) (accumulate op initial (cdr sequence)))
    )
)


(accumulate (lambda (x y) (+ x y)) 0 (list 1 2 3 4 5))
(/ (* (+ 5 1) 5) 2)


;;2.33 a
(define (map-using-accumulate p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) () sequence))


(map (lambda (x) (* x 2)) (list 1 2 3))
(map-using-accumulate (lambda (x) (* x 2)) (list 1 2 3))


;;2.33 b
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(append (list 1 2 3) (list 4 5 6))

;;2.33 c
(define (length seq)
  (accumulate (lambda (x y) (+ y 1)) 0 seq))

(length (list 1 2 3 4 442 234 423 423 423))

;;2.34
(define (horner-val x cs)
  (accumulate (lambda (a b) (+ (* b x) a))
              0
              cs))

(horner-val 2 (list 1 2 3 4))
(horner-val 2 (list 1 3 0 5 0 1))

(define (map proc items)
  (if (null? items)
    items
    (cons (proc (car items))
	  (map proc (cdr items)))))


;;2.35
(define (leaves-mapper it)
  (cond ((null? it) 0)
	((pair? it) (count-leaves it))
	(else 1))
)

(define (count-leaves t)
	(accumulate + 0 (map leaves-mapper t)))

(define tree (list (list 1 2 3) (list (list 4 5) (list 6 7)) 8))

(count-leaves tree)
