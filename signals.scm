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

;;2.36
;(define (first-elem-list seqs)
;  (accumulate (lambda (a b) (cons (car a) b)) (list ) seqs))
(define (first-elem-list seqs)
  (accumulate (lambda (a b)
		(if (null? a)
		  (list )
		  (cons (car a) b)))
	(list )
	seqs))
(define (remind-elem-list seqs)
  (accumulate (lambda (a b)
		(cond ((null? a) (list ))
		      ((null? (cdr a)) (list ))
		      (else (cons (cdr a) b))))
	(list )
	seqs))

(define (accumulate-n op init seqs)
  (if (null? seqs)
    (list )
    (cons (accumulate op init (first-elem-list seqs))
	  (accumulate-n op init (remind-elem-list seqs)))))

(first-elem-list (list (list 1 2 3) (list 4 5 6)))
(remind-elem-list (list (list 1 2 3) (list 4 5 6)))

(accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(accumulate-n + 0 (list (list 1) (list 4) (list 7)))
(accumulate-n + 0 (remind-elem-list (list (list 1) (list 4) (list 7))))

;;2.37
(define (matrix-mapper r v)
  (if (null? r)
    0
    (+ (* (car r) (car v)) (matrix-mapper (cdr r) (cdr v)))
))

(matrix-mapper (list 1 2 3) (list 1 2 3))

(define (matrix-*-vector m v)
  (map (lambda (r) (matrix-mapper r v)) m))

(matrix-*-vector (list (list 1 2 3) (list 4 5 6)) (list 1 2 3))

(define (transpose m) (accumulate-n cons (list ) m))

(transpose (list (list 1 2 3) (list 4 5 6)))

(define (builder a b)
  (newline)
  (display "a:")
  (display a)
  (newline)
  (display "b:")
  (display b)
  (if (pair? b)
    (cons a b)
    (cons a (list b))))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (r) (map (lambda (c) (matrix-mapper r c)) cols)) m)))

(matrix-*-matrix (list (list 1 2 3) (list 4 5 6)) (list (list 1 2) (list 3 4) (list 5 6)))

;;2.38
(define fold-right accumulate)

(define (fold-left op initial sequence)
  (define (iter value sequence)
    (if (null? sequence)
      value
      (iter (op value (car sequence)) (cdr sequence))))
  (iter initial sequence))

(fold-right + 0 (list 1 2 3 4))
(fold-left + 0 (list 1 2 3 4))

(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))

(fold-right list (list ) (list 1 2 3))
(fold-left list (list ) (list 1 2 3))

;;2.39
(define (reverse-right s)
  (fold-right (lambda (x y) (append y (list x))) (list ) s))

(reverse-right (list 1 2 3))

(define (reverse-left s)
  (fold-left (lambda (x y) (cons y x)) (list ) s))

(reverse-left (list 1 2 3))
