(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence) (accumulate op initial (cdr sequence)))
    )
)


(accumulate (lambda (x y) (+ x y)) 0 (list 1 2 3 4 5))

(/ (* (+ 5 1) 5) 2)


(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) () sequence))


(map (lambda (x) (* x 2)) (list 1 2 3))


(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(append (list 1 2 3) (list 4 5 6))

(define (length seq)
  (accumulate (lambda (x y) (+ y 1)) 0 seq))

(length (list 1 2 3 4 442 234 423 423 423))

(define (horner-val x cs)
  (accumulate (lambda (a b) (+ (* b x) a))
              0
              cs))

(horner-val 2 (list 1 2 3 4))
