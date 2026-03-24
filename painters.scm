;2.44
(define (beside a b) 
  (string "(beside " a " " b ")")
  ;(lambda () (beside a b))
)
(define (below a b)
  (string "(below " a " " b ")")
  ;(lambda () (below a b))
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

;(corner-split 1 3)
;(beside 1 2)

;;2.46

;make-vect xcor-vect and ycor-vect
;add-vect, sub-vect, and scale-vect

(define (make-vect a b) (cons a b))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
	     (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
	     (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v)) (* s (ycor-vect v))))

(define (make-frame o e1 e2) (list o e1 e2))
(define (origin-frame f) (car f))
(define (edge1-frame f) (car (cdr f)))
(define (edge2-frame f) (car (cdr (cdr f))))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
      (origin-frame frame)
      (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
		(scale-vect (ycor-vect v) (edge2-frame frame))))))

(define a-frame (make-frame (make-vect 1 2) (make-vect 3 4) (make-vect 5 6)))
((frame-coord-map a-frame) (make-vect 0 0))
