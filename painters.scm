;2.44
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame
                   new-origin
                   (sub-vect (m corner1) new-origin)
                   (sub-vect (m corner2) new-origin)))))))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
            (transform-painter
              painter1
              (make-vect 0.0 0.0)
              split-point
              (make-vect 0.0 1.0)))
          (paint-right
            (transform-painter
              painter2
              split-point
              (make-vect 1.0 0.0)
              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-top
            (transform-painter
              painter1
              split-point
              (make-vect 1.0 0.5)
              (make-vect 0.0 1.0)))
          (paint-bottom
            (transform-painter
              painter2
              (make-vect 0.0 0.0)
              (make-vect 1.0 0.0)
              split-point)))
      (lambda (frame)
        (paint-top frame)
        (paint-bottom frame)))))


;2.45
(define (split a b)
  (define (split-impl painter n)
    (if (= n 0)
      painter
      (let ((smaller (split-impl painter (- n 1))))
        (a painter (b smaller smaller)))))
  split-impl)

(define right-split (split beside below))
(define up-split (split below beside))


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

(define a-frame (make-frame (make-vect 2 2) (make-vect 300 0) (make-vect 100 200)))
(define b-frame (make-frame (make-vect 2 2) (make-vect 1 0) (make-vect 0 1)))
(define c-frame (make-frame (make-vect 2 2) (make-vect 300 0) (make-vect 100 200)))
((frame-coord-map a-frame) (make-vect 1 1))

;;2.48
(define (make-segment v1 v2) (cons v1 v2))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

;Attempt at drawing in the terminal
; (define (make-geo-vect a b)
;   (let ((xa (xcor-vect a))
;         (xb (xcor-vect b))
;         (ya (ycor-vect a))
;         (yb (ycor-vect b)))
;     22))
;     (if (= xa xb)
;        (list a b "naf" "naf"))
;         (let ((m (/ (- ya yb) (- xa xb))))
;
;        (list a b ))))



;; to be used in an html canvas context
(define (draw-line a b)
  (newline )
  (display (string "ctx.moveTo(" (xcor-vect a) ", " (ycor-vect a) ");"))
  (newline )
  (display (string "ctx.lineTo(" (xcor-vect b) ", " (ycor-vect b) ");")))

(include "lists.scm")

(define (segments->painter segments-list)
  (lambda (frame)
    (for-each
      (lambda (segment)
        (draw-line
          ((frame-coord-map frame) (start-segment segment))
          ((frame-coord-map frame) (end-segment segment))))
      segments-list)))

((segments->painter (list
                      (make-segment (make-vect 0 0) (make-vect 0.5 0.5))
                      (make-segment (make-vect 0.5 0.5) (make-vect 1 1))))
 a-frame)

((segments->painter (list
                      (make-segment (make-vect 0 0) (make-vect 0.5 0.5))
                      (make-segment (make-vect 0.5 0.5) (make-vect 1 1))))
 a-frame)

;;2.49
;;a
(define outline (list
                    (make-segment (make-vect 0 0) (make-vect 1 0))
                    (make-segment (make-vect 1 0) (make-vect 1 1))
                    (make-segment (make-vect 1 1) (make-vect 0 1))
                    (make-segment (make-vect 0 1) (make-vect 0 0))))

;;b
(define cross (list
                    (make-segment (make-vect 0 0) (make-vect 1 1))
                    (make-segment (make-vect 0 1) (make-vect 1 0))))

;;c
(define diamond (list
                    (make-segment (make-vect 0.5 0) (make-vect 1 0.5))
                    (make-segment (make-vect 1 0.5) (make-vect 0.5 1))
                    (make-segment (make-vect 0.5 1) (make-vect 0 0.5))
                    (make-segment (make-vect 0 0.5) (make-vect 0.5 0))))

((segments->painter outline) a-frame)
((segments->painter cross) a-frame)
((segments->painter diamond) a-frame)



((corner-split (segments->painter outline) 3) a-frame)
