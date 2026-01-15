;;2.29
(define (make-mobile left right)
  (cons left right)
  )

(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cdr mobile))

(define (make-branch length structure)
  (cons length structure)
  )

(define (branch-length b) (car b))
(define (branch-structure b) (cdr b))

(define (branch-total-weight b)
  (let ((current-structure (branch-structure b)))
    (if (pair? current-structure)
      (total-weight current-structure)
      current-structure
      )
    )
  )

(define (total-weight m)
  (+ (branch-total-weight (left-branch m)) (branch-total-weight (right-branch m)))
  )

(define (torque b) (* (branch-length b) (branch-total-weight b)))

(define (is-balanced m)
  (if (pair? m) 
    (and
      (= (torque (left-branch m)) (torque (right-branch m)))
      (is-balanced (branch-structure (left-branch m)))
      (is-balanced (branch-structure (right-branch m)))
      )
    #t
    )
  )
;(define my-mobile (make-mobile (make-branch 2 (make-mobile (make-branch 4 6)) (make-branch 5

(define six (make-branch 6 8))
(define seven (make-branch 7 11))
(define eight (make-branch 8 10))
(define nine (make-branch 9 12))
(define four (make-branch 4 6))
(define five (make-branch 5 (make-mobile six seven)))
(define two (make-branch 2 (make-mobile four five)))
(define three (make-branch 3 (make-mobile eight nine)))
(define mobile-one (make-mobile two three))

(define balanced-mobile-one (make-mobile (make-branch 4 5) (make-branch 5 4)))
(define balanced-mobile-two (make-mobile (make-branch 4 5) (make-branch 5
                                                                        (make-mobile (make-branch 2 2) (make-branch 2 2)))))


(display mobile-one)
(total-weight mobile-one)

(is-balanced mobile-one)

(display balanced-mobile-one)
(is-balanced balanced-mobile-one)

(display balanced-mobile-two)
(is-balanced balanced-mobile-two)
(is-balanced (branch-structure (right-branch balanced-mobile-two)))
(total-weight (branch-structure (right-branch balanced-mobile-two)))


(define (map proc items)
  (cond ((null? items) items)
        ((pair? items) (cons (map proc (car items)) (map proc (cdr items))))
        (else (proc items))
        )
  ;(if (null? items)
  ;  items
  ;  (cons (proc (car items)) (map proc (cdr items)))
  ;)
  )
(define (map-list proc items)
  (if (null? items)
    items
    (cons (proc (car items)) (map-list proc (cdr items)))
    )
)

(map (lambda (x) (* x 2)) (list 1 2 3))

(define (tree-val node) (car node))
(define (tree-children node) (cdr node))
(define tree (list 1 (list 2 (list 3 5 6) 4)))

;;2.30
(define (square-tree node)
  (map (lambda (x) (* x x)) node)
  )

(square-tree tree)

(define tree-map map)
(define square (lambda (x) (* x x)))

;;2.31
(define (square-tree-hop tree) (tree-map square tree))

(square-tree-hop tree)

;;2.32
(define (subsets s)
  (if (null? s)
    (list '())
    (let ((first-item (car s))
          (rest (subsets (cdr s))))
      (append rest (map-list (lambda (i) (append i (list (car s)))) rest)))))


(subsets (list 3 4 5))
