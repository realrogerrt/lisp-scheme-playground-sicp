(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)))

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree
                      (make-leaf 'D 1)
                      (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(display sample-tree)


;;2.68
(define (encode-symbol symbol tree)
  (define (walk-tree path node)
    (if (leaf? node) (if (eq? (symbol-leaf node) symbol) path (error "SYMBOL " symbol " NOT FOUND" ))
          (if (memq symbol (symbols node)) 
            (if (memq symbol (symbols (left-branch node)))
              (walk-tree (append path '(0)) (left-branch node))
              (walk-tree (append path '(1)) (right-branch node)))
            (error "SYMBOL " symbol " NOT FOUND"))))
  (walk-tree '() tree))

(define (encode message tree)
  (if (null? message)
    '()
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

(encode-symbol 'D sample-tree)
