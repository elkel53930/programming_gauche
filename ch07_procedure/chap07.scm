(define (length lis)
  (fold (lambda (a b) (+ b 1)) 0 lis))

(define (max-number lis)
  (if (null? lis)
    (error "max-number needs at least one number")
    (fold (lambda (a b) (if (> a b) a b)) (car lis) (cdr lis))))

(define (print-element lis)
  (fold (lambda (a b) (print a)) #f lis))

(define (tree-walk walker proc tree)
  (walker (lambda (elt)
            (if (list? elt)
              (tree-walk walker proc elt)
              (proc elt)))
          tree))

(define (reverse-for-each proc lis)
  (for-each proc (reverse lis)))
