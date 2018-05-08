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

(define (reverse-map proc lis)
  (map proc (reverse lis)))

(define (reversed walker)
  (lambda (proc lis)
    (walker proc (reverse lis))))

(use srfi-1)

(define (for-each-numbers proc lis)
  (for-each proc (filter number? lis)))

(define (map-numbers proc lis)
  (map proc (filter number? lis)))

(define (numbers-only walker)
  (lambda (proc lis) (walker proc (filter number? lis))))

; (filter number? lis)で入れ子になったリストがはじかれる
; number?だけでなくて、list?も条件に入れてみてはどうか？(lambda (lis) (or (number? lis) (list? lis)))

(define (numbers-only-for-tree walker)
  (lambda (proc lis) (walker proc (filter (lambda (lis) (or (number? lis) (list? lis))) lis))))

; 可変長引数
(define (func a b . c) (print "a=" a ", b=" b ", c=" c))

(define (append/log . args)
  (print "args=" args)
  (apply append args))

(define (make-logger func)
  (lambda args
    (print "args=" args)
    (apply func args)))

(define append/log_2 (make-logger append))
(define cons/log (make-logger cons))
(define fold/log (make-logger fold))

(define (append2 a b)
  (if (pair? a)
      (cons (car a) (append2 (cdr a) b))
      b))

(define (append . args)
  (cond [(null? args) '()]
        [(null? (cdr args)) (car args)]
        [else (append2 (car args) (apply append (cdr args)))]))
