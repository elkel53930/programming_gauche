(define (list? obj)
  (or (null? obj)
      (and (pair? obj)
           (list? (cdr obj)))))

(define (last-pair lis)
  (if (pair? (cdr lis))
      (last-pair (cdr lis))
      lis))

(define (length lis)
  (define (increment2 a b) (+ b 1))
  (fold increment2 0 lis))

(define (copy-list lis)
  (if (pair? lis)
      (cons (car lis) (copy-list (cdr lis)))
      lis))

(define (deep-copy-list lis)
  (if (pair? lis)
      (cons (deep-copy-list (car lis)) (deep-copy-list (cdr lis)))
      lis))

(define (append2 a b)
  (if (pair? a)
      (cons (car a) (append2 (cdr a) b))
      b))

(define (reverse lis)
  (if (null? (cdr lis))
      lis
      (append2 (reverse (cdr lis)) (list (car lis)))))

#|
(define (find pred lis)
  (if (null? lis)
      #f
      (if (pred (car lis))
          (car lis)
          (find pred (cdr lis)))))
|#

(define (find pred lis)
  (cond ((null? lis) #f)
        ((pred (car lis))  (car lis))
        (else (find pred (cdr lis)))))

(define (length2 lis)
  (if (null? lis)
      0
      (+ (length2 (cdr lis)) 1)))

(define (filter pred lis)
  (cond ((null? lis) ())
        ((pred (car lis)) (cons (car lis) (filter pred (cdr lis))))
        (else (filter pred (cdr lis)))))

(define (length_tr lis) ; tail recursion
  (define (length-rec lis n)
    (if (null? lis)
        n
        (length-rec (cdr lis) (+ n 1))))
  (length-rec lis 0))

(define (reverse_tr lis)
  (define (reverse_rec lis res)
    (if (null? lis)
        res
        (reverse_rec (cdr lis) (cons (car lis) res))))
  (reverse_rec lis '()))
