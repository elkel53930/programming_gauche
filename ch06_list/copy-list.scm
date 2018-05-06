(define (copy-list lis)
  (if (pair? lis)
    (cons (car lis) (copy-list (cdr lis)))
    lis))

(define (deep-copy-list lis)
  (if (pair? lis)
    (cons (deep-copy-list (car lis)) (deep-copy-list (cdr lis)))
    lis))
