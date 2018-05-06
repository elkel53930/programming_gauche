(define (list? obj)
  (or (null? obj)
      (and (pair? obj)
           (list? (cdr obj)))))
