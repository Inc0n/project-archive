
(define (my-for-each f lst)
  (define (iter lst)
    (cond ((null? lst) t)
          (else
           (printf "~a~%" (f (car lst)))
           (iter (cdr lst)))))
  (iter lst '()))

