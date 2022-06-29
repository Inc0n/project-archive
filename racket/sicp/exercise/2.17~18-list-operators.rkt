
(define (my-last lst)
  (let ((x (cdr lst)))
    (if (null? x)
        lst
        (last x))))

(define (my-reverse lst)
  (define (iter lst acc)
    (if (null? lst)
        acc
        (iter (cdr lst) (cons (car lst) acc))))
  (iter lst '()))