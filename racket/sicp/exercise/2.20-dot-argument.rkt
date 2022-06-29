
(define (same-parity i . lst)
  (define operator?
    (if (even? i)
        even?
        odd?))
  (define (iter lst acc)
    (if (null? lst)
        (reverse acc)
        (let ((x (car lst)))
          (if (operator? x)
              (iter (cdr lst) (cons x acc))
              (iter (cdr lst) acc)))))
  (iter (cdr lst) (list i)))