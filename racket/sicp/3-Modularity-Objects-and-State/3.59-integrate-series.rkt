

(define integers
  (stream-cons 1
               (stream-map (lambda (n) (+ n 1)) integers)))

(define (integrate-series s)
  (define series
    (stream-map (lambda (x) (/ 1 x)) integers))
  (mul-stream series s))

(define cosine-series
  (cons-stream 1 (integrate-series sine-series)))
(define sine-series
  (cons-stream 0 (stream-map -
                             (integrate-series cosine-series))))