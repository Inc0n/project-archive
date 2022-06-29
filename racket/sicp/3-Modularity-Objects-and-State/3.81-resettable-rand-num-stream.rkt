
(define (rand-update x)
  (remainder (+ (* 13 x) 5) 24))

(define (random-numbers init request-stream)
  (define (aux request value)
    (cond ((eq? request 'next) (rand-update value))
          ((number? request) request)
          ;; signal error
          ))
  (define rand-nums
    (cons-stream
     init
     (stream-map aux request-stream rand-nums)))
  rand-nums)