
(define (average a b)
  (/ (+ a b) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (stream-cons 1.0
                 (stream-map
                  (lambda (guess)
                    (sqrt-improve guess x))
                  guesses)))
  guesses)

(define (stream-limit s tolerence)
  (define (aux prev s)
    (let ((next (stream-first s)))
      (if (< (abs (- next prev)) tolerence)
          next
          (aux next (stream-rest s)))))
  (aux (stream-first s) (stream-rest s)))

(define (square-root x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(define (is-square x)
  (let ((root (square-root x 0.00000001)))
    (= (floor root) root)))

(define-syntax time-it
  (syntax-rules ()
    ((time-it a)
     (let ((start-time (current-inexact-milliseconds)))
       (let ((x a))
         (values x
                 (- (current-inexact-milliseconds) start-time)))))))