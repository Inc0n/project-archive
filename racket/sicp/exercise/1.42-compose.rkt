#|
Let f and g be two one-argument functions. The composition f after g
is defined to be the function x -> f(g(x)).
|#

(define (compose f g)
  (lambda (x) (f (g x))))

(module test racket
  (require rackunit)
  (define (square x)
    (* x x))
  (define (inc x)
    (+ x 1))
  ;; (define (compose-test f g x)
  ;;   ((compose f g) x))
  (check-= ((compose square inc) 6) 49 49))