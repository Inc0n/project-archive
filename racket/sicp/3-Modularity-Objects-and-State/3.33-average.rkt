(require "libconstraint.rkt")

(define (average a b c)
  (let ((u (make-connector))
        (x (make-connector)))
    (adder a b u)
    (multiplier x u c)
    (constant 2 x)
    'ok))

(define a (make-connector))
(define b (make-connector))
(define c (make-connector))

(probe "a" a)
(probe "b" b)
(probe "c" c)
(average a b c)