;; for example,(adder a b c) specifies that the quantities a, b, and c
;; must be related by the equation a + b = c, (multiplier x y z) expresses
;; the constraint xy = z, and (constant 3.14 x) says that the value of x
;; must be 3.14

(require "libconstraint.rkt")

(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

(define C (make-connector))
(define F (make-connector))

(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)

(celsius-fahrenheit-converter C F)
