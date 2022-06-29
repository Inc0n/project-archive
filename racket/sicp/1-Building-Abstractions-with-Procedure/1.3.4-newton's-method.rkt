
(define (deriv g)
  (define dx 0.00001)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

((deriv (lambda (x) (* x x x))) 5)
;;=> 75.00014999664018
#|
y = x - g(x) / Dg(x)
: Dg(x) - the derivative of g(x)
|#
(define (newton-transform g)
  (lambda (x) (- x
                 (/ (g x)
                    ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt x)
  (newtons-method
   (lambda (y) (- (square y)
                  x))
   1.0))