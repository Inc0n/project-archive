#|
if f is a function and dx is some small number, then the smoothed version
of f is the function whose value at a point x is the average of f(x - dx), f(x), and f(x + dx)
|#

(require '1.43-repeated) ;; import repeated procedure

(define (smooth f)
  (define dx 0.00001)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3)))

(define (n-smooth f n) ((repeated smooth n) f))