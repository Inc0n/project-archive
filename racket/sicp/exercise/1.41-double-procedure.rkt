#|
Define a procedure double that takes a procedure of one argument as ;
argument and returns a procedure that applies the original procedure
twice.
|#

(define (double f)
  (lambda (x) (f (f x))))

(define (inc x)
  (+ x 1))

(((double (double double)) inc) 5)
;; => 21 (16 + 5)

