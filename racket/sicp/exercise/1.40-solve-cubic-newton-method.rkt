#| Define a procedure cubic such that
 to be used like (newtons-method (cubic a b c) 1)
to approximate zeros of the cubic x^3 + ax^2 + bx + c;
|#

(define (square x)
  (* x x))
(define (cube x)
  (* x x x))
(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

;;(newtons-method (cubic a b c) 1)