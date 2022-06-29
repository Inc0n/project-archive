#|

A function f is defined by the rule that
f (n) = n                                if n < 3
        f(n - 1) + 2f(n - 2) + 3(fn - 3) if n >= 3

Write a procedure that computes f by means of a recursive process.
Write a procedure that computes f by means of an iterative process.

|#

(define (f-rec n)
  (if (< n 3)
      n
      (+ (f-rec (- n 1))
         (* 2 (f-rec (- n 2)))
         (* 3 (f-rec (- n 3))))))

(define (f-iter n)
  (define (aux a b c n)
    (if (< n 1)
        a
        (aux b c (+ c
                    (* 2 b)
                    (* 3 a))
             (- n 1))))
  (aux 1 2 3 (- n 1)))
