
((lambda (n)
   ((lambda (fib) (fib fib 0 1 n))
      (lambda (fib a b k)
        (if (= k 1)
            a
            (fib fib b (+ a b) (- k 1))))))
 4)

(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0)
         true
         (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0)
         false
         (ev? ev? od? (- n 1))))))