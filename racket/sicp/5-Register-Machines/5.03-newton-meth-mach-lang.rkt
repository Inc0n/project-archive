(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess)
               x))
       0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(controller
 sqrt-iter

 (test (op good-enough?) (reg guess))

 (branch (label sqrt-iter-done))
 (assign guess (op improve) (reg guess))
 (goto (label sqrt-iter))
 sqrt-iter-done)

(controller
 sqrt-iter
 ;; reg guess
 good-enough?
 (assign t (op square) (reg guess))
 (assign t (op -) (reg t) (reg x))
 (assign t (op abs) (reg t))
 (test (op <) (reg t) (const 0.001))
 good-enough?-done

 (branch (label sqrt-iter-done))

 improve
 (assign t (op /) (reg x) (reg guess))
 (assign guess (op average) (reg guess) (reg t))
 improve-done

 (goto (label sqrt-iter))
 sqrt-iter-done)
 ;; improve
 ;; reg guess
