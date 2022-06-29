;; This is a linear recursive process, which requires Θ(n) steps and Θ(n) space
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))


;; This version requires Θ(n) steps and Θ(1) space

(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                 (- counter 1)
                 (* b product))))
#||
We can compute exponentials in fewer steps by using successive squaring. For instance, rather than computing b^8 as:
  b * (b * (b * (b * (b * (b * (b * b))))))

we can compute it using three multiplications:

b^2 = b   * b
b^4 = b^2 * b^2
b^8 = b^4 * b^4
||#

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))