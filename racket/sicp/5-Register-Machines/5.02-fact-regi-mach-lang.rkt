(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

;; (define (gcd a b)
;;   (if (= b 0)
;;       a
;;       (gcd b (remainder a b))))

;; (controller
;;  test-b
;;  (test (op =) (reg b) (const 0))
;;  (branch (label gcd-done))
;;  (assign t (op rem) (reg a) (reg b))
;;  (assign a (reg b))
;;  (assign b (reg t))
;;  (goto (label test-b))
;;  gcd-done)

(controller
 factorial
 ;; factorial
 ;; reg n
 (controller
  ;; iter
  ;; reg product
  ;; reg counter
  iter
  (test (op >) (reg counter) (reg n))
  (branch (label iter-done))
  (assign t (op +) (reg counter) (const 1))
  (assign product (op *) (reg counter) (reg product))
  (assign counter (reg t))
  (goto (label iter))
  iter-done))