
(load-option 'format)

(define (maptimes f n init)
  (define (aux n acc)
    (if (= n 0)
        acc
        (aux (- n 1) (f acc))))
  (aux n init))

(define (^ x power)
  (maptimes (lambda (i) (* i x)) power 1))

(define (my-a x y)
  (^ () y))

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

;; What are the values of the following expressions?
(A 1 10) ;; => 1024
(A 2 4)  ;; => 65536
(A 3 3)  ;; => 65536

;; Consider the following procedures, where A is the procedure defined above:

;; Give concise mathematical definitions for the functions computed by
;; the procedures f, g, and h for positive integer values of n.
;; For example, (k n) computes 5(n^2)

(define (f n)
  ;; => 2n
  (A 0 n))

(define (g n)
  ;; => 2^n
  (A 1 n))

(define (h n)
  ;; => 2^ (n^2)
  (A 2 n))

(define (k n)
  (* 5 n n))


