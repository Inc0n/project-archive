(define (integers-starting-from n)
  (stream-cons n
               (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))


(define (fib-stream a b)
  (cons-stream a
               (fib-stream b (+ a b))))
(define fib (fib-stream 0 1))

(define (divisible? x y)
  (= (remainder x y) 0))

(define (stream-car stream) (stream-first stream))
(define (stream-cdr stream) (stream-rest stream))

(define (sieve stream)
  (stream-cons
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))

(define ones (cons-stream 1 ones))
(define (add-streams s1 s2) (stream-map + s1 s2))

(define integers
  (stream-cons 1
               (add-streams ones integers)))
(define fibs
  (cons-stream
   0
   (cons-stream 1 (add-streams (stream-cdr fibs) fibs))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))

(define primes
  (cons-stream
   2
   (stream-filter prime? (integers-starting-from 3))))

(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) true)
          ((divisible? n (stream-car ps)) false)
          (else (iter (stream-cdr ps)))))
  (iter primes))

(define (mul-streams s1 s2)
  (stream-map (lambda (x y) (* x y)) s1 s2))

(define factorials
  (stream-cons 1 (mul-streams integers factorials)))