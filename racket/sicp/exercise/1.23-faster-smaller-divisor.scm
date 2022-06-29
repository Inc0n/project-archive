
#lang racket

#|
The smallest-divisor procedure shown at the start of this section does ;
lots of needless testing: After it checks to see if the number is ;
divisible by 2 there is no point in checking to see if it is divisible ;
by any larger even numbers. This suggests that the values used for ;
test-divisor should not be 2, 3, 4, 5, 6, but rather 2, 3, 5, 7, 9 ;
|#

(provide search-for-primes next-num prime?)

(define (square n)
  (* n n))

(define (next-test-num n)
  (if (even? n)
      (+ n 1)
      (+ n 2)))

(define (no-remainder? a b)
  (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n)
         n)
        ((no-remainder? test-divisor n)
         test-divisor)
        (else
         ;; new bit
         (find-divisor n (next-test-num test-divisor)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (search-for-primes)
  (define (first-prime n)
    (if (prime? n)
        n
        (first-prime (+ n 1))))
  (let ((find-prime
         (lambda (n)
           (let ((start-time (current-inexact-milliseconds)))
             (printf "~a *** ~a~%"
                     (first-prime n)
                     (- (current-inexact-milliseconds)
                        start-time)))))
        (start-time (current-inexact-milliseconds)))
    (find-prime 1000)
    (find-prime 10000)
    (find-prime 1000000)
    (printf "finished *** ~a~%" (- (current-inexact-milliseconds)
                                   start-time))))

#|
racket@> (search-for-primes)
1009 *** 0.01318359375
10007 *** 0.014892578125
1000003 *** 0.10400390625
finished *** 0.231201171875
racket@> (search-for-primes)
1009 *** 0.01318359375
10007 *** 0.01806640625
1000003 *** 0.1279296875
finished *** 0.277099609375
racket@> (search-for-primes)
1009 *** 0.013916015625
10007 *** 0.01904296875
1000003 *** 0.111083984375
finished *** 0.263916015625
racket@> (search-for-primes)
1009 *** 0.012939453125
10007 *** 0.01806640625
1000003 *** 0.10009765625
finished *** 0.239013671875
racket@> (search-for-primes)
1009 *** 0.013916015625
10007 *** 0.01806640625
1000003 *** 0.10107421875
finished *** 0.245849609375
racket@> (search-for-primes)
1009 *** 0.010986328125
10007 *** 0.012939453125
1000003 *** 0.070068359375
finished *** 0.169921875
racket@> (search-for-primes)
1009 *** 0.01416015625
10007 *** 0.086181640625
1000003 *** 0.1240234375
finished *** 0.359130859375
racket@> (search-for-primes)
1009 *** 0.013916015625
10007 *** 0.02978515625
1000003 *** 0.110107421875
finished *** 0.278076171875
racket@> (search-for-primes)
1009 *** 0.01318359375
10007 *** 0.01806640625
1000003 *** 0.10107421875
finished *** 0.242919921875
racket@> (search-for-primes)
1009 *** 0.012939453125
10007 *** 0.015869140625
1000003 *** 0.114013671875
finished *** 0.244140625
|#