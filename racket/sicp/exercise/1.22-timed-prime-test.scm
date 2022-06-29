

(require "../1.2.6-example-prime-num-test.scm")

(define (search-for-primes)
  (define (first-prime n)
    (if (prime? n)
        n
        (first-prime (+ n 1))))
  (let ((find-prime
         (lambda (n) (let ((start-time (current-inexact-milliseconds)))
                       (printf "~a *** ~a~%" (first-prime n)
                               (-
                                (current-inexact-milliseconds)
                                start-time)))))
        (start-time (current-inexact-milliseconds)))
    (find-prime 1000)
    (find-prime 10000)
    (find-prime 1000000)
    (printf "finished *** ~a~%" (- (current-inexact-milliseconds)
                                   start-time))))
#||

racket@> (search-for-primes)
1009 *** 0.014892578125
10007 *** 0.01904296875
1000003 *** 0.100830078125
finished *** 0.447998046875

racket@> (search-for-primes)
1009 *** 0.012939453125
10007 *** 0.018798828125
1000003 *** 0.10400390625
finished *** 0.34912109375

racket@> (search-for-primes)
1009 *** 0.013916015625
10007 *** 0.01904296875
1000003 *** 0.10009765625
finished *** 0.2470703125

racket@> (search-for-primes)
1009 *** 0.010009765625
10007 *** 0.01318359375
1000003 *** 0.06689453125
finished *** 0.1650390625

racket@> (search-for-primes)
1009 *** 0.013916015625
10007 *** 0.01806640625
1000003 *** 0.10009765625
finished *** 0.24609375

racket@> (search-for-primes)
1009 *** 0.0078125
10007 *** 0.009033203125
1000003 *** 0.052978515625
finished *** 0.1318359375

racket@> (search-for-primes)
1009 *** 0.01318359375
10007 *** 0.01904296875
1000003 *** 0.10009765625
finished *** 0.244140625

racket@> (search-for-primes)
1009 *** 0.013916015625
10007 *** 0.017822265625
1000003 *** 0.099853515625
finished *** 0.245849609375

||#