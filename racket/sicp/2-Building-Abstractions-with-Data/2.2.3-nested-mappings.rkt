
#lang racket

(require "2.2.3-sequence-interface.rkt"
         "../1-Building-Abstractions-with-Procedure/1.2.6-example-prime-num-test.rkt")

(provide flatmap)

(define nil '())

;; (accumulate
;;  append nil (map (lambda (i)
;;                    (map (lambda (j) (list i j))
;;                         (enumerate-interval 1 (- i 1))))
;;                  (enumerate-interval 1 n)))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

;; Combining all these steps yields the complete procedure
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap (lambda (i)
                          (map (lambda (j)
                                 (list i j))
                               (enumerate-interval 1 (- i 1))))
                        (enumerate-interval 1 n)))))

(define (permutations s)
  (if (null? s) ; empty set?
      (list nil) ; sequence containing empty set
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))