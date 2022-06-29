#lang racket

(provide filter accumulate enumerate-interval enumerate-tree)

(define nil '())

(define (filter predicate sequence)
  (cond ((null? sequence)
         nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate
                       (cdr sequence))))
        (else
         (filter predicate
                 (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial
                      (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low
            (enumerate-interval (+ low 1)
                                high))))

(define (enumerate-tree tree)
  (cond ((null? tree)
         nil)
        ((not (pair? tree))
         (list tree))
        (else
         (append (enumerate-tree (car tree))
                 (enumerate-tree (cdr tree))))))

;; examples

(define (sum-odd-squares tree)
  (define (square x) (* x x))
  (accumulate
   + 0
   (map square
        (filter odd? (enumerate-tree tree)))))

;; (define (even-fibs n)
;;   (accumulate
;;    cons
;;    nil
;;    (filter even?
;;            (map fib (enumerate-interval 0 n)))))

;; (define (list-fib-squares n)
;;   (define (square x) (* x x))
;;   (accumulate
;;    cons
;;    nil
;;    (map square
;;         (map fib
;;              (enumerate-interval 0 n)))))

;; (list-fib-squares 10)
;; (0 1 1 4 9 25 64 169 441 1156 302