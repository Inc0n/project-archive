
(require "2.38-fold-left-right.rkt")

(define nil '())

(define (reverse sequence)
  (fold-right (lambda (x acc)
                (cons x acc))
              nil
              sequence))

(define (reverse sequence)
  (fold-left (lambda (acc x)
               (append acc (list x)))
             nil
             sequence))