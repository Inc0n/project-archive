#|| Exercise-2.33-list-operators ||#

(require "../2-Building-Abstractions-with-Data/2.2.3-sequence-interface.rkt")

(define nil '())

(define (my-map p sequence)
  (accumulate (lambda (x acc)
                (cons (p x) acc))
              nil
              sequence))
(my-map even? '(1 2 3 4))

(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))
(my-append '(1 2) '(3 4))

(define (my-length sequence)
  (accumulate (lambda (x acc)
                (+ acc 1))
              0 sequence))
(my-length '(1 2 3 4))

#|| 2019-05-08 17:09 ||#