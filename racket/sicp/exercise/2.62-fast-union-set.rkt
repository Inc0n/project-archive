
(require "../2-Building-Abstractions-with-Data/2.3.3-example-representing-sets.rkt")

(define (union-set set1 set2)
  (define (set-cons set1 lst) (cons (car set1) acc))
  (define (iter set1 set2 acc)
    (cond ((null? set1) (append (reverse acc) set2))
          ((null? set2) (append (reverse acc) set1))
          ((= (car set1) (car set2))
           (iter (cdr set1) (cdr set2) (set-cons set1 acc)))
          ((< (car set1) (car set2))
           (iter (cdr set1) set2 (set-cons set1 acc)))
          (else (iter set1 (cdr set2) (set-cons set2 acc)))))
  (iter set1 set2 '()))