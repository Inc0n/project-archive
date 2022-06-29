
(require "../2-Building-Abstractions-with-Data/2.3.3-example-representing-sets.rkt")

(define (union-set set1 set2)
  (if (null? set2)
      set1
      (if (element-of-set? set1 (car set2))
          (union-set set1 (cdr set2))
          (cons (car set2)
                (union-set set1 (cdr set2))))))