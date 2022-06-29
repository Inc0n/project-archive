
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2))
         '())
        ((eq? (car set1) (car set2))
         (cons (car set1) (intersection-set (cdr set1) (cdr set2))))
        (else (intersection-set (cdr set1) set2))))



;; How does the efficiency of each compare with the corresponding
;; procedure for the non-duplicate representation?

;; It's much faster, because there are no need to call element-of-set?
;; Operations are now more "mindless", carried out with less restrictions