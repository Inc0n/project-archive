
;; (accumulate combiner null-value term a next b)
(define (accumulate-iter combiner null-value term a next b)
  (define (iter a acc)
    (if (> a b)
        acc
        (iter (next a) (combiner acc (term a)))))
  (iter a null-value))

(define (accumulate-rec combiner null-value term a next b)
  (define (rec a)
    (if (> a b)
        null-value
        (combiner (term a) (rec (next a)))))
  (rec a))

(define (accumulate combiner null-value term next)
  (define (iter a b acc)
    (if (> a b)
        acc
        (iter (next a) b (combiner acc (term a)))))
  (lambda (a b)
    (iter a b null-value)))