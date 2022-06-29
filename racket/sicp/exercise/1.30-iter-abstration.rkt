(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (sum term a next b)
  (define (iter a acc)
    (if (> a b)
        acc
        (iter (next a) (+ acc (term a)))))
  (iter a 0)

(define (inc n)
  (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))

(sum-cubes 1 10)
