
(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))

(define (* a b)
  (define (aux b acc)
    (if (= b 0)
        acc
        (aux (- b 1) (+ acc a))))
  (aux b 0))



(define (my-* b n)
  (cond ((= n 0) 0)
        ((even? n) (double (my-* b (/ n 2))))
        (else (+ b (my-* b (- n 1))))))

(define (double num)
  (+ num num))

(define (my-* a b)
  (define (aux b n acc)
    (if (= n 1)
        (+ acc b)
        (if (even? n)
            (aux (double b) (/ n 2) acc)
            (aux b (- n 1) (+ acc b)))))
  (aux a b 0))