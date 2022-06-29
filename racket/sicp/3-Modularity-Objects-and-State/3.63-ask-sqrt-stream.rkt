
;; way number 1
(define (sqrt-stream x)
  (cons-stream 1.0 (stream-map
                    (lambda (guess)
                      (sqrt-improve guess x))
                    (sqrt-stream x))))

;; way number 2
(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0 (stream-map
                      (lambda (guess)
                        (sqrt-improve guess x))
                      guesses)))
  guesses)

first way would not becase each recursive call to sqrt-improve
resets the approximation
