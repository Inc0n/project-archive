
(require "3.5.1-streams.rkt")

(define (show x)
  (display x)
  (newline)
  x)

(define (stream-from-to low high)
  (if (> low high)
      '()
      (stream-cons low
                   (stream-from-to (+ low 1) high))))
(define x
  (stream-map show (stream-from-to 0 10)))

(stream-ref x 5)
(stream-ref x 7)
