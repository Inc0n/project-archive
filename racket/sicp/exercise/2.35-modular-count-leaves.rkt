#|| Exercise-2-35-modular-count-leaves ||#

(require "../2-Building-Abstractions-with-Data/2.2.3-sequence-interface.rkt")

(define (count-leaves t)
  (accumulate (lambda (x y)
                (if (pair? x)
                    (+ y (count-leaves x))
                    (+ y 1)))
              0
              t))

#|| 2019-05-08 18:05 ||#