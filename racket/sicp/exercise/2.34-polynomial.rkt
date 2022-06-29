#|| Exercise-2-34-polynomial ||#

(require "../2-Building-Abstractions-with-Data/2.2.3-sequence-interface.rkt")

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ (* higher-terms x)
                   this-coeff))
              0
              coefficient-sequence))

;; For example,
;; to compute 1 + 3x + 5x^3 + x^5 at x = 2 you would evaluate
(horner-eval 2 (list 1 3 0 5 0 1))

#|| 2019-05-08 17:12 ||#