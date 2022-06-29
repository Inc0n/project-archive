(define x (cons 1 2))
(define z (cons x x))
;;          _______________________
;; global->| make-withdraw : *     |
;; env.    | W1 :  *         |     |
;;          -------|---^-----|---^-
(set-car! (cdr z) 17)
(car x)
17

