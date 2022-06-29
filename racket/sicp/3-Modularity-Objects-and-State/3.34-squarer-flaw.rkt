
(define (squarer a b)
  (multiplier a a b))

;; when b has been set a value from user, the system could not determine
;; the value of a, because it will try to do b / a => to get a,
;; which the value of a is never set, so it could not be found.