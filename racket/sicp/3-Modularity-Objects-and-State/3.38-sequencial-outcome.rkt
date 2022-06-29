
Peter: (set! balance (+ balance 10)) ; +10
Paul:  (set! balance (- balance 20)) ; -20
Mary:  (set! balance (- balance (/ balance 2))) ; -50%

;; sequential

peter -> paul -> mary

(* 0.5 (- balance 10))


peter -> mary -> paul

(- (* 0.5 (+ balance 10)) 20)


mary -> peter -> paul

(- (* 0.5 balance) 10)


;; concurrent
