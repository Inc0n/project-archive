

;; (define us-coins (list 50 25 10 5 1))
;; (define uk-coins (list 100 50 20 10 5 2 1 0.5))

;;;;;;;
;;;; GOAL
;; (cc 100 us-coins)
;; => 292
;;;; END
;;;;;;;;

(define (no-more? lst)
  (null? lst))
(define (except-first-denomination coins)
  (cdr coins))
(define (first-denomination coins)
  (car coins))

(define (cc amount coin-values)
  (cond
   ;; If there's no change left, we have a solution
   ((= amount 0) 1)
   ;; If we're gone -ve amount, or there are no more kinds of coins
   ;; to play with, we don't have a solution.
   ((or (< amount 0) (no-more? coin-values)) 0)
   (else
    ;; number of ways to make change without the current coin type
    ;; plus the number of ways after subtracting the amount of the
    ;; current coin.
    (+ (cc amount
           (except-first-denomination
            coin-values))
       (cc (- amount
              (first-denomination
               coin-values))
           coin-values)))))

(define (count-change amount coin-set)
  (define coins
    (case coin-set
      ('us '(50 25 10 5 1))
      ('uk '(100 50 20 10 5 2 1 0.5))
      (else (error "need us-coins or uk-coins"))))
  (cc amount coins))


#| Does the order of the list coinvalues affect the answer produced by cc ? Why or why not?
It would not.
The order of the list affects the sequence of solutions, ie, reversing the current coin sets, would result in the algorithm to look for solution from (amount - 1) instead of branching off with (amount - 50) (us coins)
|#