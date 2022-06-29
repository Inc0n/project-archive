
;; Exercise 3.9
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

;; global  ________________________
;; env     | other var.            |
;; ------->| factorial : *         |
;;         |             |         |
;;         |_____________|_________|
;;                       |     ^
;;                       |     |
;;                 variables : n
;;                 body: (if (= n 1) 1 (* n (factorial (- n 1))))
;; (factorial 6)
;;
;; [factorial: n = 6
;;   [factorial: n = 5
;;     [factorial: n = 4
;;       [factorial: n = 3
;;         [factorial: n = 2
;;           [factorial: n = 1]]]]]]

;; and an iterative version
(define (factorial n)
  (define (fact-iter product counter max-count)
    (if (> counter max-count)
        product
        (fact-iter (* counter product)
                   (+ counter 1)
                   max-count)))
  (fact-iter 1 1 n))
(factorial 6)

;; global  ___________________________________
;; env     | other var.                       |
;; ------->| factorial : *                    |
;;         | fact-iter : |               *    |
;;         |_____________|_______________|____|
;;                       |       ^       |  ^
;;                       |       |       |  |
;;                       |       |       variable : (product counter max-count)
;;                       |       |       body: (if (> counter max-count)
;;                       |       |                 prod
;;                       |       |                 (fact-iter (* counter product)
;;                       |       |                            (+ counter 1)
;;                       |       |                            max-count))
;;                       |       |
;;                 variable: n
;;                 body: (fact-iter 1 1 n)

;; [factorial: n = 6
;;         [fact-iter: product = 1
;;                     counter = 1
;;                     max-count = 6
;;                 [fact-iter: product = 1
;;                             counter = 2
;;                             max-count = 6
;;                         [fact-iter: product = 2
;;                                     counter = 3
;;                                     max-count = 6
;;                                 [fact-iter: product = 6
;;                                             counter = 4
;;                                             max-count = 6]]]]]

(define (make-withdraw initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
       "Insufficient funds"))))


;; global  ___________________________________
;; env     | other var.                       |
;; ------->| make-withdraw : *                |
;;         |                 |                |
;;         |_________________|________________|
;;                           |   ^
;;                           variable: initial-amount
;;                           body: <lambda: balance>
(define W1 (make-withdraw 100))
;;          _______________________
;; global->| make-withdraw : *     |
;; env.    | W1 :  *         |     |
;;          -------|---^-----|---^-
;;                 |   |     |   |
;;                 |   |     parameter: initial-mount
;;                 |   |     body: ((lambda (balance) ((...)))
;;                 |   |                    initial-mount)
;;                 |   |
;;                 |  _|___Frame_A__________
;;                 | | initial-mount : 100  |<- E0
;;                 |  -^--------------------
;;                 |   |
;;                 |  _|___Frame_B_____________
;;                 | | balance : initial-mount | <- E1
;;                 |  -^-----------------------
;;                 |   |
;;                 parameter: amount
;;                 body: (if (>= balance amount) ... )
(W1 50);;                     ^
;;         E2 -> balance: 50  |
(define W2 (make-withdraw 100))