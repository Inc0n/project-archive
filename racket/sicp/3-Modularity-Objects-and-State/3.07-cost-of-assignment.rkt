
;; Exercise 3.7

(define (passwd-protect passwd proc)
  (let ((num-attemp 0))
    (lambda (passwd-attempt msg)
      (if (eq? passwd-attempt passwd)
          (begin (set! num-attemp 0)
                 (proc msg))
          (begin (set! num-attemp (+ num-attemp 1))
                 (if (= num-attemp 7)
                     (error "Wrong password for this account")))))))
(define (make-account balance passwd)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin
          (set! balance (- balance amount))
          balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: MAKE-ACCOUNT" m))))
  (passwd-protect passwd dispatch))

(define (make-joint-account account original-password password)
  (define (dispatch msg)
    (account original-password msg))
  (password-protect password dispatch))

;; Exercise 3.8
;; When we introduce assignment, the order in which the arguments to a
;; procedure are evaluated can make a difference to the result.
;; Define a simple procedure f such that evaluating
(+ (f 0) (f 1))
;; will return 0 if the arguments to + are evaluated from left to
;; right but will return 1 if the arguments are evaluated from
;; right to left.

(define f
  (let (num)
    (define (dispatch x)
      (if num
          num
          (set! num x)))
    dispatch))