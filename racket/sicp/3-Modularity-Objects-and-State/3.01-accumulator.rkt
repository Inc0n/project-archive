
;; Exercise 3.1
;; accumulator
;;
(define (make-accumulator num)
  (define (dispatch x)
    (set! num (+ x num))
    num)
  dispatch)

;; Exercise 3.2
;; monitor number of function calls
;;
(define (make-monitored proc)
  (let ((count 0))
    (define (dispatch arg)
      (case arg
        ('count count)
        (else
         (set! count (+ 1 count))
         (proc arg)))))
  dispatch)

;; Exercise 3.3
;; make-account with password
;;
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
  (define (dispatch p m)
    (if (eq? passwd p)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request: MAKE-ACCOUNT" m)))
        (error "Wrong password for this account" m))))

;; Exercise 3.4
;; make-account for 7-tries
;;
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
  (let ((tries 7))
    (define (dispatch p m)
      (if (eq? passwd p)
          (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                (else (error "Unknown request: MAKE-ACCOUNT" m)))
          (begin
            (set! tries (- tries 1))
            (if (= tries 0)
                (call-cop)
                '())
            (printf "Wrong password, try again~%"))))))