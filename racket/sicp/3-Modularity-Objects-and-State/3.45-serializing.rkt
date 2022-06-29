
(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request: MAKE-ACCOUNT" m))))
    dispatch))

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange))
     account1
     account2)))

;; why would serializing 'withdraw & 'deposit cause issues for exchanging?
Let User1 perform exchange with accounts a1 & a2, User2 perform exchange with a1 & a3, and their actions happened concurrently.
In any order of execution, the same difference would be calculated,
but for withdraw & deposit, if withdraw phase happened at the same time for both users, first withdraw will be successfull, while the second could fail if insufficient funds were left, because exchange is still executing, account2 isstill getting deposit even the withdraw from account1 failed.