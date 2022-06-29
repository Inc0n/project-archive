
;; do, for, while, and until.
;; Express iterative processes in terms of ordinary procedure calls

;; example

(while 1
  (format "hello world"))

;; do is a special proc in racket, AND it would be just another
;; while procedure in my opinion, so skipped

(until false
  (format "hello world"))


(define (while? exp) (tagged-list? exp 'while))
(define (while-pred exp) (cadr exp))
(define (while-clauses exp) (cddr exp))

(define (while->lambda exp)
  `((lambda (inner-proc)
      (inner-proc true))
    (lambda (continue?)
      (if continue?
          (begin ,@(while-clauses exp)
                 (inner-proc ,(while-pred)))
          'done))))

