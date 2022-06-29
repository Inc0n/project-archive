
(define (let*? exp) (tagged-list? exp 'let*))
(define (let*-bindings exp) (cadr exp))
(define (let*-clauses exp) (cddr exp))

(define (let*->nested-let exp)
  (define (expand-clauses bindings clauses)
    (if (null? bindings)
        clauses
        `(let (,(first bindings))
           ,(expand-clauses (rest bindings) clauses))))
  (expand-clauses (let-bindings exp) (let-clauses exp)))

