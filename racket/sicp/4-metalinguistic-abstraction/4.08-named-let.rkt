
(define (named-let? exp)
  (and (let? exp) (symbol? (second exp))))
(define (let-named-name exp) (cadr exp))
;; (define (named-let-vars exp) (caddr exp))
;; (define (named-let-clauses exp) (cdddr exp))

(define (let->lambda exp)
  (let ((name (let-named-name exp))
        (expanded-let (let->lambda exp)))
    (let ((procedure (car expanded-let))
          (init-values (cdr expanded-let)))
      `(let ((,name ,procedure))
         (,name . ,init-values)))))
