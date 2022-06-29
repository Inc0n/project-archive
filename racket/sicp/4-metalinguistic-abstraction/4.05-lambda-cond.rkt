
(define (make-lambda-if pred then-proc alt)
  `(let ((prev ,pred))
     (if prev
         (,then-proc prev)
         ,alt)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                            ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last: COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

;; (cond ((assoc 'b '((a 1) (b 2))) => cadr)
;;       (else false))

(define (cond-actions clause)
   (if (eq? '=> (cadr clause))
     (list (caddr clause) (cond-predicate clause))
     (cdr clause)))