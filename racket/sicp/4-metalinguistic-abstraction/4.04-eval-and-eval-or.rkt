
(define (eval-and exps env)
  (cond ((null? exps) 'true)
        ((last-exp? exps)
         (eval (first-expression exps) env))
        ((true? (eval (first-expression exps) env))
         (eval-and (rest-expressions exps) env))
        (else 'false)))

(define (eval-or exps env)
  (cond ((null? exps) 'false)
        ((true? (eval (first-expression exps) env)) 'true)
        (else
         (eval-or (rest-expressions exps) env))))

;; derived expressions

(define (expand-and exps)
  (if (null? exps)
      'x
      `(let ((x ,(first-exp exps)))
         (if x
             ,(expand-and (rest-exps exps))
             false))))

(define (expand-or exps)
  (if (null? exps)
      false
      `(let ((x ,(first-exp exps)))
         (if x
             x
             ,(expand-or (rest-exps exps))))))


(define (install-rectangular-package)
  ;; ...
  (put 'special 'and
       (lambda (exp env)
         (eval-and (operands exp) env)))
  (put 'special 'or
       (lambda (exp env)
         (eval-or (operands exp) env)))
  'done)