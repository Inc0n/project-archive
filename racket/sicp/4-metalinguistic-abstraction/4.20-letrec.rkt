(define (f x)
  (letrec ((even? (lambda (n)
                    (if (= n 0)
                        true
                        (odd? (- n 1)))))
           (odd? (lambda (n)
                   (if (= n 0)
                       false
                       (even? (- n 1))))))
    ⟨rest of body of f⟩))


(define (letrec? exp) (tagged-list? exp 'letrec))
(define (letrec-bindings exp) (cadr exp))
(define (letrec-clauses exp) (cddr exp))

(define (letrec->let exp)
  (define (expand-clauses intern-defs clauses)
    ;; (cons `(lambda ,(map get-var bindings)
    ;;          ,clauses)
    ;;       (map get-value bindings))
    (define (get-var binding)
      (cond ((variable? binding) binding)
            ((pair? binding) (car binding))
            (else (error "invalid binding form: LET->LAMBDA"
                         binding))))
    (define (get-value binding)
      (cond ((pair? binding) (cadr binding))
            (else false)))
    (cons (make-lambda (map get-var bindings)
                       clauses)
          (map get-value bindings)))
  (expand-clauses (letrec-bindings exp) (letrec-clauses exp)))


(define (declare-variables intern-defs)
  (map (lambda (x) (list x '*unassigned*))
       intern-defs))

(define (set-variables intern-defs)
  (map (lambda (x) (list 'set! (car x) (cadr x)))
       intern-defs))

(define (letrec->let expr)
  (let ((intern-defs (letrec-bindings expr)))
    `(let ,(declare-variables (map car intern-defs))
       ,@(set-variables intern-defs)
       ,@(letrec-body expr))
    (cons (list 'let (declare-variables (map car intern-defs))
                (append (set-variables intern-defs)
                        (letrec-body expr))))))