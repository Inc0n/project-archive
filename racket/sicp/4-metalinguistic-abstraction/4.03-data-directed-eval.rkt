
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (eval (first-exp exps) env))
        (else
         (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable!
    (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

;; end
(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error "Unknown procedure type: APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (install-rectangular-package) ;; internal procedures
  ;; interface to the rest of the system
  (define (self-evaluating? exp)
    (cond ((number? exp) true)
          ((string? exp) true)
          (else false)))
  (define (expr-type exp)
    (cond ((self-evaluating? exp) 'self-evaluating)
          ((variable? exp) 'variable)
          ((expression? exp) (expr-tag exp))
          (else false)))
  (define (eval exp env)
    (let ((special? (expr-type exp)))
      (cond (special?
             ((get 'special special?) (expr-content expr) env))
            ((application? exp)
             ((get 'eval 'application) exp env))
            (else
             (error "Unknown expression type: EVAL" exp)))))
  (put 'eval 'eval eval)
  ;;
  (put 'special 'self-evaluating (lambda (exp env) exp))
  (put 'special 'variable lookup-variable-value)
  (put 'special 'quote
       (lambda (exp env) (text-of-quotation exp)))
  (put 'special 'set! eval-assignment)
  (put 'special 'define eval-definition)
  (put 'special 'if eval-if)
  (put 'special 'lambda
       (lambda (exp env)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env)))
  (put 'special 'begin eval-sequence)
  (put 'special 'cond
       (lambda (exp env)
         (eval (cond->if exp) env)))
  (put 'eval 'application
       (lambda (exp env)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env))))
  'done)

;;;;

(define (install-rectangular-package) ;; internal procedures
  ;; interface to the rest of the system
  (define (self-evaluating? exp)
    (cond ((number? exp) true)
          ((string? exp) true)
          (else false)))
  (define (eval exp env)
   (cond ((self-evaluating? exp) exp)
         ((variable? exp) (lookup-variable-value exp env))
         ((get 'special (car exp)) =>
          (lambda (eval-rule) (eval-rule exp env)))
         ((application? exp)
          (apply (eval (operator exp) env)
                 (list-of-values (operands exp) env)))
         (else
          (error "Unknown expression type -- EVAL" exp))))
  (put 'eval 'eval eval)
  ;;
  (put 'special 'self-evaluating (lambda (exp env) exp))
  (put 'special 'variable lookup-variable-value)
  (put 'special 'quote text-of-quotation)
  (put 'special 'set! eval-assignment)
  (put 'special 'define eval-definition)
  (put 'special 'if eval-if)
  (put 'special 'lambda
       (lambda (exp env)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env)))
  (put 'special 'begin
       (lambda (exp env)
         (eval-sequence (begin-actions exp) env)))
  (put 'special 'cond
       (lambda (exp env)
         (eval (cond->if exp) env)))
  'done)