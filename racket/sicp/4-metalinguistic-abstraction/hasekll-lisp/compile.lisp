
(load '("env.scm" "data.scm")

(defun eval (exp env) ((analyze exp) env))

(defun analyze (exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((application? exp) (analyze-application exp))
        (else (error "Unknown expression type: ANALYZE" exp))))

(defun analyze-self-evaluating (exp)
  (lambda (env) exp))

(defun analyze-quoted (exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

(defun analyze-variable (exp)
  (lambda (env) (lookup-variable-value exp env)))

(defun analyze-assignment (exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))

(defun analyze-definition (exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

(defun analyze-if (exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env) (if (true? (pproc env))
                      (cproc env)
                      (aproc env)))))

(defun analyze-lambda (exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env) (make-procedure vars bproc env))))

(defun analyze-sequence (exps)
  (labels
      ((sequentially (proc1 proc2)
         (lambda (env) (proc1 env) (proc2 env)))
       (loop-all (first-proc rest-procs)
          (if (null? rest-procs)
              first-proc
              (loop-all
                 (sequentially first-proc (car rest-procs))
                 (cdr rest-procs)))))
    (let ((procs (map analyze exps)))
      (if (null? procs)
          (error "Empty sequence: ANALYZE")
          false)
      (loop-all (car procs) (cdr procs)))))

(defun analyze-application (exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application
       (fproc env)
       (map (lambda (aproc) (aproc env))
            aprocs)))))

(defun execute-application (proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment
           (procedure-parameters proc)
           args
           (procedure-environment proc))))
        (else
         (error nil
                "Unknown procedure type: EXECUTE-APPLICATION"
                proc))))

(defun analyze (exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
        ((variable? exp) (analyze-variable exp))
        ((get 'special (car exp)) =>
         (lambda (eval-rule) (eval-rule exp env)))
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type: ANALYZE" exp))))

;; (put 'special 'self-evaluating analyze-self-evaluating)
;; (put 'special 'variable analyze-variable)
(put 'special 'quote analyze-quoted)
(put 'special 'set! analyze-assignment)
(put 'special 'define analyze-definition)
(put 'special 'if analyze-if)
(put 'special 'lambda analyze-lambda)
(put 'special 'begin
     (lambda (exp)
       (analyze-sequence (begin-actions exp))))
(put 'special 'cond
     (lambda (exp) (analyze (cond->if exp))))