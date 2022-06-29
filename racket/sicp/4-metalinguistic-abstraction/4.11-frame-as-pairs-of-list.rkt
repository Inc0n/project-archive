
(define (make-binding var val) (cons var val))
(define (binding-var bind) (car bind))
(define (binding-value bind) (cdr bind))

(define (make-frame variables values)
  (map make-binding variables values))

(define (frame-variables frame) (map binding-var frame))
(define (frame-values frame) (map binding-value frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons (make-binding var val) frame)))


;; exercise 4.12

(define (scan var bindings)
  (cond ((null? vars) false)
        ((eq? var (binding-var (car bindings)))
         (car bindings))
        (else (scan (cdr bindings)))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((binding (scan var (first-frame env))))
          (if binding
              (binding-value (car bindings))
              (env-loop (enclosing-environment env))))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (let ((binding (scan var frame)))
      (if binding
          (set-cdr! binding val)
          (add-binding-to-frame! (make-binding var val)
                                 frame)))))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Cannot set variable before its definition" var)
        (let ((binding (scan var (first-frame env))))
          (if binding
              (set-cdr! binding val)
              (env-loop (enclosing-environment env))))))
  (env-loop env))
