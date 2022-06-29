#lang racket

(provide (all-defined-out))

(require (only-in "data.rkt"
                  tagged-list?)
         rnrs/mutable-pairs-6)

(define (true? x)  (not (eq? x false)))
(define (false? x) (eq? x false))

(define (make-procedure parameters body env)
  ;; `(procudure ,parameters ,body ,env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-binding var val) (cons var val))
(define (binding-var bind) (car bind))
(define (binding-value bind) (cdr bind))

(define (make-frame variables values)
  (map make-binding variables values))
(define (frame-variables frame) (map binding-var frame))
(define (frame-values frame) (map binding-value frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons (make-binding var val) frame)))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (scan var bindings)
  (cond ((null? bindings) false)
        ((eq? var (binding-var (first bindings)))
         (car bindings))
        (else (scan (rest bindings)))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((binding (scan var (first-frame env))))
          (if binding
              (binding-value binding)
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
