

;; Q:
;; a. Explain what was done above. Why can’t we assimilate the predicates
;; number? and variable? into the data-directed dispatch?

;; A:
;; because they are data only without tags of operator

(define (install-rectangular-package) ;; internal procedures
  (define (operator exp) (car exp))
  (define (operands exp) (cdr exp))
  ;; interface to the rest of the system
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2))
           (+ a1 a2))
          (else `(+ ,a1 ,a2))))
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2))
           (* m1 m2))
          (else `(* ,m1 ,m2))))
  (define (get-rest x op-sym)
    (let ((rest (cddr x)))
      (if (pair? rest)
          (cons op-sym (cddr x))
          1)))
  (define (deriv exp var)
    (cond ((number? exp) 0)
          ((variable? exp)
           (if (same-variable? exp var) 1 0))
          (else ((get 'deriv (operator exp))
                 (operands exp) var))))

  ;; end
  (define (addend x) (cadr x))
  (define (augend x) (get-rest x '+))
  (define (multiplier p) (cadr p))
  (define (multiplicand p) (get-rest p '*))
  (define (deriv-rules operator)
    (case operator
      ('+
       (lambda (expr var)
         (make-sum (deriv (addend expr) var)
                   (deriv (augend expr) var))))
      ('*
       (lambda (expr var)
         (make-sum (make-product (multiplier expr)
                                 (deriv (multiplicand expr) var))
                   (make-product (deriv (multiplier expr) var)
                                 (multiplicand expr)))))))
  (put 'deriv '(+ *) deriv-rules)
  'done)