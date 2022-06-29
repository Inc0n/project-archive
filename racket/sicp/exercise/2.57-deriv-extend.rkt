
#lang racket

(provide deriv)

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;; A sum is a list whose first element is the symbol +:

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

;; abstraction
(define (get-rest x op-sym)
  (let ((rest (cddr x)))
    (if (pair? rest)
        (cons op-sym (cddr x))
        1)))
;; end
(define (addend x) (cadr x))
(define (augend x) (get-rest x '+))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (get-rest p '*))

(define (exponential? x) (eq? (car x) '**))
(define (base x) (cadr x))
(define (exponent x) (caddr x))

(define (deriv expr var)
  (cond ((number? expr) 0)
        ((variable? expr)
         (if (same-variable? expr var) 1 0))
        ((pair? expr)
         (cond
          ((sum? expr)
           (make-sum (deriv (addend expr) var)
                     (deriv (augend expr) var)))
          ((product? expr)
           (make-sum (make-product (multiplier expr)
                                   (deriv (multiplicand expr) var))
                     (make-product (deriv (multiplier expr) var)
                                   (multiplicand expr))))
          ((exponential? expr)
           (make-product (make-product
                          (make-exponential (base expr)
                                            (- (exponent expr) 1))
                          (exponent expr))
                         (deriv (base expr) var)))))
        (else (error "unknown expression type: DERIV" expr))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else `(* ,m1 ,m2))))
(define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2))
           (+ a1 a2))
          (else `(+ ,a1 ,a2))))
(define (make-exponential base exponent)
  (cond ((=number? base 1) 1)
        ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        (else `(** ,base ,(- exponent 1)))))
