
;; (define (make-poly variable term-lst)
;;   (let ((poly (cons variable term-lst)))
;;     (define (dispatch m)
;;       (case m
;;         ('variablle variable)
;;         ('term-list term-lst)
;;         (else poly)))
;;     dispatch))

(define (add-terms p1 p2))
(define (install-polynomial-package)
  (define (same-variable? var1 var2) (eq? var1 var2))
  (define (variable? x) (symbol? x))
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  ;; representation of term
  (define (make-term coff power) (cons coff power))
  (define (coefficient p) (car p))
  (define (power p) (cdr p))
  ;; representation of terms and term lists
  ⟨procedures adjoin-term ::: coeff from text below⟩
  (define (add-poly p1 p2)
    (if (same-variable? (poly-variable p1) (poly-variable p2))
        (make-poly (poly-variable p1)
                   (add-terms (poly-term-list p1) (poly-term-list p2)))
        (error "Polys not in the same var: ADD-POLY" (list p1 p2))))
  (define (add-terms term-lst1 term-lst2)
    (define (add term1 term2)
      (if term2
          (make-term (+ (coefficient term1)
                        (coefficient term2))
                     (power term2))
          term1))
    (term-op add term-lst1 term-lst2))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1))
                 (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term (make-term (order t1)
                                            (add (coeff t1)
                                                 (coeff t2)))
                                 (add-terms (rest-terms L1)
                                            (rest-terms L2)))))))))

  (define (mul-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1) (term-list p2)))
      (error "Polys not in the same var: MUL-POLY" (list p1 p2))))
  (define (mul-terms L1 L2)
    (if (empty-term-list? L1)
        (empty-term-list)
        (add-terms (mul-all-terms-by (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-all-terms-by t1 lst)
    (if (empty-term-list? L2)
        (empty-term-list)
        (let ((t2 (first-term L)))
          (adjoin-term (make-term (+ (order t1)
                                     (order t2))
                                  (mul (coeff t1) (coeff t2)))
                       (mul-term-by-all-terms t1 (rest-terms L))))))
  ⟨procedures used by mul-poly⟩
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)