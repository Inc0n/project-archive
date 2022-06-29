
(define (install-scheme-number-package)
  ;; ...
  (put 'equ? '(scheme-number scheme-number) =)
  'done)

 (define (install-rational-package)
   ;; ...
   (define (equ? x y)
     (= (* (numer x) (denom y))
        (* (numer y) (denom x))))
;; (define (rational-equ? n1 n2)
;;   (= (/ (numer n1) (denom n1))
;;      (/ (numer n2) (denom n2))))
   ;; ...
   (put 'equ? '(rational rational) equ?)
   'done)

 (define (install-complex-package)
   ;; ...
   (define (equ? x y)
     (and (= (real-part x) (real-part y))
          (= (imag-part x) (imag-part y))))
   ;; ...
   (put 'equ? '(complex complex) equ?)
   'done)

 (define (equ? x y) (apply-generic 'equ? x y))
