
(define (install-scheme-number-package)
  ;; ...
  (put 'zero?? 'scheme-number zero?)
  'done)

 (define (install-rational-package)
   ;; ...
   (define (zero? x) (= (numer x) 0))
;; (define (rational-zero?? n1 n2)
;;   (= (/ (numer n1) (denom n1))
;;      (/ (numer n2) (denom n2))))
   ;; ...
   (put 'zero?? 'rational zero?)
   'done)

 (define (install-complex-package)
   ;; ...
   (define (zero? x)
     (and (= (real-part x) 0)
          (= (imag-part x) 0)))
   ;; ...
   (put 'zero?? 'complex zero?)
   'done)


(define (zero?? x) (apply-generic 'zero? x))
