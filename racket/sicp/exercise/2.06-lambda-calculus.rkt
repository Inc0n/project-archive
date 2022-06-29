
; from problem statement
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;; "Hint: Use substitution to evaluate (add-1 zero)"
;; (add-1 zero)
 ;; = (lambda (f) (lambda (x) (f ((zero f) x))))
      ;; but (zero f) = ((lambda (f) (lambda (x) x)) f)
                   ;; = ((lambda (f) x) f) ; independent of f!
                   ;; = (lambda (x) x)
 ;; = (lambda (f) (lambda (x) (f ((lambda (x) x) x))))
 ;; = (lambda (f) (lambda (x) (f x)))

;; so one must be as below
(define one
  (lambda (f) (lambda (x) (f x))))

;; (add-1 one)
 ;; = (lambda (f) (lambda (x) (f ((one f) x))))
      ;; but (one f) = (lambda (f) (lambda (x) (f x)))
 ;; = (lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
 ;; = (lambda (f) (lambda (x) (f (f x))))

(define two
  (lambda (f) (lambda (x) (f (f x)))))

(define (compose f g)
  `(lambda (x) (,f (,g x))))

(define zero `(lambda (f) (lambda (x) x)))

(define (add-1 n)
  `(lambda (f) (lambda (x) (f ((,n f) x)))))

;; (add-2 n) = (add-1 add-1)
;;  = (lambda (f) (lambda (x) (f ((add-one f) x))))
;;  = (lambda (f) (lambda (x) (f ((lambda (z) (lambda (x) (z ((f z) x)))) x))))
;;  = (lambda (f) (lambda (x) (f (lambda (z) (x ((f x) z))))))
(lambda (f) (lambda (x) (f (f ((n f) x) ))))

(define (add m n)
    (lambda (f)
        (lambda (x)
            ((m f) ((n f) x)))))