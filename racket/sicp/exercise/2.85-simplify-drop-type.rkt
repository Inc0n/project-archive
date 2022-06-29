


(define (install-scheme-number-package)
  ;; ...
  ;; ...
  (put 'project? 'scheme-numer (lambda (x) #f))
  'done)

(extern lowest-common-multiple)
(define (install-rational-package)
  ;; ...
  ;; ...
  (put 'project 'rational
       (lambda (x)
         (make-scheme-numer (project? (numer x) (denom x)))))
  (put 'project? 'rational
       (lambda (x)
         (let ((numer (numer x))
               (denom (denom x)))
           (= (lowest-common-multiple numer denom) denom))))
  'done)

(define (install-real-package)
  ;; ...
  ;; ...
  (put 'project 'real
       (lambda (x)
         (let ((rat (rationalize x)))
           rat)))
  (put 'project? 'real)
  'done)
(define (install-complex-package)
  ;; ...
  ;; ...
  (put 'project 'rational
       (lambda (x)
         (make-rational (/ (numer x) (denom x)))))
  (put 'project? 'rational
       (lambda (x)
         (= (imag-part x) 0)))
  'done)

(define (drop x)
  (let ((type (type-tag x)))
    (if ((get 'project? type) x)
        (let ((project-proc (get 'project type)))
          (if project-proc
              (let ((project-number (project-proc (contents x))))
                (drop project-number))
              x))
        x)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (drop (apply proc (map contents args)))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((x (- (type-lvl type1)
                            (type-lvl type2))))
                  (if (zero? x)
                      (apply-generic op a1 a2)
                      (if (positive? x)
                          (apply-generic op a1 (map-times raise a2 x))
                          (apply-generic op (map-times raise a1 x) a2)))))
              (error "No method for these types"
                     (list op type-tags)))))))