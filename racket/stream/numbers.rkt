
(require "data-tag.rkt"
         "operation-table.rkt")
;; (define operation-table (make-table))
;; (define get (operation-table 'lookup))
;; (define put (operation-table 'insert!))

(define (install-scheme-number-package)
  (define (raise num) ((get 'make 'rational) num 1))
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y)
         (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y)
         (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y)
         (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y)
         (tag (/ x y))))
  (put 'make '(scheme-number)
       (lambda (x) (tag x)))
  (put 'equ? '(scheme-number scheme-number) =)
  (put 'zero? 'scheme-number zero?)
  (put 'project? 'scheme-number (lambda (x) #f))
  (put 'raise  'scheme-number raise)
  'done)

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (equ? x y)
    (= (* (numer x) (denom y))
       (* (numer y) (denom x))))
  (define (zero? x) (= (numer x) 0))
  (define (raise x) (make-from-real-imag (/ (numer x) (denom x)) 0))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y)
         (tag (add-rat x y))))
  (put 'sub '(rational rational) (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational) (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational) (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational) equ?)
  (put 'zero? 'rational zero?)
  (put 'raise 'rational raise)
  (put 'project? 'rational
       (lambda (x)
         (let ((numer (numer x))
               (denom (denom x)))
           (= (lowest-common-multiple numer denom) denom))))
  (put 'project 'rational
       (lambda (x) (make-scheme-numer (numer x) (denom x))))
  'done)

(define (make-rational n d) ((get 'make 'rational) n d))

(define (install-rectangular-package) ;; internal procedures
  (define (square x) (* x x))
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z)) (square (imag-part z)))))
  (define (angle z) (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part 'rectangular real-part)
  (put 'imag-part 'rectangular imag-part)
  (put 'magnitude 'rectangular magnitude)
  (put 'angle 'rectangular angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a)
         (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z) (* (magnitude z)
                           (cos (angle z))))
  (define (imag-part z) (* (magnitude z)
                           (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  (define (equ? x y)
    (and (= (real-part x) (real-part y))
         (= (imag-part x) (imag-part y))))
  (define (zero? x)
    (and (= (real-part x) 0)
         (= (imag-part x) 0)))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part 'complex real-part)
  (put 'imag-part 'complex imag-part)
  (put 'magnitude 'complex magnitude)
  (put 'equ? '(complex complex) equ?)
  (put 'zero? 'complex zero?)
  (put 'project? 'complex
       (lambda (x) (= (imag-part x) 0)))
  (put 'project 'complex
       (lambda (x)
         (make-rat (real-part x) 1)))
  'done)

(define (type-lvl type)
  (cond ((eq? type 'scheme-number) 0)
        ((eq? type 'rational) 1)
        ;; ((eq? type 'real) 2)
        ((eq? type 'complex) 2)
        ((eq? type 'rectangular) 2)
        (else (error "Invalid type: LEVEL" type))))


(define (single? x) (null? (cdr x)))

(define (map-times f initial-val n)
  (if (< n 1)
      (error "n has be bigger than 0: MAP-TIMES" n)
      #f)
  (define (iter n acc)
    (if (= n 0)
        acc
        (iter (- n 1) (f acc))))
  (iter n initial-val))

(define (apply-generic-1 op arg)
  (let ((tag (type-tag arg)))
    (let ((proc (get op tag)))
      (if proc
          (proc (contents arg))
          (error "No method for these types"
                 (list op tag))))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
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


;; generic
(define (raise x) (apply-generic-1 'raise x))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (equ? x y) (apply-generic 'equ? x y))
(define (zero? x) (apply-generic 'zero? x))

;; scheme number
(define (make-scheme-number x) ((get 'make 'scheme-number) x))
;; rational
(define (make-rat n d) ((get 'make 'rational) n d))
;; complex
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (real-part x) (apply-generic-1 'real-part x))
(define (imag-part x) (apply-generic-1 'imag-part x))

(define (drop x)
  (let ((type (type-tag x)))
    (if ((get 'project? type) x)
        (let ((project-proc (get 'project type)))
          (if project-proc
              (let ((project-number (project-proc (contents x))))
                (drop project-number))
              x))
        x)))

(define (test)
  (install-scheme-number-package)
  (install-rational-package)
  (install-rectangular-package)
  (install-polar-package)
  (install-complex-package)
  ;; (define (test-1 . args)
  ;;   (map type-tag args))
  (add (make-from-real-imag 2 0)
       (make-from-real-imag 2 0)))

(begin
  (install-scheme-number-package)
  (install-rational-package)
  (install-rectangular-package)
  (install-polar-package)
  (install-complex-package))