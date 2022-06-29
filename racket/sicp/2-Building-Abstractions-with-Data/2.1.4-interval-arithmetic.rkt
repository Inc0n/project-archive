
;; Exercise 2.7
;; Define selectors upper-bound and lower-bound to complete the
;; implementation
(define (make-interval a b) (cons a b)) ;; given

(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

;;

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y)))))

;; Exercise 2.8
;; Define sub-interval that finds the difference of the two intervals
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;; Exercise 2.9
;; The width of an interval is half of the difference between its upper
;; and lower bounds.
(define (interval-width x)
  (/ (- (upper-bound x) (lower-bound x)) 2))


;; Exercise 2.11
;; multiple
(define (mul-interval x y)
  (define (endpoint-sign i)
    (cond ((and (>= (upper-bound i) 0)
                (>= (lower-bound i) 0))
           1)
          ((and (< (upper-bound i) 0)
                (< (lower-bound i) 0))
           -1)
          (else 0)))
  (let ((es-x (endpoint-sign x))
        (es-y (endpoint-sign y))
        (x-up (upper-bound x))
        (x-lo (lower-bound x))
        (y-up (upper-bound y))
        (y-lo (lower-bound y)))
    (if (and (= es-x 0) (= es-y 0))
        ;; Take care of the exceptional condition where we have to test
        (make-interval (min (* x-lo y-up) (* x-up y-lo))
                       (max (* x-lo y-lo) (* x-up y-up)))
        ;; Otherwise, select which value goes in which "slot". I'm not
        ;; sure whether there is an intuitive way to explain *why* these
        ;; selections work.
        (let ((a1 (if (and (<= es-y 0) (<= (- es-y es-x) 0)) x-up x-lo))
              (a2 (if (and (<= es-x 0) (<= (- es-x es-y) 0)) y-up y-lo))
              (b1 (if (and (<= es-y 0) (<= (+ es-y es-x) 0)) x-lo x-up))
              (b2 (if (and (<= es-x 0) (<= (+ es-x es-y) 0)) y-lo y-up)))
          (make-interval (* a1 a2) (* b1 b2))))))