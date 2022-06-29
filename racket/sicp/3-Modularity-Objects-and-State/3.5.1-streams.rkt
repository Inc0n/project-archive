;; #lang racket

;; (provide stream-from-to)

(define (memo-proc proc)
  (let ((already-run? false)
        (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

;; (define (delay x) (memo-proc (lambda () x)))

(define-syntax (delay syntax-object)
  (syntax-case syntax-object ()
    ((_ a)
     #'(lambda () a))))

(define (force delayed-object)
  (if (procedure? delayed-object)
      (delayed-object)
      delayed-object))

(define the-empty-stream '())
(define stream-null? null?)

(define (stream-cons a b)
  (cons a (delay b)))
(define (strea-car stream) (car stream))
(define (strea-cdr stream) (force (cdr stream)))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (stream-cons (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))


(define (stream-filter pred s)
  (if (stream-null? s)
      the-empty-stream
      (if (pred (stream-car s))
          (stream-cons (stream-car s)
                       (stream-filter pred (stream-cdr s)))
          (stream-filter pred (stream-cdr s)))))

(define (stream-from-to low high)
  (if (> low high)
      the-empty-stream
      (stream-cons low
                   (stream-from-to (+ low 1) high))))

;; stream-for-each
;; is useful for viewing streams:
(define (display-stream s) (stream-for-each display-line s))
(define (display-line x) (display x) (newline))