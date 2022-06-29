
#lang racket

(provide make-vect xcor-vect ycor-vect add-vect scale-vect)

(define (make-vect x y) (cons x y))

(define (xcor-vect vect) (car vect))
(define (ycor-vect vect) (cdr vect))

(define (op-vect op vect1 vect2)
  (make-vect (op (xcor-vect vect1) (xcor-vect vect2))
             (op (ycor-vect vect1) (ycor-vect vect2))))
(define (add-vect vect1 vect2)
  (make-vect (+ (xcor-vect vect1) (xcor-vect vect2))
             (+ (ycor-vect vect1) (ycor-vect vect2)))
  ;; (op-vect + vect1 vect2)
  )

(define (sub-vect vect1 vect2)
  ;; (op-vect - vect1 vect2)
  (make-vect (- (xcor-vect vect1) (xcor-vect vect2))
             (- (ycor-vect vect1) (ycor-vect vect2))))

(define (scale-vect vect1 scale)
  (make-vect (* (xcor-vect vect1) scale)
             (* (ycor-vect vect1) scale)))
