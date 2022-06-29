
#lang racket

(provide make-frame frame-origin frame-edge1 frame-edge2)

;; case one
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

;; case two
;; (define (make-frame origin edge1 edge2)
;;   (cons origin
;;         (cons edge1 edge2)))

;; selectors
(define (frame-origin frame) (car frame))
(define (frame-edge1 frame) (cadr frame))

;; for case 1
(define (frame-edge2 frame) (caddr frame))
;; for case 2
;; (define (frame-edge2 frame) (cddr frame))