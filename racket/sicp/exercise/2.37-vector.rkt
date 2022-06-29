
(require "2.36-accumulate-n.rkt")

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (w)
         (dot-product v w))
       m))

;; (matrix-*-vector '((1 -1 2)
;;                    (0 -3 1))
;;                  '(2 1 0))

(define (transpose mat)
  (accumulate-n cons '() mat))

(transpose '((1 -1 2)
             (0 -3 1)
             (2 1 0)))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (m-rows)
           (matrix-*-vector cols m-rows))
         m)))