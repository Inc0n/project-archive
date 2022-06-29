
(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (iter n acc)
    (if (< n 1)
        acc
        (iter (- n 1) (compose f acc))))
  (iter (- n 1) f))

(module test racket
  (require rackunit)
  (define (square x)
    (* x x))
  ;; (define (compose-test f g x)
  ;;   ((compose f g) x))
  (check-= ((repeated square 2) 5) 625 0 "check repeated"))