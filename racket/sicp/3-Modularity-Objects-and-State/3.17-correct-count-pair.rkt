
;; Exercise 3.17-count-pair

(define (my-count-pairs x)
  (let ((n 0))
    (define (iter pair acc)
      ;; (printf "~a~%" pair)
      (if (null? pair)
          acc
          (if (pair? pair)
              (iter (cdr pair) (iter (car pair) acc))
              (if (member pair acc)
                  acc
                  (begin
                    (set! n (+ n 1))
                    (cons pair acc))))))
    (iter x '())
    n))

;;; a more elegant solution from the schemewiki

;; AThird

;; A slightly different way of doing it. Note that unlike the above solution this one continues to traverse pairs it's already seen and so will never return if there's a loop.

(define (count-pairs x)
  (let ((counted '()))
    (define (uncounted? x)
      (if (memq x counted)
          0
          (begin
            (set! counted (cons x counted))
            1)))
    (define (count x)
      (if (not (pair? x))
          0
          (+ (count (car x))
             (count (cdr x))
             (uncounted? x))))
    (count x)))
