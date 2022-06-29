
(define (cont-frac n d k)
  (define (iter k acc)
    (if (zero? k)
        acc
        (iter (- k 1)
              (/ (n k) (+ (d k) acc)))))
  (define (rec i)
    (if (= i k)
        0
        (/ (n i) (+ (d i) (rec (+ i 1))))))
  ;; (iter k 0.0)
  (rec 0))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           k)

racket@> (cont-frac (lambda (i) 1.0)
                        (lambda (i) 1.0)
                        10)
0.6179775280898876
racket@> (cont-frac (lambda (i) 1.0)
               (lambda (i) 1.0)
               100)
0.6180339887498948

