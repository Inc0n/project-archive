; S n

(define (add-stream s1 s2)
  (stream-cons (+ (stream-first s1)
                  (stream-first s2))
               (add-stream (stream-rest s1)
                           (stream-rest s2))))
(define (partial-sums s)
  (define (aux prev s)
    (let ((x (+ prev (stream-first s))))
      (stream-cons x
                   (aux x (stream-rest s)))))
  (aux 0 s))

(define (ln2-summands n)
  (stream-cons (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))

(define (stream-limit s tolerence)
  (define (aux prev s)
    (if (stream-empty? s)
        false
        (let ((next (stream-first s)))
          (if (< (abs (- next prev)) tolerence)
              next
              (aux next (stream-rest s))))))
  (aux (stream-first s) (stream-rest s)))

(define ln2-stream
  (partial-sums (ln2-summands 1)))

;; (stream-ref ln2-stream 1000)
;; 0.6936464315588232
;; it takes 1000 tries before reaching a 2d.p accuracy
;; and noticable calculation time abt 0.5 sec

(define (euler-transform s)
  (define (square x) (* x x))
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (stream-cons (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-rest s)))))

(define euler-ln2-stream
  (stream-limit
   (euler-transform (partial-sums (ln2-summands 1)))
   0.000001))

;; (stream-ref euler-ln2-stream 100)
;; 0.6931472966210653
;; it took 100 tries before getting a 6.d.p accuracy result
;; not too bad

(define (make-tableau transform s)
  (stream-cons s (make-tableau transform (transform s))))

;; The tableau has the form
;;     s00 s01 s02 s03 s04 ...
;;         s10 s11 s12 s13 ...
;;             s20 s21 s22 ...
;;                 ...

;; Finally, we form a sequence by taking the
;; first term in each row of the tableau:

(define (accelerated-sequence transform s)
  (stream-map stream-first (make-tableau transform s)))

;; (define (display-stream s) (stream-for-each display-line s))
;; (define (display-line x) (display (x)) (newline))

(define acc-ln2-stream
  (accelerated-sequence euler-transform
                        ln2-stream))

;; (stream-ref acc-ln2-stream 5)
;; 0.6931471806635636
;; racket@> (stream-ref acc-ln2-stream 6)
;; 0.6931471805604039
;; ...
;; racket@> (stream-ref acc-ln2-stream 9)
;; 0.6931471805599454
;; racket@> (stream-ref acc-ln2-stream 10)
;; +nan.0
