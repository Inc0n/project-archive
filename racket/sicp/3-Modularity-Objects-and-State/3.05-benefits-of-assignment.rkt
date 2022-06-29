
(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (random) (random)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0) (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

;; Exercise 3.5

(define (square x) (* x x))

(define (estimate-circle trials cx cy r)
  (let ((radius (square r))
        (d  (* 2 r))
        (x1 (- cx r))
        (y1 (- cy r))
        (x2 (+ cx r))
        (y2 (+ cy r)))
    (* (square d) (monte-carlo-integral trials
                                (lambda (x y)
                                  (<= (+ (square (- x cx))
                                         (square (- y cy)))
                                      radius))
                                x1 y1 x2 y2))))

(define (monte-carlo-integral trials P x1 y1 x2 y2)
  (monte-carlo trials (lambda ()
                        (P (random-in-range x1 x2)
                           (random-in-range y1 y2)))))


(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))


;; exercise 3.6

(define (rand-update x)
  (let ((a 27) (b 26) (m 127))
    (modulo (+ (* a x) b) m)))

(define rand
  (let ((seed random-init))
    (define (dispatch message)
      (case message
        ('generate
         (set! seed (rand-update seed))
         seed)
        ('reset
         (lambda (x)
           (set! seed x)))))
    dispatch))