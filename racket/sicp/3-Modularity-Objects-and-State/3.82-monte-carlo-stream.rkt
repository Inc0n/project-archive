

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

(define (monte-carlo experiment)
  (define (aux x)
    (let ((est (car x))
          (past-trials (cdr x)))
      (let ((passed (* est past-trials))
            (trials (+ past-trials 1)))
        (let ((new-est (if (experiment)
                           (/ (+ passed 1) trials)
                           (/ passed trials))))
          (cons new-est trials)))))
  (define monte-steram
    (stream-cons '(0 . 0)
                 (stream-map aux monte-stream)))
  monte-stream)

(define (square x) (* x x))


(define (monte-carlo-integral P x1 y1 x2 y2)
  (monte-carlo (lambda ()
                 (P (random-in-range x1 x2)
                    (random-in-range y1 y2)))))


(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

