
(define (stream-fibmap proc stream)
  (define (aux stream last-value)
    (let ((this (stream-car stream)))
      (let ((x (proc this last-value)))
        (stream-cons x
                     (stream-fibmap (stream-cdr stream)
                                    this)))))
  (aux (stream-rest stream)
       (stream-first stream)))

(define (smooth input-s)
  (stream-fibmap (lambda (x0 x1)
                   (/ (+ x0 x1) 2))
                 input-s ))

;; modular zero-crossing

(define (make-zero-crossings input-stream last-value last-avpt)
  (let ((s (smooth input-stream)))
    (stream-map sign-change-detector s (stream-rest s))))