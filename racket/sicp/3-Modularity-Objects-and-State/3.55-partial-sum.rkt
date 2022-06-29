(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (stream-cons
       (apply proc (map stream-car argstreams))
       (apply stream-map (cons proc (map stream-cdr argstreams))))))

(define (add-stream s1 s2)
  (stream-map + s1 s2))

(define (add-stream s1 s2)
  (stream-cons (+ (stream-first s1)
                  (stream-first s2))
               (add-stream (stream-rest s1)
                           (stream-rest s2))))
(define (partial-sum s)
  (define (aux prev s)
    (let ((x (+ prev (stream-first s))))
      (stream-cons x
                   (aux x (stream-rest s)))))
  (aux 0 s))