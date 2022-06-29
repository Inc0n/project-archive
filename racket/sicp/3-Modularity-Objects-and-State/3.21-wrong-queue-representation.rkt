
(require "3.3.2-queues.rkt")

(define q1 (make-queue))

(insert-queue! q1 'a)
((a) a)
(insert-queue! q1 'b)
((a b) b)
(delete-queue! q1)
((b) b)
(delete-queue! q1)
(() b)

(define (print-queue queue)
  (printf "~a" (rear-ptr queue)))