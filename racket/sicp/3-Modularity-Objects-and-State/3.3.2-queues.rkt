
#lang racket
;; q -> +-------+
;;      | * | * ------------------.
;;      +-|-----+                 |
;;        | front-ptr             | rear-ptr
;;        v                       v
;;      +-------+  +-------+  +-------+
;;      | * | * -->| * | * -->| * | \ |
;;      +-|-----+  +-|-----+  +-|-----+
;;        v          v          v
;;      +---+      +---+      +---+
;;      | a |      | b |      | c |
;;      +---+      +---+      +---+

(provide front-ptr rear-ptr set-front-ptr! set-rear-ptr! empty-queue?
         make-queue front-queue insert-queue! delete-queue!)

(define (front-ptr queue) (mcar queue))
(define (rear-ptr queue) (mcdr queue))
(define (set-front-ptr! queue item)
  (set-mcar! queue item))
(define (set-rear-ptr! queue item)
  (set-mcdr! queue item))


(define (empty-queue? queue)
  (null? (front-ptr queue)))
(define (make-queue) (mcons '() '()))

;; (insert-queue! q 'd)
;; q -> +-------+
;;      | * | * ---------------------------.
;;      +-|-----+                          |
;;        | front-ptr                      | rear-ptr
;;        v                                v
;;      +---+---+  +---+---+  +---+---+  +---+---+
;;      | * | * -->| * | * -->| * | * -->| * | \ |
;;      +-|-+---+  +-|-+---+  +-|-+---+  +-|-+---+
;;        v          v          v          v
;;      +---+      +---+      +---+      +---+
;;      | a |      | b |      | c |      | d |
;;      +---+      +---+      +---+      +---+

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (mcar (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (mcons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-mcdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

;; (delete-queue! q)
;; q -> +-------+
;;      | * | * ---------------------------.
;;      +-|-----+                          |
;;        | front-ptr                      | rear-ptr
;;         '---------.                     |
;;                   v                     v
;;      +---+---+  +---+---+  +---+---+  +---+---+
;;      | * | * -->| * | * -->| * | * -->| * | \ |
;;      +-|-+---+  +-|-+---+  +-|-+---+  +-|-+---+
;;        v          v          v          v
;;      +---+      +---+      +---+      +---+
;;      | a |      | b |      | c |      | d |
;;      +---+      +---+      +---+      +---+

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue
                         (mcdr (front-ptr queue)))
         queue)))