make-deque
empty-deque?
front-deque
rear-deque
front-insert-deque!
rear-insert-deque!
front-delete-deque!
rear-delete-deque!

(define (make-deque)
  (let ((deque (make-queue)))
    (define (set-front-ptr! item)
      (set-mcar! front-ptr item))
    (define (set-rear-ptr! item)
      (set-mcdr! rear-ptr item))
    (define (rear-insert-queue! item))
    (define (delete-queue!)
      (cond ((empty-queue? front-ptr)
             (error "DELETE! called with an empty queue" front-ptr))
            (else
             (set-front-ptr! (mcdr front-ptr))
             queue)))
    (define (front-queue)
      (if (empty-queue? front-ptr)
          (error "FRONT called with an empty queue" front-ptr)
          (mcar front-ptr)))
    (define (dispatch m)
      (case m
        ('front-deque (deque 'front-ptr))
        ('rear-deque  (deque 'rear-ptr))
        ('empty-queue? (deque 'empty-queue?))
        ('front-insert-queue! (deque 'insert-queue!))
        ('rear-insert-queue! (deque 'insert-queue!))
        ('front-delete-queue! (deque 'delete-queue!))
        ('rear-delete-queue! (deque 'delete-queue!))))
    dispatch))