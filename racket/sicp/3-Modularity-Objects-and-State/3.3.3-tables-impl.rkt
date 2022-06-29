
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        #f)))

(define (assoc key records)
  (cond ((null? records)
         false)
        ((equal? key (caar records))
         (car records))
        (else (assoc key (mcdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-mcdr! record value)
        (set-mcdr! table
                   (mcons (mcons key value)
                          (mcdr table)))))
  'ok)

(define (make-table)
  (list '*table*))

(define (loopup key1 key2 table)
  (let ((subtable (assoc key1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              false))
        false)))

(define (make-table)
  (define (assoc key records)
    (cond ((null? records)
           false)
          ((equal? key (mcar (mcar records)))
           (mcar records))
          (else (assoc key (mcdr records)))))
  (define (mlist . x)
    (foldr mcons '() x))
 (let ((local-table (mcons '*table* '())))
   (define (lookup key-1 key-2)
     (let ((subtable (assoc key-1 (mcdr local-table))))
       (if subtable
           (let ((record (assoc key-2 (mcdr subtable))))
             (if record
                 (mcdr record)
                 false))
           false)))
   (define (insert! key-1 key-2 value)
     (let ((subtable (assoc key-1 (mcdr local-table))))
       (if subtable
           (let ((record (assoc key-2 (mcdr subtable))))
             (if record
                 (set-mcdr! record value)
                 (set-mcdr! subtable
                            (mcons (mcons key-2 value)
                                   (mcdr subtable)))))
           (set-mcdr! local-table
                      (mcons (mlist key-1 (mcons key-2 value))
                             (mcdr local-table)))))
     'ok)
   (define (print)
     (printf "~a~%" local-table))
   (define (dispatch m)
     (cond ((eq? m 'lookup) lookup)
           ((eq? m 'insert!) insert!)
           ((eq? m 'print) print)
           (else (error "Unknown operation: TABLE" m))))
   dispatch))