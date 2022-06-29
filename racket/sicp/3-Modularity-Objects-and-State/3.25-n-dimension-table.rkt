
(define (make-table)
 (define (assoc key records)
   (cond ((null? records)
          false)
         ((equal? key (mcar (mcar records)))
          (mcar records))
         (else (assoc key (mcdr records)))))
  (let ((local-table (mcons '*table* '())))
    (define (lookup-1 keys table found-proc not-found-proc)
      (if (null? keys)
          (found-proc table)
          (let ((subtable (assoc (car keys) (mcdr table))))
            (if subtable
                (lookup-1 (cdr keys) subtable
                          found-proc not-found-proc)
                (not-found-proc keys table)))))
    (define (lookup . keys)
      (lookup-1 keys local-table
                (lambda (record) (mcdr record))
                (lambda (keys table) #f)))
    (define (insert! keys value)
      (define (found-proc record) (set-mcdr! record value))
      (define (not-found-proc keys table)
        (let ((subtable (mcons (car keys) '())))
          (set-mcdr! table
                     (mcons subtable
                            (mcdr table)))
          (lookup-1 (cdr keys) subtable
                    found-proc
                    not-found-proc)))
      (lookup-1 keys local-table
                found-proc
                not-found-proc)
      'ok)
    (define (print)
      (printf "~a~%" local-table))
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            ((eq? m 'print) print)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))