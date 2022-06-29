#lang racket

(provide make-table)

;; for racket to scheme - compatibility layer
;; (define mcons cons)
;; (define mcar car)
;; (define mcdr cdr)
;; (define foldr fold-right)

;; (define (mlist . items) (foldr mcons '() items))


(define (make-table)
  (define (assoc key records)
    (cond ((null? records)
           false)
          ((equal? key (mcar (mcar records)))
           (mcar records))
          (else (assoc key (mcdr records)))))
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
                       (mcons (mcons key-1 (mcons
                                            (mcons key-2 value) '()))
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

(define (test)
  (define x (make-table))
  (display ((x 'insert!) '(a) 1))
  (display #\newline)
  ((x 'print))
  (display ((x 'insert!) '(b) 2))
  (display #\newline)
  ((x 'print))
  ((x 'insert!) '(c) 3)
  ((x 'print)))
