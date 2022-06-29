
;; for racket to scheme - compatibility layer
;; (define mcons cons)
;; (define mcar car)
;; (define mcdr cdr)
;; (define foldr fold-right)
#lang racket

(provide make-table)

(define (mlist . items) (foldr mcons '() items))
;; interface to binary tree
;; (define empty-tree? null?)
(define (entry tree) (mcar tree))
(define (left-branch tree) (mcdr tree))
(define (right-branch tree) (mcdr (mcdr tree)))
(define (make-tree entry left right)
  (mlist entry left right))
;; interface to record
(define (make-record key value) (mcons key value))
(define (record-key record) (mcar record))
(define (record-value record) (mcdr record))

(define (set-value! x value) (set-mcdr! x value))

;; table impl
(define (make-table)
  (define (assoc key set equal-proc inequal-proc)
    (define (assoc-1 set)
      (cond ((equal? key (record-key (entry set)))
             (equal-proc (entry set)))
            ((symbol<? key (record-key (entry set)))
             (inequal-proc assoc-1 (left-branch set)))
            (else
             (inequal-proc assoc-1 (right-branch set)))))
    (assoc-1 set))
  (define (rec-assoc keys set equal-proc inequal-proc)
    (define (rec-assoc-1 keys set)
      (assoc (car keys) set
             (lambda (record)
               (if (null? keys)
                   (equal-proc record)
                   (rec-assoc-1 (cdr keys) (record-value record))))
             (lambda (assoc-1 branch)
               (inequal-proc rec-assoc-1 branch keys))))
    (rec-assoc-1 keys (mcdr set)))
  (define (single? x) (null? (mcdr x)))
  (define (new-tree keys value)
    (if (null? keys)
        value
        (make-tree (make-record (car keys)
                                (new-tree (cdr keys) value))
                   '() '())))
  (let ((local-table (mcons '*table* '())))
    (define (print)
      (display local-table)
      (display #\newline))
    (define (lookup . keys)
      (rec-assoc keys local-table
                 (lambda (record) (recrod-value record))
                 (lambda (assoc-1 branch keys)
                   (if (null? (mcar branch))
                       #f
                       (assoc-1 (mcar branch))))))
    (define (insert! keys value)
      (if (single? local-table)
          (set-value! local-table (new-tree keys value))
          (rec-assoc keys local-table
                     (lambda (record)
                       (set-value! record value))
                     (lambda (rec-assoc branch keys)
                       (if (null? (mcar branch))
                           (if (null? keys)
                               #f
                               (set-mcar! branch
                                          (new-tree keys value)))
                           (rec-assoc keys (mcar branch)))))))
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            ((eq? m 'print) print)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

(define (test)
  (define x (make-table))
  ;; ((x 'insert!) '(a) 1)
  ;; ((x 'print))
  ;; ((x 'insert!) '(b) 2)
  ;; ((x 'print))
  ;; ((x 'insert!) '(c) 3)
  ((x 'insert!) '(a a) 1)
  ((x 'insert!) '(a b) 2)
  ((x 'insert!) '(a c) 3)
  ((x 'print)))

