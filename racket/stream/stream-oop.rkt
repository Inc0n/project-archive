
#lang racket

(provide make-stream
         make-string-stream)

(require "data-tag.rkt"
         "operation-table.rkt")

(define (tag name x) (attach-tag name x))


(define (list-copy lst)
  (define (iter lst acc)
    (if (null? lst)
        (reverse lst)
        (iter (cdr lst) (cons (car lst) acc))))
  (iter lst '()))

(define (make-list-stream lst)
  (let ((lst (list-copy lst)))
    (define (lst-car!)
      (if (null? lst)
          #f
          (let ((fst (car lst)))
            (set! lst (cdr lst))
            fst)))
    (define (lst-cdr)
      (if (null? lst)
          #f
          (cdr lst)))
    (define (dispatch m)
      (case m
        ('car! lst-car!)
        ('cdr lst-cdr)
        (else (error "unknown message: LIST-STREAM" m))))
    dispatch))

(define (make-string-stream string)
  (let ((index 0)
        (length (string-length string)))
    (define (read-char end-val)
      (if (>= index length)
          end-val
          (let ((char (string-ref string index)))
            (set! index (+ index 1))
            char)))
    (define (read-until delim)
      (let ((char (read-char 'done)))
        (cond ((eq? char 'done) '())
              ((char=? char delim)
               '())
              (else (cons char (read-until delim))))))
    (define (dispatch m)
      (case m
        ('read-char read-char)
        ('read-until read-until)
        (else (error "unknown message: STRING-STREAM" m))))
    dispatch))
;;
(put 'make-stream 'list
     (lambda (lst) (tag 'list-stream (make-list-stream lst))))
(put 'make-stream 'string
     (lambda (str) (tag 'string-stream (make-string-stream str))))

(define (apply-generic name object)
  (let ((type (type-tag object)))
    (let ((proc (get type name)))
      (if proc
          (proc object)
          (error "unknown procedure name: APPLY-GENERIC" name)))))

(define (make-stream obj)
  (define (type-of x)
    (cond ((list? x) 'list)
          ((char? x) 'char)
          ((vector? x) 'vectorr)
          ((number? x) 'number)
          ((string? x) 'string)
          ((symbol? x) 'symbol)
          ((keyword? x) 'keyword)
          (else (error "unknown type of object " x))))
  (define (get-make-stream type)
    (let ((proc (get 'make-stream type)))
      (if (procedure? proc)
          proc
          (error "make-stream missing for type: MAKE-STREAM" type))))
  ((get-make-stream (type-of obj)) obj))