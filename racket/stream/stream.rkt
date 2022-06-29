#lang racket

(provide test-stream make-stream)
(require "data-tag.rkt"
         "operation-table.rkt")

(define (tag name x) (attach-tag name x))

(define (install-stream)
  (define (list-copy lst)
    (define (iter lst acc)
      (if (null? lst)
          (reverse lst)
          (iter (cdr lst) (cons (car lst) acc))))
    (iter lst '()))
  (define (make-list-stream lst) (tag 'list-stream (list-copy lst)))
  (define (make-string-stream string)
    (tag 'string-stream
         (cons 0 (string-length string) string)))
  (define (read-char stream)
    (let ((index (stream-index stream)))
      (let ((char (string-ref (stream-string stream)
                              index)))
        (set! (stream-index stream) (+ index 1))
        char)))
  ;;
  (put 'make-stream 'list make-list-stream)
  (put 'make-stream 'string make-string-stream)
  ;;
  (put 'string-stream 'stream-index (lambda (stream) (car stream)))
  (put 'string-stream 'stream-length (lambda (stream) (cadr stream)))
  (put 'string-stream 'stream-string (lambda (stream) (caddr stream)))
  (put 'string-stream 'read-char read-char)
  (put 'string-stream 'read-until read-until)
  'done)

(define (test-stream)
  (define (print-line x)
    (display x) (display #\newline))
  (define x (make-stream "3]"))
  (print-line ((x 'read-char)))
  (print-line ((x 'read-char)))
  (print-line ((x 'read-until) #\])))

(define (test))

