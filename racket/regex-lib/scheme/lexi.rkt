(require "stream-oop.rkt"
         "operation-table.rkt")
;; (define operation-table (make-table))
;; (define get (operation-table 'lookup))
;; (define put (operation-table 'insert!))

(define (re/tag x y) (cons x y))

(define (stream-next stream) ((stream 'read-char) 'done))
(define (read-until stream c) ((stream 'read-until) c))

(define (lexi stream char)
  (let ((lexi-proc (get 'lexi char)))
    (if lexi-proc
        (lexi-proc stream char)
        (re/tag 'char char))))

(define (install-re-lexi)
  (define (lexi-self stream c) (re/tag c c))
  (define (lexi-repeat stream c) c)
  ;;
  (put 'lexi #\$ lexi-self)
  (put 'lexi #\^ lexi-self)
  (put 'lexi #\. lexi-self)
  ;;
  (put 'lexi #\* lexi-repeat)
  (put 'lexi #\+ lexi-repeat)
  (put 'lexi #\? lexi-repeat)
  ;;
  (put 'lexi #\|
       (lambda (stream c) (re/tag c (lexi stream (stream-next stream)))))
  (put 'lexi 'char
       (lambda (stream c) (re/tag 'char c)))
  (put 'lexi #\[
       (lambda (stream c) (re/tag c ((stream 'read-until) #\]))))
  (put 'lexi #\{
       (lambda (stream c) (re/tag c ((stream 'read-until) #\}))))
  (put 'lexi #\(
       (lambda (stream c) (re/tag c ((stream 'read-until) #\)))))
  (put 'lexi #\\
       (lambda (stream c) (re/tag c (stream-next stream))))
  ;;
  'done)

(define (install-re-parse)
  (define (parse-re/pop re acc)
    (re/tag (cons re (car acc)) (cdr acc)))
  (define (parse-re/brkt re acc)
    (cons (parse/square-brkt re) acc))
  (put 'parse #\* parse-re/pop)
  (put 'parse #\+ parse-re/pop)
  (put 'parse #\? parse-re/pop)
  (put 'parse #\| parse-re/pop)
  ;;
  (put 'parse #\[ parse-square-brkt)
  'done)

(define (parse lexical-re acc)
  (install-re-parse)
  (define (parsable? x) (char? x))
  (if (parsable? lexical-re)
      (let ((parse-proc (get 'parse lexical-re)))
        (if parse-proc
            (parse-proc lexical-re acc)
            (error "a proc is missing for re-type: PARSE" lexicla-re)))
      (cons lexical-re acc)))

(define (analysis string)
  (install-re-lexi)
  (define stream (make-string-stream string))
  (define (iter acc)
    (let ((c (stream-next stream)))
      (if (eq? c 'done)
          (reverse acc)
          (iter (parse (lexi stream c) acc)))))
  (iter '()))

