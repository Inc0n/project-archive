(load-option 'format)

#|
fn assoc
| is pre-defined & compiled, here is a lisp version ;
| but no need to use this slower implementation instead
(define (assoc key records)
    (cond ((null? records)
           false)
          ((equal? key (car (car records)))
           (car records))
          (else (assoc key (cdr records)))))
|#

(define (make-table)
  (let ((local-table '(*table*)))
    (define (assoc-aux keys table found-proc not-found-proc)
      (if (null? keys)
          (found-proc table)
          (let ((subtable (assoc (car keys) (cdr table))))
            (if subtable
                (assoc-aux (cdr keys) subtable
                          found-proc not-found-proc)
                (not-found-proc keys table)))))
    (define (lookup . keys)
      (assoc-aux keys local-table
                (lambda (record) (cdr record))
                (lambda (keys table) #f)))
    (define (insert! keys value)
      (define (found-proc record)
        (set-cdr! record value))
      (define (not-found-proc keys table)
        (let ((subtable (cons (car keys) '())))
          (set-cdr! table
                    (cons subtable
                          (cdr table)))
          (assoc-aux (cdr keys) subtable
                    found-proc
                    not-found-proc)))
      (assoc-aux keys local-table
                found-proc
                not-found-proc)
      'ok)
    (define (print)
      (let ((str-rep (format #f "~a" local-table)))
        (format #t "~a~%" str-rep)
        str-rep))
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            ((eq? m 'print) print)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

(define (test-runner)
  (let ((total 0)
        (passed 0))
    (define (run-test expr expected)
      (set! total (+ total 1))
      (if (equal? expr expected)
          (begin (set! passed (+ passed 1))
                 #t)
          #f))
    (define (report)
      (format t "=> ~a/~a~%" passed total))
    (define (dispatch msg)
      (case msg
        ('report report)
        ('run-test run-test)
        (else (error "Unknown operation: TEST" msg))))
    dispatch))

(define-syntax run-test
  (syntax-rules ()
    ((run-test tester expr expected)
     (format #t "~a ~a ~a~%" expr expected
             (if ((tester 'run-test) expr expected)
                 "passed"
                 "failed")))))

(define (test)
  (define tester (test-runner))
  (define x (make-table))
  (run-test tester ((x 'insert!) '(a) 1) 'ok)
  (run-test tester ((x 'print)) '(*table* (a . 1)))
  ((tester 'report)))

