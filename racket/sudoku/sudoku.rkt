
(define (stream-from-to low high)
  (if (< low high)
      the-empty-stream
      (stream-cons low
                   (stream-from-to (+ low 1) high))))
(define (display-stream s) (stream-for-each display-line s))
(define (display-line x) (display x) (newline))

(define the-empty-stream '())

(define (single? x) (null? (cdr x)))

(define (permut lst)
  (define (iter lst prev acc)
    (cond ((null? lst) (reverse acc))
          (else
           (let ((fst (car lst))
                 (rst (cdr lst)))
             (iter rst
                   (reverse (cons fst prev))
                   (cons (cons fst (append prev rst))
                         acc))))))
  (define (aux fst rst)
    (map (lambda (x) (append fst x)) (iter rst '() '())))
  (let ((len (- (length lst) 3)))
    (define (rec acc n)
      (if (= n len)
          acc
          (rec (append-map (lambda (x)
                             (aux (take x n) (drop x n)))
                           acc)
               (+ n 1))))
    (rec (iter lst '() '()) 1)))

(define (time-it proc)
  (let ((start-time (current-inexact-milliseconds)))
    (let ((x (proc)))
      (values x
              (- (current-inexact-milliseconds) start-time)))))

(define (valid-row row)
  (andmap (lambda (x) (and (> x 0) (<= x 9)))
          row))
(define (check-rows rows)
  (andmap valid-row rows))

(define (accumulate-n op init seqs)
  (define (iter lsts acc)
    (if (null? (car lsts))
        (reverse acc)
        (iter (map cdr lsts)
              (cons (foldr op init (map car lsts))
                    acc))))
  (iter seqs init))
(define (transpose mat)
  (accumulate-n cons '() mat))

(define (all-distinct lst)
  (define (rec lst acc)
    (cond ((null? lst) true)
          ((memq (car lst) acc) false)
          (else (rec (cdr lst)
                     (cons (car lst) acc)))))
  (rec lst '()))
;; fill the first empty space, fill the board until no more blanks or a contradiction is found
(define (sudoku-complete? rows)
  (andmap all-distinct rows)
  (let ((columns (transpose rows)))
    (andmap all-distinct columns))
  ())

