
;; Examples of such structures include B-trees and red-black trees.
;; There is a large literature on data structures devoted to this problem.
;; See Cormen et al. 1990.
(require (submod "../2-Building-Abstractions-with-Data/2.3.3-example-representing-sets.rkt" binary-tree-set))


(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1
                     (right-branch tree))))))

(define (tree->list-2 tree)
  (define (iter tree result-list)
    (if (null? tree)
        result-list
        (iter (left-branch tree)
              (cons (entry tree)
                    (iter
                     (right-branch tree)
                     result-list)))))
  (iter tree '()))

(define (time times proc tree)
  (define (timed-proc proc)
    (let ((start-time (current-inexact-milliseconds)))
      (proc)
      (- (current-inexact-milliseconds) start-time)))
  (define (output proc n)
    (let ((times (for/list ((_ (in-range 0 n))) (proc))))
      (let ((total (foldr + 0.0 times)))
        (printf "time: ~a~%average: ~a~%"
                times (/ total n)))
      ))
  (let ((proc (lambda ()
                (timed-proc (lambda () (proc tree))))))
    (output proc times)))

(define fig2-16-1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
(define fig2-16-2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
(define fig2-16-3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))