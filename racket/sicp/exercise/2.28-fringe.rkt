#|| Exercise-2.28-fringe ||#

;;; Write a procedure fringe that takes as argument a tree
;;; (represented as a list) and returns a list whose elements
;;; are all the leaves of the tree arranged in left-to-right
;;; order. For example,

(define x (list (list 1 2) (list 3 4))) ; => ((1 2) (3 4))
(fringe x)                              ; => (1 2 3 4)
(fringe (list x x))                     ; => (1 2 3 4 1 2 3 4)

;;;; definition

(define (fringe lst)
  (define (iter lst acc)
    (if (null? lst)
        acc
        (let ((x (car lst)))
          (if (pair? x)
              (iter (cdr lst) (iter x acc))
              (iter (cdr lst) (cons x acc))))))
  (reverse (iter lst '())))

#|| side notes
obviously, this is like flatten in common lisp, so *shrug.
but however, with these multi-encounter of these familiar concepts
it should not be made out with much difficulty, of both the structural
similarities and the accumulation difference in this exercise and
the previous one.

  * nested recursion
  * (cons) or direct return value of acc into the next

||#
#|| 2019-05-07 23:28 - 23:41 ||#