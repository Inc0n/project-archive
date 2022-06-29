#|| Exercise 2.27-deep-reverse ||#

;;; Modify your reverse procedure of Exercise 2.18 to produce a
;;; deep-reverse procedure that takes a list as argument and
;;; returns as its value the list with its el ments reversed and
;;; with all sublists deep-reversed as well.
;;; For example

(define x (list (list 1 2) (list 3 4)))
x                                       ; => ((1 2) (3 4))
(reverse x)                             ; => ((3 4) (1 2))
(deep-reverse x)                        ; => ((4 3) (2 1))

(define (deep-reverse lst)
  (define (iter lst acc)
    (if (null? lst)
        acc
        (let ((x (car lst)))
          (if (pair? x)
              (iter (cdr lst) (cons (iter x '())
                                    acc))
              (iter (cdr lst) (cons x acc))))))
  (iter lst '()))
#|| 2019-05-07 23:21:19 ||#