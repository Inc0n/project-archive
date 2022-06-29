
#lang racket

(module unordered-set racket
  (define (element-of-set? x set)
    (cond ((null? set) false)
          ((equal? x (car set)) true)
          (else (element-of-set? x (cdr set)))))

  (define (adjoin-set x set)
    (if (element-of-set? x set)
        set
        (cons x set)))

  (define (intersection-set set1 set2)
    (cond ((or (null? set1) (null? set2))
           '())
          ((element-of-set? (car set1) set2)
           (cons (car set1) (intersection-set (cdr set1) set2)))
          (else (intersection-set (cdr set1) set2)))))
;; exercise 2.59 union-set
;; exercise 2.60 duplicatable set

(module ordered-set racket
  (define (element-of-set? x set)
    (cond ((null? set) false)
          ((= x (car set)) true)
          ((< x (car set)) false)
          (else (element-of-set? x (cdr set))))))

;; Because the items in this set are now ordered, they could be indexed at
;; the begining and the end of the set, so the number of steps is the same
;; as for the unordered representation. At the same time, searching for
;; items of many different sizes it would stop at a point near the beginning
;; and that other times search would reach near the end. On the average
;; the expected number of steps should be half of the items in the set.
;; Thus, the average number of steps required will be about n = 2. This is
;; still Θ(n) growth, but it does save us, on the average, a factor of 2
;; in number of steps over the previous implementation.

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set (cdr set1)
                                          (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

;; Exercise 2.61 adjoin-set
;; Exercise 2.62 0(n) union-set

;;;;;;;;;
;;
;; Sets as binary trees
;;
;;;;;;;;;
(module binary-tree-set racket
  (provide entry left-branch right-branch make-tree
           empty-tree)
  (define empty-tree '())
  (define empty-tree? null?)
  (define (entry tree) (car tree))
  (define (left-branch tree) (cadr tree))
  (define (right-branch tree) (caddr tree))
  (define (make-tree entry (left empty-tree) (right empty-tree))
    (list entry left right))

  (define (element-of-set? x set)
    (cond ((null? set) false)
          ((= x (entry set)) true)
          ((< x (entry set))
           (element-of-set? x (left-branch set)))
          ((> x (entry set))
           (element-of-set? x (right-branch set)))))
  (define (adjoin-set x set)
    (cond ((null? set) (make-tree x '() '()))
          ((= x (entry set)) set)
          ((< x (entry set))
           (make-tree (entry set)
                      (adjoin-set x (left-branch set))
                      (right-branch set)))
          ((> x (entry set))
           (make-tree (entry set)
                      (left-branch set)
                      (adjoin-set x (right-branch set)))))))

