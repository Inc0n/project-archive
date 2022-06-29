
(require (submod "../2-Building-Abstractions-with-Data/2.3.3-example-representing-sets.rkt" binary-tree-set))


(define (tree-= tree1 tree2)
  (= (entry tree1) (entry tree2)))
(define (tree-< tree1 tree2)
  (< (entry tree1) (entry tree2)))

(define (union-set tree1 tree2)
  (define (rec tree1 tree2)
    (cond ((null? tree1)
           tree2)
          ((null? tree2)
           tree1)
          ((tree-= tree1 tree2)
           (make-tree (entry tree1)
                      (rec (left-branch tree1)
                           (left-branch tree2))
                      (rec (right-branch tree1)
                           (right-branch tree2))))
          ((tree-< tree1 tree2)
           (make-tree (entry tree2)
                      (rec tree1 (left-branch tree2))
                      (right-branch tree2)))
          (else ;; (tree-> tree1 tree2)
           (make-tree (entry tree1)
                      (rec (left-branch tree1) tree2)
                      (right-branch tree1)))))
  (rec tree1 tree2))

(define (intersection-set tree1 tree2)
  (define (rec tree1 tree2)
    (cond ((or (null? tree1) (null? tree2)) '())
          ((tree-= tree1 tree2)
           (make-tree (entry tree1)
                      (rec (left-branch tree1)
                           (left-branch tree2))
                      (rec (right-branch tree1)
                           (right-branch tree2))))
          ((tree-< tree1 tree2)
           (rec tree1
                (left-branch tree2)))
          (else ;; (tree-> tree1 tree2)
           (rec (left-branch tree1)
                tree2))))
  (rec tree1 tree2))