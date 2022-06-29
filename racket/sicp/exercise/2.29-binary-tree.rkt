#|| Exercise 2.29-binary-mobile ||#

;;; Intro
;; wait binary-mobile, is it not binary tree.
;; hmm, 'make-branch' ... yh it's got to be binary tree
;

;; A binary mobile consists of two branches, a left branch and
;; a right branch. Each branch is a rod of a certain length,
;; from which hangs either a weight or another binary mobile.
;; We can represent a binary mobile using compound data by
;; constructing it from two branches (for example, using list):
(define (make-mobile left right)
  (list left right))

;; A branch is constructed from a length (which must be a number)
;; together with a structure, which may be either a number
;; (representing a simple weight) or another mobile:
(define (make-branch length structure)
  (list length structure))

;; a.
;; Write the corresponding selectors left-branch and right-branch,
;; which return the branches of a mobile, and branch-length and
;; branch-structure, which return the components of a branch.

(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cadr mobile))

(define (branch-length branch) (car branch))
(define (branch-structure branch) (cadr branch))

;; b. Using your selectors, define a procedure total-weight that returns the total weight of a mobile.

;;;;; eh, what is weight? Is it the total length of sub-branches?
;;;;;

(define (total-weight)
  (define (iter mobile acc)
    (if (null? lst)
        acc
        (let ((x (car lst)))
          (if (pair? x)
              (iter (cdr lst) (iter x acc))
              (iter (cdr lst) (cons x acc)))))))

;; c. A mobile is said to be balanced if the torque applied by its top-left branch is equal to that applied by its topright branch (that is, if the length of the left rod multiplied by the weight hanging from that rod is equal to the corresponding product for the right side) and if
;; each of the submobiles hanging off its branches is balanced.
;; Design a predicate that tests whether a binary mobile is balanced.

(define (balanced? mobile)
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    (and (= (branch-length left)
            (branch-length right))
         (= (total-weight left)
            (total-weight right))
         (balanced? left)
         (balanced? right))))

;; d. Suppose we change the representation of mobiles so that the constructors are
(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))
;; How much do you need to change your programs to
;; convert to the new representation?

#|| 2019-05-07 23:44 ||#