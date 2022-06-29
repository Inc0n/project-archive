
#|
         '((1 2) 3 4)
             / | \
            /  |  \
        '(1 2) 3   4
          / \
         /   \
        1     2
     Fig. list structure viewed as a binary tree


To implement count-leaves, recall the recursive plan for computing
length:
• length of a list x is 1 plus length of the cdr of x
• length of the empty list is 0. count-leaves is similar.
  The value for the empty list is the same: ;
• count-leaves of the empty list is 0

|#

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define (count-leaves x)
  (define (iter x acc)
    (cond ((null? x) acc)
          ((not (pair? x)) (+ acc 1))
          (else (iter (cdr x)
                      (iter (car x) acc)))))
  (iter x 0))