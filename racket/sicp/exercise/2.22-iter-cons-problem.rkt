define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))

#|
Q: Unfortunately, defining square-list this way produces the
   answer list in the reverse order of the one desired. Why?

A: Because items are consed in a reverse order, ie,
   (cons new-item old-accumulated) the new-item are transformed
   from the first item of the list, which always gets pushed to
   the front of the acculatoin, while procedure continues
   to transform items in a proceeding (normal) order
|#

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

#|
Q: This doesn't work either. Explain.

A: This doesn't work because a list (nil) is consed to an atom
   (transformed item)
|#