
(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (* sub-tree sub-tree)))
       tree))

(define (square-list items)
  (define (square x)
    (* x x))
  (if (null? items)
      nil
      (cons (square (car items))
            (square-list (cdr items)))))