
(require "2.67-decode-tree.rkt")

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define (element-in-set? x set)
  (define (aux set)
    (if (leaf? set)
        (eq? (symbol-leaf set) x)
        (if (aux (right-branch set))
            #t
            (aux (left-branch set)))))
  (aux set))

(define (encode-symbol sym branch)
  (define (rec-encode branch)
    (if (leaf? branch)
        (if (element-in-set? sym branch)
            '()
            (error "symbol cannot be encoded" sym))
        (let ((right-node (right-branch branch)))
          (if (element-in-set? sym right-node)
              (cons 1 (rec-encode right-node))
              (cons 0 (rec-encode (left-branch branch)))))))
  (rec-encode branch))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))