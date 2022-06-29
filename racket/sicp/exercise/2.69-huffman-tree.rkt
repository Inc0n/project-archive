
(require "2.67-decode-tree.rkt")

(define (single? x) (null? (cdr x)))

(define (adjoin-set x set)
  (cond ((null? set)
         (list x))
        ((< (weight x) (weight (car set)))
         (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

;;

(define (successive-merge leaf-set)
  (define (iter prev set acc)
    (let ((i (car set)))
      (if (< (weight prev) (weight i))
          (if (null? acc)
              (adjoin-set (make-code-tree prev i)
                          (cdr set))
              (append (reverse (cdr acc))
                      (adjoin-set (make-code-tree prev
                                                  (car acc))
                                  set)))
          (iter i (cdr set) (cons prev acc)))))
  (if (single? leaf-set)
      (car leaf-set)
      (let ((first (car leaf-set))
            (rest (cdr leaf-set)))
        (successive-merge (iter first rest '())))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(generate-huffman-tree
 '((A 8) (B 3) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1)))