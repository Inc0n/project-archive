
(define (huffman-tree-n n proc)
  (proc n
        (generate-huffman-tree
         (for/list ((i (in-range 1 (+ 1 n)))) (list i i)))))

(generate-huffman-tree
 '((A 8) (B 3) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1)))

;; the most frequent bit
(huffman-tree-n 5 (lambda (n tree) (encode-symbol n tree)))
'(1 1)

(huffman-tree-n 10 (lambda (n tree) (encode-symbol n tree)))
'(0 0)
;;

;; the least frequent bit
(huffman-tree-n 5 (lambda (n tree) (encode-symbol 1 tree)))
'(0 0 0)

(huffman-tree-n 10 (lambda (n tree) (encode-symbol 1 tree)))
'(0 1 0 0 0)
;;
