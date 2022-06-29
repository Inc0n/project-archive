;; Given a list of words containing only lower-case letters, return a list of characters that appear in all the words, with multiplicity. For instance, given the words BELLA, LABEL and ROLLER, the characters E, L and L appear in all three words.

(define word-list '("bella" "label" "roller"))

(define (run word-list)
  (let ((lst (map (lambda (x)
                    (list->set (string->list x)))
                  word-list)))
    (foldr set-intersect (car lst) (cdr lst))))