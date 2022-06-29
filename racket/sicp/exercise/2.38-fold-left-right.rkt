
(require "../2-Building-Abstractions-with-Data/2.2.3-sequence-interface.rkt")

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (fold-right op init seq)
  (accumulate op init seq))

;; What are the values of
(fold-right / 1 (list 1 2 3))           ; 3/2
(fold-left / 1 (list 1 2 3))            ; 1/6
(fold-right list '() (list 1 2 3))      ; '(1 (2 (3 ())))
(fold-left list '() (list 1 2 3))       ; '(((() 1) 2) 3)