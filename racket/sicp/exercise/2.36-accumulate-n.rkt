
(require "../2-Building-Abstractions-with-Data/2.2.3-sequence-interface.rkt")

(define nil '())
(define (accumulate-n op init seqs)
  (define (iter lsts acc)
    (if (null? (car lsts))
        (reverse acc)
        (iter (map cdr lsts)
              (cons (foldr op init (map car lsts))
                    acc))))
  (iter seqs init))