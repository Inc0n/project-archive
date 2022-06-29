
(define lst '(0 0 0 1 1 1 1 2 2 3 3 3 4 4 4))

;; (define (search lst)
;;   (let aux ((lst lst) (acc '(())))
;;     (cond ((eq? (car lst) (car most))
;;            (let ((most (car acc)))
;;              (aux (cdr lst) (cons (cons (car most)
;;                                         (add1 (cdr most)))
;;                                   (cdr acc)))))
;;           (else
;;            (aux (cdr lst) (cons (cons (car lst) 1) acc))))))

(define (search lst)
  (let aux ((lst lst) (most '()) (crnt '()))
    (cond ((null? lst)
           (if (null? most) crnt most))
          ((null? crnt)
           (aux (cdr lst) most (cons (car lst) 1)))
          ((eq? (car lst) (car crnt))
           (aux (cdr lst) most (cons (car lst)
                                     (+ (cdr crnt) 1))))
          (else
           (aux (cdr lst)
                (if (or (null? most) (> (cdr crnt) (cdr most)))
                    crnt
                    most)
                (cons (car lst) 1))))))