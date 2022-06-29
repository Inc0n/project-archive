
;; my ver. assume pair list encountered had their first item as atom
;;
(define (rec-equal? lst1 lst2)
  (define (rec lst1 lst2 acc)
    (if (and acc (pair? lst1) (pair? lst2))
        (let ((item-1 (car lst1))
              (item-2 (car lst2)))
          (let ((pair?-1 (pair? item-1))
                (pair?-2 (pair? item-2)))
            (if (and pair?-1 pair?-2)
                (rec (append (cdr item-1) (cdr lst1))
                     (append (cdr item-2) (cdr lst2))
                     (rec (car list1) (car list2) #t))
                (rec (cdr lst1)
                     (cdr lst2)
                     (equal? item-1 item-2)))))
        acc))
  (rec lst1 lst2 #t))

(rec-equal? '(this is a list) '(this (is a) list))