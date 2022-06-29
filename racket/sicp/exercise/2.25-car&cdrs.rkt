#| Exercise 2.25:

Q: Give combinations of car s and cdr s that will pick 7 from each of the
   following lists:
   '(1 3 (5 7) 9)
   '((7))
   '(1 (2 (3 (4 (5 (6 7))))))

A: (1 3 (5 7) 9)
   => cdr cdr car cdr car

   '((7))
   => car car

   '(1 (2 (3 (4 (5 (6 7))))))
   => cdr cdr cdr cdr cdr cdr cdr car

Extension (myself):
write a program that does all of above
|#

(define (find-7 lst)
  (define (rec lst acc)
   ;; + first edit
   (if (null? lst)
        #f
        (if (pair? lst)
            (or (rec (car lst) (cons 'car acc))
                (rec (cdr lst) (cons 'cdr acc)))
            (if (eq? lst 7)
                (reverse acc)
                #f))))
  (define (iter lst acc)
   ;; + second edit
   (if (not (or lst acc))
        #f
        (if (pair? lst)
            (rec (cdr lst)
                 (or (rec (car lst) (cons 'car acc))
                     (cons 'cdr acc)))
            (if (eq? lst 7)
                (reverse acc)
                #f))))
  (iter lst '()))

#| 2019-05-07 23:13
  as it turns out:

  REPL> (find-7 '(1 (2 (3 (4 (5 (6 7))))))) ;
    => '(cdr car cdr car cdr car cdr car cdr car cdr car)
        ...
  the mistake is cdr of '(1 (2 ...)) is '((2 ...)) not '(2 ...)
|#

