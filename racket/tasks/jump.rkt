;; [2, 0, 3, 5, 0, 0, 3, 0, 0, 3, 1, 0]

(define vec '(2 0 3 5 0 0 3 0 0 3 1 0))

(define (filter-set-cdr x set test-proc)
  (let iter ((set set))
    (cond ((null? set) '())
          ((test-proc x (car set))
           (map car set))
          (else (iter (cdr set))))))

(define (my-for/list lst f stop? test?)
  (let rec ((i 0) (lst lst))
    (if (null? lst)
        '()
        (let ((x (car lst)))
          (cond ((stop? i x) '())
                ((test? i x)
                 (cons (f i x) (rec (+ i 1) (cdr lst))))
                (else (rec (+ i 1) (cdr lst))))))))

(define (new-set lst (init '()))
  (foldr adjoin-set init lst))

(define puzzle '(2 0 3 5 0 0 3 0 0 3 1 0))

(define (jump! puzzle)
  (define len (length puzzle))
  (define (parse-puzzle vec)
    (define enumerated
      (my-for/list vec
                   cons
                   (lambda x #f)
                   (lambda (i x) (not (zero? x)))))
    (define sorted (sort (cdr enumerated) > #:key cdr))
    (map (lambda (x)
           (let ((start (car x))
                 (x (cdr x)))
             (let ((range (+ start x)))
               (cons start
                     (new-set
                      (filter-set-cdr x sorted
                                      (lambda (x y)
                                        (> x (cdr y))))
                      (new-set
                       (my-for/list enumerated
                                    (lambda (i x) (car x))
                                    (lambda (i x) (> (car x) range))
                                    (lambda (i x) (> (car x) start)))))))))
         enumerated))

  (define available-moves (parse-puzzle puzzle))
  (define (goal? x)
    (>= (+ x (list-ref puzzle x)) len))
  (define (get-moves-at i visited)
    (assoc i available-moves))
  (define (next-moves state)
    (let ((posn (first state))
          (route (second state)))
      (let ((moves (get-moves-at posn route)))
        (if moves
            (for/list ((i (cdr moves)))
              (list i (cons posn route)))
            '()))))
  (define (tree-search states)
    (printf "~a~%" states)
    (if (null? states)
        '()
        (cond ((goal? (first (first states)))
               (first states))
              ((null? (assoc (first (first states))
                             available-moves))
               (tree-search (rest states)))
              (else (tree-search
                     (append (rest states)
                             (next-moves (first states))))))))
  (tree-search (list (list 0 '()))))
