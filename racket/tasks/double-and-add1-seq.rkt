
(define (A a b)
  (values (* 2 a) (+ 1 b)))

(define (B a b)
  (values (+ 1 a) (* 2 b)))

(define (run a b)
  (define (aux a b f)
    (printf "~a ~a~%" a b)
    (if (= a b)
        (list a b)
        (let-values (((a b) (f a b)))
          (cond ((= a b) (list a b))
                ((= (/ b a) 4) (aux a b A))
                ((= (abs (- b a)) (+ a 1))
                 (aux a b B))
                ((let-values (((x y) (quotient/remainder b a)))
                   (and (= x 3) (= (+ y 2) a)))
                 (aux a b A))
                ((let-values (((x y) (quotient/remainder b a)))
                   (= x y 2))
                 (aux a b B))
                ((= (abs (- b a)) (- a 1))
                 (aux a b A))
                (else (format "stopped for unknown ~a ~a~%"
                              a b))))))
  (if (< a b)
      (aux a b values)
      (aux b a values)))

(define (1-even&1-odd? a b)
  (eq? (even? a) (not (even? b))))

(define (diff a b)
  (abs (- a b)))

(define (valid-pair x y a b)
  (let ((diff (abs (- x y))))
    (let-values (((smaller bigger) (f x y)))
      (or (zero? diff)
          (< (- smaller 1) diff)
          (= (- smaller 1) diff)))))

(define (run a b)
  (printf "~a ~a~%" a b)
  (let ((x (* a 2))
        (y (+ b 1)))
    (if (= x y)
        (list x y)
        (let ((a (+ a 1))
              (b (* b 2)))
          (if (valid-pair x y a b)
              (run x y)
              (run a b))))))


;;

def dao(x,y): #This is the map A; B is rev(A(rev(x,y)), where rev(x,y)=(y,x) (so pythonically, (x,y)[::-1])
	    return (2*x, y+1)

    ;; def daoByKey(x,y,S): #Reads S as a composition, so daoByKey(x,y,"AABA") returns AABA(x,y)
	;;    for l in S[::-1]:
	;;    	   if l=="A":
	;; 		   x,y=dao(x,y)
	;; 	   else:
	;; 		   y,x=dao(y,x)
	;;    return (x,y)

(define (run-until a b (maxRuns 500))
  (define (aux a b)
    ))


(define (goal? lst)
  (let ((x (car lst))
        (y (cdr lst)))
    (= x y)))

(define (compute-run route start-pair)
  (define (aux lst route acc)
    (if (null? route)
        (reverse acc)
        (let ((next-pair
               (if (eq? (car route) 'a)
                   (act lst)
                   (rev lst))))
          (aux next-pair (cdr route) (cons next-pair acc)))))
  (list route (aux start-pair route '())))

(define (next-moves state)
  (let ((cells (first state))
        (route (second state)))
    (list (list (act cells) (cons 'a route))
          (list (rev cells)(cons 'b route)))))

(define (search a b)
  (define (tree-search states)
   (if (null? states)
       '()
       (if (goal? (first (first states)))
           (cons (first (first states))
                 (compute-run
                  (reverse (second (first states)))
                  (cons a b)))
           (tree-search
            (append (rest states)
                    (next-moves (first states)))))))
  (tree-search (list (list (cons a b) '()))))

(define (count-digits x)
  (let count ((n x) (acc 1))
    (let ((x (quotient n 10)))
      (if (>= x 1)
          (count x (+ acc 1))
          acc))))

(define (search X Y)
  (let step ((seed 0) (ref 0) (x X) (y Y) (diff 0) (trend 0)
             (seq '()))
    (let ((pattern (string->list (number->string seed 2))))
      (cond
        ((= x y) (cons (list X Y) (reverse seq)))
        ((or (> trend 10) (>= ref (length pattern)))
         (step (add1 seed) 0 X Y 0 0 '()))
        (else
         (let* ((pattern-0?
                 (equal? (list-ref pattern ref) #\0))
                (new-x (if pattern-0? (* 2 x) (add1 x)))
                (new-y (if pattern-0? (add1 y) (* 2 y)))
                (new-diff (abs (- new-x new-y)))
                (new-trend (if (> new-diff diff)
                               (add1 trend) 0))
                (new-seq (cons (list new-x new-y) seq)))
           (step seed (add1 ref) new-x new-y
                 new-diff new-trend new-seq)))))))