
;; The Rat Eats The Cheese
;; May 14, 2019

;; A square maze contains cheese wedges on some of its squares:

;; ·  ·  · 🧀  ·
;; ·  ·  ·  ·  ·
;; · 🧀  · 🧀 🧀
;; ·  · 🧀  · 🧀
;; ·  ·  ·  · ·

;; [ Did you know there is a cheese-wedge character in Unicode? I didn’t. The center-dot is & # 183 ;, the cheese is & # 129472 ;, and I had to sprinkle in a few & thinsp ; characters to line things up. And of course to type those I had to add extra spaces, because WordPress is aggressive about turning them into characters. ]

;; A rat, starting at the lower left-hand corner of the maze, can move only up or right. What is the maximum amount of cheese the rat can eat?

;; Your task is to write a program to determine how much cheese the rat can eat. When you are finished, you are welcome to read or run a suggested solution, or to post your own solution or discuss the exercise in the comments below.

;;

;; because of the nature of list structure, my maze list is reversed up side down.


(define (up-side-down lst)
  (foldl cons '() lst))

(define maze
  '((0 0 0 0 0)
    (0 0 1 0 1)
    (0 1 0 1 1)
    (0 0 0 0 0)
    (0 0 0 1 0)))

(define (effect-down maze) (car maze))
(define (move-down maze) (cdr maze))

(define (effect-right maze)
  (map car maze))
(define (move-right maze)
  (map cdr maze))

(define (sum lst)
  (count (lambda (x) (= x 1)) lst))

;;

(define (evaluate maze)
  (let ((up-list (effect-down maze))
        (left-list (effect-right maze)))
    (let ((up-cheese-count (sum up-list))
          (left-cheese-count (sum left-list)))
      (if (> up-cheese-count left-cheese-count)
          move-right
          move-down))))

(define (run maze)
  (define (aux maze path count)
    (for-each (lambda (x) (printf "~a~%" x)) maze)
    (if (or (null? maze) (null? (car maze)))
        count
        (let ((path-proc (evaluate maze))
              (cheese (caar maze)))
          (printf "~%")
          (aux (path-proc maze)
               (cons path-proc path)
               (if (= cheese 1)
                   (+ count 1)
                   count)))))
  (aux maze '() 0))

;; for this maze max cheese count is 3
;; but my algorithm only choose the path that would able to get
;; the most cheese

(define (goto-cheese up-rest-lst left-rest-lst)
  (let ((up-fst-cheese-pos (position 1 up-rest-lst))
        (left-fst-cheese-pos (position 1 left-rest-lst)))
    (if (> up-fst-cheese-pos left-fst-cheese-pos)
        move-right
        move-down)))

(define (position item lst)
  (define (aux lst pos)
    (cond ((null? lst) -1)
          ((equal? item (car lst)) pos)
          (else (aux (cdr lst) (+ pos 1)))))
  (aux lst 0))

(define (evaluate maze)
  (let ((up-list (effect-down maze))
        (left-list (effect-right maze)))
    (let ((up-cheese-count (sum up-list))
          (left-cheese-count (sum left-list)))
      (cond ((= up-cheese-count left-cheese-count)
             (goto-cheese (cdr up-list) (cdr left-list)))
            ((> up-cheese-count left-cheese-count)
             move-right)
            (else move-down)))))