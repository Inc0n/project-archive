(require "../2-Building-Abstractions-with-Data/2.2.3-nested-mappings.rkt")

;; (define (queens)
;;   (define (map-enumerated proc lst)
;;     (map proc lst (enumerate-interval 1 8)))
;;   (define (filter-enumerated pred lst)
;;     (filter pred (map-enumerated list lst)))
;;   (filter-enumerated
;;    (lambda (row col) )
;;    (permutations 8 8)))


(define (queens board-size)
  (define (adjoin-position row k rest-of-queens)
    (cons row rest-of-queens))
  (define empty-board '())
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (safe? k board)
  ;; `queen` is the position of the last queen to be pushed onto the board
  ;; Conveniently, it does not need to change during this procedure
  (let ((queen (car board)))
    ;; As we `cdr` down our board, we need to check "down" and "diagonally"
    ;; Since "down" is always the same, we can just use `queen`
    ;; The right diagonal is just `(- queen 1)` and the left diagonal is
    ;; (+ queen 1). When we call `iter` again the right and left diagonals
    ;; will be incremented and decremented.
    ;; If we make it through the whole list, that means that neither our
    ;; queen nor its diagonals matched a position in the board, so we return
    ;; true.
    (define (iter rest-of-board right-diagonal left-diagonal)
      (cond
       ((null? rest-of-board) #t)
       ((= queen (car rest-of-board)) #f)
       ((= right-diagonal (car rest-of-board)) #f)
       ((= left-diagonal (car rest-of-board)) #f)
       (else
        (iter
         (cdr rest-of-board)
         (- right-diagonal 1)
         (+ left-diagonal 1)))))
    ;; I'm adding -1 because I don't like non-commutative operations
    (iter (cdr board) (- queen 1) (+ queen 1))))