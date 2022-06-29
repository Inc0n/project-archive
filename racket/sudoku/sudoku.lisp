;
; Sudoku Solver
;
; Licensed under
; Sun. Jan 20
;

(defparameter sudoku
  '(0 0 0 1 4 9 7 3 5
    5 0 0 0 0 0 1 0 0
    0 1 0 2 0 8 4 0 9
    0 0 7 0 0 5 0 0 0
    3 4 0 7 0 1 0 2 6
    0 0 0 4 0 0 5 0 0
    1 0 9 8 0 2 0 5 0
    0 0 2 0 0 0 0 8 0
    4 5 8 6 1 3 0 0 0))


; sudoku logic functions
(defun get-reg-num (x y)
  (let ((x-reg (ceiling (+ 1 x) 3))
        (y-reg (ceiling (+ 1 y) 3)))
    (* x-reg y-reg)))

(defun get-region-num-list (board region)
  (let ((reg-x (* (- (rem region 3) 1) 3))
        (reg-y (* (floor region 3) 3 9)))
    (loop for i to 8 collect
         (multiple-value-bind (x y)
             (floor i 3)
           (nth (+ (* 9 x) reg-x reg-y y) board)))))

(defun get-row-num-list (board row)
  (loop for i to 8 collect (nth (+ i (* row 9)) board)))

(defun get-col-num-list (board column)
  (loop for i to 8 collect (nth (+ column (* i 9)) board)))

;; (defun get-other-rows (y)
;;   (let ((region-list '((0 1 2) (3 4 5) (6 7 8)))
;;         (reg-y (* (- (rem region 3) 1) 3)))
;;     (remove-if (lambda (z) T) (nth reg-x region-list) :start y :end (+ y 1))))

(defun get-other-lanes (x)
  (let ((region-list '((0 1 2) (3 4 5) (6 7 8)))
        (reg-x (floor x 3)))
    (remove-if (lambda (z) (eq z x)) (nth reg-x region-list))))

(defun is-empty (l)
  (mapcan #'(lambda (x) (null x)) l))

(defun flatten (&rest l)
  (labels
      ((flatten-aux (l a)
         (if (null l)
             a
             (let ((x (first l)))
               (if (listp x)
                   (setq a (flatten-aux x a))
                   (push x a))
               (flatten-aux (rest l) a)))))
    (reverse (flatten-aux l '()))))

(defun remove-dup (&rest l)
  ;; (format t "~a~%" l)
  (remove-duplicates (flatten l)))


(defun count-nums (l)
  (count-nums-aux l '()))

(defun count-nums-aux (l nums)
  (if (not l)
      ; keep only the number that has a count of 3
      (mapcan (lambda (x) (if (= (second x) 3) (list (first x)))) nums)
      (let* ((idx (first l))
             (num (find-if (lambda (x) (if (eq (first x) idx) x)) nums)))
        (if num
            (incf (second num))       ; num exist in num-count-list
            (push (list idx 1) nums)) ; add it if not encountered yet
        (count-nums-aux (rest l) nums))))

; sudoku solver functions
(defun get-possible-at (board x y row-list col-list reg-num)
  (let ((num (nth (+ x (* y 9)) board)))
    (if (eq num 0)
        (let ((nums (loop for i from 1 to 9 collect i))
              (other-rows (get-other-lanes y))
              (other-cols (get-other-lanes x))
              (row-result)
              (col-result)
              (conflict))
          (setq conflict
                (remove-dup
                            row-list
                            col-list
                            (get-region-num-list board reg-num)))
          (setq nums
                (remove-if (lambda (x) (member x conflict)) nums))
          (setq row-result (count-nums
                            (flatten nums
                                     (loop for i in other-rows collect (get-row-num-list board i)))))

          (format t "~a~%" row-result)
          (setq col-result (count-nums
                            (flatten nums
                                     (loop for i in other-cols collect (get-col-num-list board i)))))
          (format t "~a~%" col-result)

          (remove-dup nums row-result col-result)
          ;; (if (eq (length nums) 1)
          ;;     (first nums)
              )
        num)))

(defmacro get-possible-macro (board x y row-list)
  `(let* ((col-list (get-col-num-list ,board ,x))
          (reg-num (get-reg-num ,x ,y)))
    (get-possible-at ,board ,x ,y ,row-list col-list reg-num)))


(defun possible-at (board x y)
  (get-possible-macro board x y (get-row-num-list board y)))

(defun solve (board)
  (loop for y from 0 to 8
     collect
       (let ((row-list (get-row-num-list board y)))
         (loop for x from 0 to 8
            collect
              (get-possible-macro board x y row-list)))))

