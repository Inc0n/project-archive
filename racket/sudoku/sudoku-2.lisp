;;;
;; Sudoku Solver
;;;
;; Licensed under
;; Sun. Jan 20
;;;
;; this sudoku solver implements a faulted algorithm it removes all already exist
;; numbers in rows and columns, and in the same region. However, it fails to
;; compute the correct solution because it doesn't consider the numbers from both
;; horizontal region and vertical region together, it utilizes them seperately to
;; eliminate the possiblity
;;;

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

(defparameter solution
  (copy-list sudoku))

;;; generic helper function

(defun flatten (&rest l)
  ;; (format t "~a~%" l)
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


(defun remove-nth (i l)
  (append
   (subseq l 0 i)
   (subseq l (+ 1 i))
   ))


(defun remove-dup (&rest l)
  ;; (format t "~a~%" l)
  (remove-duplicates (flatten l)))


(defun group-by (l n)
  (labels
      ((group-by-aux (l n A)
         (if (null l)
             A
             (group-by-aux (subseq l n) n (append A (list (subseq l 0 n)))))))
    (group-by-aux l n '()))
  )

(defun print-board (board)
  (loop for y to 8 collect
       (loop for x to 8 collect
            (pos-at board x y))))

;;; sudoku logic functions

(defun get-reg-pos-from (x y)
  (values
   (floor x 3)
   (floor y 3)))

(defun get-regional-num-list (board x y &optional check-y)
  (multiple-value-bind
        (reg-x reg-y)
      (get-reg-pos-from x y)
    (if check-y
        (psetf reg-x reg-y
               reg-y reg-x))
    (setq reg-x (* reg-x 3))  ; adjust x from 2d to 1d co-ordinate
    (setq reg-y (* reg-y 27)) ; now reg-y it * by 27 in total
    ;; (format t "~d ~d ~%" reg-x reg-y)
    (loop for i to 8 collect
         (multiple-value-bind (x y) (floor i 3)
           ;; (format t "~d ~d ~%" x y)
           (nth (+ (* 9 x) y reg-x reg-y) board)
           ))))

(defun get-row-num-list (board row)
  (loop for i to 8 collect (nth (+ i (* row 9)) board)))

(defun get-col-num-list (board column)
  (loop for i to 8 collect (nth (+ column (* i 9)) board)))

(defun get-other-lanes (i)
  (let ((region-list '((0 1 2) (3 4 5) (6 7 8))))
    (multiple-value-bind (reg-i i) (floor i 3)
      (remove-nth i (nth reg-i region-list)))))


;;; specialized helper functions

(defun count-nums (l)
  (labels
      ((count-nums-aux (l nums)
         (if (not l)
             ; keep only the number that has a count of 3
             (mapcan (lambda (x) (if (= (second x) 3) (list (first x)))) nums)
             (let* ((num (first l))
                    (count-l (find-if (lambda (x) (if (eq (first x) num) x)) nums)))
               (if count-l
                   (incf (second count-l))   ; increment count by 1
                   (push (list num 1) nums)) ; add it if it's not in list
               (count-nums-aux (rest l) nums)))))
   (count-nums-aux l '())))


(defun is-only-space-at (board x y &optional (check-x T))
  ;
  ; check-x - default T, false means check y
  ;
  (let ((reg-list (get-regional-num-list board x y check-x)))
    (let ((reg-i (rem
                  (if check-x x y) 3)))
      ; convert num 1d list to a 3x3 list
      (setf reg-list
            (nth reg-i
                 (group-by reg-list 3)))
      (eq 1 (count 0 reg-list)))
      ))

(defun vali-poss-nums (board x y nums &rest b)
        ;; (mapcar (lambda (x) (count-nums (flatten a x))) b)
  ;; (format t "~a ~a ~%" nums b)
  (let ((l (loop for x in b collect (count-nums (flatten nums x)))))
    (let ((row (first l))
          (col (second l)))
      ;; (format t "~a ~a ~%" nums l)
      (cond
        ((and (= (length row) 1) (is-only-space-at board x y T))
         (first row))
        ((and (= (length col) 1) (is-only-space-at board x y nil))
         (first col))
        ((= (length nums) 1)
         (first nums))
        (T nums)))
    ))

(defun pos (x y)
  (+ x (* 9 y)))

(defun pos-at (board x y)
  (nth (+ x (* y 9)) board))


;;; sudoku solver functions

(defun get-all-poss-nums-at (board x y row-l col-l poss-l)
  (let ((other-rows (get-other-lanes x))
        (other-cols (get-other-lanes y))
        (conflict (remove-dup
                   row-l
                   col-l
                   (get-regional-num-list board x y))))
    (setq poss-l
          (remove-if
           (lambda (x) (position x conflict)) poss-l))
    (vali-poss-nums board x y poss-l
                    (loop for i in other-rows collect (get-row-num-list board i))
                    (loop for i in other-cols collect (get-col-num-list board i)))))

(defun get-possible-at (board x y row-list col-list)
  (let ((num (pos-at solution x y)))
    (cond
      ((eq num 0)
       (get-all-poss-nums-at board x y row-list col-list
                             (loop for i from 1 to 9 collect i)))
      ((listp num)
       ;; (format t "~a ~a ~%" (pos-at solution x y) num)
       (get-all-poss-nums-at board x y row-list col-list
                             (pos-at solution x y)))
      (T num))))

(defmacro get-possible-macro (board x y row-list)
  `(let ((col-list (get-col-num-list ,board ,x)))
    (get-possible-at ,board ,x ,y ,row-list col-list)))


(defun possible-at (board x y)
  (get-possible-macro board x y (get-row-num-list board y)))

(defun solve (board)
  (let ((new-board
         (loop for y from 0 to 8
            collect
              (let ((row-list (get-row-num-list board y)))
                (loop for x from 0 to 8
                   collect
                     (let ((num (get-possible-macro board x y row-list)))
                       (setf (nth (pos x y) solution) num)
                       (if (numberp num)
                           num
                           0
                           )))))))
    (format t "~a~%~a~%" solution new-board)
    (if (equal (read-line) "")
        (solve (flatten new-board))
        (loop for i in new-board do (format t "~a~%" i)))))

