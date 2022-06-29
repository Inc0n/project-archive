;;
;; Sudoku Solver
;;
;; Licensed under
;; Sun. Jan 24
;;;
;; In this version, code is tidied a little
;; And x -> row & y -> col, compared to y was -> row & x -> col-col
;; more logically and easier to understand for me
;; but still not working, it's validating the possibility needs improvement
;;;

;;;
;; general helper function
;;;

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


(defun group-by (l n)
  (labels
      ((group-by-aux (l n A)
         (if (null l)
             A
             (group-by-aux (subseq l n) n (append A (list (subseq l 0 n)))))))
    (group-by-aux l n '())))


;;;
;; sudoku class
;;;

(defclass sudoku ()
  ((board
    :initarg :board
    :accessor board
    :initform '())
   (solution
    :accessor solution
    :initform '())))

(defmethod initialize-instance :after ((obj sudoku) &key)
  (let ((board (board obj)))
    (setf (slot-value obj 'solution)
          (copy-list board)
          )))

;; position handling

(defun pos (x y)
  (+ y (* 9 x)))

(defmethod at-pos ((obj sudoku) x y)
  (nth (pos x y) (board obj)))

(defmethod at-solution-pos ((obj sudoku) x y)
  (nth (pos x y) (solution obj)))

(defmethod setf-at-pos ((obj sudoku) x y num)
  (setf (nth (pos x y) (board obj)) num))

(defmethod set-solution-at-pos ((obj sudoku) x y num)
  (setf (nth (pos x y) (solution obj)) num))

;; getting list in corresponding dimension

(defmethod get-row-at ((obj sudoku) row)
  (let ((start-x (* row 9)))
    (subseq (board obj) start-x (+ start-x 9))))

(defmethod get-col-at ((obj sudoku) col)
  (loop for i to 8 collect (at-pos obj i col)))

(defmethod print-board ((board sudoku))
  (loop for x to 8
     for row = (get-row-at board x)
     do (format t "~a~%" row)))

(defmacro region-pos (i)
  `(floor ,i 3))

(defun get-other-lanes (i)
  (nth (region-pos i) '((0 1 2) (3 4 5) (6 7 8))))

(defmethod get-region-at ((obj sudoku) x y &optional check-y)
  (let ((reg-x (region-pos x))
        (reg-y (region-pos y)))
    (if check-y
        ;; swape x & y to "inverse" the output list
        (psetf reg-x reg-y
               reg-y reg-x))
    (setq reg-x (* reg-x 27))      ; adjust x from 2d to 1d co-ordinate
    (setq reg-y (* reg-y 3))     ; now reg-y it * by 27 in total
    ;; (format t "~d ~d ~%" x y)
    (loop for i to 8 collect
         (multiple-value-bind (x y) (region-pos i)
           (at-pos obj x (+ y reg-x reg-y))
           ;; (nth (+ (* 9 x) y reg-x reg-y) board)
           ))))

;; check if position is the only blank space

(defun is-only-space-at (board x y &optional check-y)
  (let ((reg-list (get-region-at board x y check-y))
        (reg-i (rem (if check-y y x) 3)))
    ;; group-by-3 group 1d list into a 3x3 list
    (format t "~a~%" (nth reg-i (group-by reg-list 3)))
    (eq 1
        (count 0
               (nth reg-i
                    (group-by reg-list 3))))))

(defun is-only-space-at (board x y &optional check-y)
  (let ((reg-list (get-region-at board x y check-y))
        (reg-i (rem (if check-y y x) 3)))
    ;; group-by-3 group 1d list into a 3x3 list
    (format t "~a~%" (nth reg-i (group-by reg-list 3)))
    (eq 1
        (count 0
               (nth reg-i
                    (group-by reg-list 3))))))
(defun iter (one two)
  (mapcan (lambda (x) (mapcar (lambda (y) (list x y)) two)) one))

(defun is-possi-valid (board x y possi)
  ;; (mapcar (lambda (x) (count-nums (flatten a x))) b)
  (let* ((rows (get-other-lanes x))
         (cols (get-other-lanes y))
         (row-conflicts
          (loop for i in rows collect (get-row-at board i)))
         (col-conflicts
          (loop for i in cols collect (get-col-at board i)))
         (row-ok
           (loop for i to 2
              when (not (position possi (nth i row-conflicts)))
              collect (nth i rows)))
         (col-ok
          (loop for i to 2
             when (not (position possi (nth i col-conflicts)))
             collect (nth i cols))))
    (format t "~a ~a ~a~%" possi row-ok col-ok)
    (remove-if #'> (remove (list x y) (iter row-ok col-ok)) :test 0)))

(defun get-all-possi-at (board x y row-l col-l poss-l)
  (let ((conflict (remove-duplicates
                   (flatten
                    row-l
                    col-l
                    (get-region-at board x y)))))
    (remove-if
     (lambda (possi) (position possi conflict)) poss-l)))


(defmethod solve-board-at ((obj sudoku) x y row-list col-list)
  (let ((num (at-solution-pos obj x y)))
    (cond
      ((eq num 0)
       (let ((nums (get-all-possi-at obj x y
                                     row-list col-list
                                     (loop for i from 1 to 9 collect i))))
         (format t "~a~%" nums)
         (loop for possi in nums
            when (is-possi-valid obj x y possi)
            return possi)))
      ((listp num)
       (loop for possi in num
          when (is-possi-valid obj x y possi)
          return possi))
       ;; (format t "~a ~a ~%" (pos-at solution x y) num)
      (T num))))


(defun solve-at (board x y)
  (solve-board-at board x y
                  (get-row-at board x)
                  (get-col-at board y)))

(defmethod solve ((obj sudoku))
  (loop for x to 8
     for row = (get-row-at obj x) do
       (loop for y to 8
          for col = (get-col-at obj y) do
            (let ((num (solve-board-at obj x y
                                       row col)))
              (set-solution-at-pos obj x y num)
              ;; (setf (nth (pos x y) solution) num)
              (if (numberp num)
                  (setf-at-pos obj x y num)))))

  (format t "~a~%" (solution obj))
  (print-board obj)
  (if (equal (read-line) "")
      (solve obj)))

(defparameter sudoku-1
  (make-instance
   'sudoku
   :board '(0 0 0 1 4 9 7 3 5
            5 0 0 0 0 0 1 0 0
            0 1 0 2 0 8 4 0 9
            0 0 7 0 0 5 0 0 0
            3 4 0 7 0 1 0 2 6
            0 0 0 4 0 0 5 0 0
            1 0 9 8 0 2 0 5 0
            0 0 2 0 0 0 0 8 0
            4 5 8 6 1 3 0 0 0)))
