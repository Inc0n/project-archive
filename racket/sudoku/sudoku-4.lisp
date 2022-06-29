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

(defun pos (x-y)
  (+ (second x-y) (* 9 (first x-y))))

(defmethod at-pos ((obj sudoku) x-y)
  (nth (pos x-y) (board obj)))

(defmethod setf-at-pos ((obj sudoku) x-y num)
  (setf (nth (pos x-y) (board obj)) num))

(defmethod at-solution-pos ((obj sudoku) x-y)
  (nth (pos x-y) (solution obj)))

(defmethod set-solution-at-pos ((obj sudoku) x-y num)
  (setf (nth (pos x-y) (solution obj)) num))

;; getting list in corresponding dimension

(defmethod get-row-at ((obj sudoku) row)
  (let ((start-x (* row 9)))
    (subseq (board obj) start-x (+ start-x 9))))

(defmethod get-col-at ((obj sudoku) col)
  (loop for i to 8 collect (at-pos obj (list i col))))

(defmethod print-board ((board sudoku))
  (loop for x to 8
     for row = (get-row-at board x)
     do (format t "~a~%" row)))

(defmacro region-pos (i)
  `(floor ,i 3))

(defun get-all-lanes (i)
  (nth (region-pos i) '((0 1 2) (3 4 5) (6 7 8))))

(defmethod get-region-at ((obj sudoku) x-y &optional check-y)
  (let* ((reg-x-y (loop for i in x-y collect (region-pos i)))
         ;; convert from 2d to 1d co-ordinate
         (reg-x (* (first reg-x-y) 27))
         (reg-y (* (second reg-x-y) 3)))
    ;; swape x & y to "inverse" the output list
    (if check-y
        (setq reg-x-y (reverse reg-x-y)))
    ;; (format t "~d ~d ~%" x-y)
    (loop for i to 8 collect ;with rem-i = (rem i 3)
         (multiple-value-bind (x y) (region-pos i)
           (at-pos obj (list x (+ y reg-x reg-y)))
           ;; (nth (+ (* 9 x) y reg-x reg-y) board)
           ))))

;; check if position is the only blank space

(defun iter (one-two)
  (let ((one (first one-two))
        (two (second one-two)))
    (mapcan (lambda (x) (loop for y in two collect (list x y))) one)))

(defun is-num-valid (board x-y num)
  ;; (mapcar (lambda (x) (count-nums (flatten a x))) b)
  (let ((possible-x-y
         (iter (loop for lane in x-y
                  for get-lane in '(get-row-at get-col-at) collect
                    (let ((lanes (get-all-lanes lane)))
                      (loop for i in lanes
                         when (not (position num
                                             (funcall get-lane board i)))
                         collect i))))))
    (if (position x-y possible-x-y :test #'equal)
        (loop for x-y in (remove x-y possible-x-y :test #'equal)
           when (eq (at-pos board x-y) 0)
           return nil
           finally (return t)))))


(defun get-all-possi-at (board x-y row-l col-l poss-l)
  (let ((conflict (remove-duplicates
                   (flatten
                    row-l
                    col-l
                    (get-region-at board x-y)))))
    (remove-if
     (lambda (possi) (position possi conflict)) poss-l)))


(defmethod solve-board-at ((obj sudoku) x-y row-list col-list)
  (let ((num (at-pos obj x-y)))
    (cond
      ((> num 0) num)
      (t
       (setq num (get-all-possi-at obj x-y
                                   row-list col-list
                                   (loop for i from 1 to 9 collect i)))
       (if (eq (list-length num) 1)
           (first num)
           (loop for possi in num
              when (is-num-valid obj x-y possi)
              return possi
              finally (return num)))))))


(defun solve-at (board x y)
  (solve-board-at board (list x y)
                  (get-row-at board x)
                  (get-col-at board y)))


(defmethod solve ((obj sudoku))
  (loop for x to 8
     for row = (get-row-at obj x) do
       (loop for y to 8
          for col = (get-col-at obj y) do
            (let* ((x-y (list x y))
                   (num (solve-board-at obj x-y
                                        row col)))
              (set-solution-at-pos obj x-y num)
              (if (numberp num)
                  (setf-at-pos obj x-y num)))))

  (format t "~a~%" (solution obj))
  (print-board obj)
  (if (equal (read-line) "")
      (solve obj)))

(defparameter sudoku-1
  (make-instance
   'sudoku
   ;; hard level
   ;; :board '(1 0 0 0 8 0 4 0 0
   ;;          7 0 0 4 9 0 0 2 0
   ;;          0 0 5 0 0 0 3 0 0
   ;;          4 0 6 0 3 0 0 0 0
   ;;          3 9 0 0 0 0 0 5 2
   ;;          0 0 0 0 1 0 6 0 4
   ;;          0 0 9 0 0 0 2 0 0
   ;;          0 5 0 0 6 8 0 0 1
   ;;          0 0 3 0 2 0 0 0 5)))

    ;; medium level
    :board '(9 0 5 0 0 0 3 0 0
            0 8 0 3 9 0 0 0 0
            4 3 6 0 0 0 0 7 1
            0 1 0 0 3 2 0 0 0
            0 0 9 0 0 0 5 0 0
            0 0 0 9 4 0 0 1 0
            3 6 0 0 0 0 4 9 5
            0 0 0 0 8 9 0 3 0
            0 0 7 0 0 0 8 0 2)))