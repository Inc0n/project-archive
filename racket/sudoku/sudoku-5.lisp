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

;;;
;; sudoku class
;;;
(defparameter *back*
  '(9 0 5 0 0 0 3 0 0
    0 8 0 3 9 0 0 0 0
    4 3 6 0 0 0 0 7 1
    0 1 0 0 3 2 0 0 0
    0 0 9 0 0 0 5 0 0
    0 0 0 9 4 0 0 1 0
    3 6 0 0 0 0 4 9 5
    0 0 0 0 8 9 0 3 0
    0 0 7 0 0 0 8 0 2))
(defparameter *board*
  (copy-list *back*))
(defparameter *solution* (copy-list *board*))
(defparameter *width* 9)
(defparameter *all-nums* (loop :for i from 1 to *width* :collect i))

;; position handling

(defun posn-x (x-y)
  (first x-y))

(defun posn-y (x-y)
  (second x-y))

(defun pos (x-y)
  (+ (posn-x x-y) (* 9 (posn-y x-y))))

(defun at-pos (board x-y)
  (nth (pos x-y) board))

(defun set-at-pos (board x-y num)
  (setf (nth (pos x-y) board) num))

;; getting list in corresponding dimension

(defun get-row-at (board row)
  (let ((start-x (* row 9)))
    (subseq board start-x (+ start-x 9))))

(defun get-col-at (board col)
  (loop for i to 8 collect (at-pos board (list col i))))

(defun print-board (board)
  (loop :for x to 8
     :for row = (get-row-at board x)
     :do (format t "~a~%" row)))

(defmacro region-pos (i)
  `(floor ,i 3))

(defun get-all-lanes (i)
  (nth (region-pos i) '((0 1 2) (3 4 5) (6 7 8))))

(defun get-region-at (board x-y &optional check-y)
   (destructuring-bind (reg-x reg-y)
         ;; convert from 2d to 1d co-ordinate
       (mapcar (lambda (x) (* (region-pos x) 3)) x-y)
    ;; swape x & y to "inverse" the output list
     (if check-y
         (setq reg-x-y (reverse reg-x-y)))
     (loop for i to 8 collect
          (multiple-value-bind (y x) (region-pos i)
            (at-pos board
                    (list (+ x reg-x)
                          (+ y reg-y)))))))

;; check if position is the only blank space

(defun remove-nth (pos list)
  (let ((nth (rem pos 3)))
    (remove-if (lambda (_) t) list :start nth :end (1+ nth))))

(defun list-* (lst1 lst2)
  (mapcan (lambda (x)
            (loop for y in lst2 collect (list x y)))
          lst1))

(defun non-conflict (pos-i fn)
  (loop for i in (get-all-lanes pos-i)
     when (position num
                    (funcall fn board i) :test #'/=)
     collect i))

(defun singlep (lst)
  (and (consp lst) (null (cdr lst))))

(defmacro rec-map (expr &rest lists)
  (let ((args (mapcar #'car lists))
        (srcs (mapcar #'cadr lists)))
    (format t "~a ~a~%" args srcs)
    (labels
        ((parse (lists vars srcs conds)
           (let ((lst (car lists)))
             (when (symbolp (car lst))
               (push (car lists) vars)
               (when (listp (cadr lst))
                 (push (cadr lst) srcs)
                 (when ())))))
         (can (lists params)
           `(mapcan (lambda (,(car params))
                      ,(aux (cdr lists) (cdr params)))
                    ,(car lists)))
        (aux (lists params)
          (if (null lists)
              expr
              `(mapcar (lambda (,(car params))
                         ,(aux (cdr lists) (cdr params)))
                       ,(car lists)))))
     (can srcs args))))

(defun is-num-valid (board x-y num)
  (mapcar (lambda (i j) (cons i j)) '(col row) x-y)
  (let ((possible-x
         (loop for i in (get-all-lanes (posn-x x-y))
            when (position num
                           (get-col-at board i) :test #'/=)
            collect i))
        (possible-y
         (loop for i in (get-all-lanes (posn-y x-y))
            when (position num
                           (get-row-at board i) :test #'/=)
            collect i)))
    (loop for x in possible-x
         )
    (loop for coords in possible-x-y
       when (and (eq (at-pos board coords) 0)
                 (not (equal coords x-y)))
       return nil
       finally (return t)))
  (let ((lst-to-check
         (mapcar (lambda (i) (remove-nth i (get-all-lanes i))) x-y))
        (x (posn-x x-y))
        (y (posn-y x-y)))
    ;; check if surrounded num is filled
    ;; or the num to check if in the surrounding row / column
    (and
     (every (lambda (i)
              (or (/= 0 (at-pos board (list i y)))
                  (position num (get-col-at board i) :test #'=)))
            (first lst-to-check))
     (every (lambda (i)
              (or (/= 0 (at-pos board (list x i)))
                  (position num (get-row-at board i) :test #'=)))
            (second lst-to-check)))))


(defun valid-nums-at (board x-y row-l col-l)
  (let ((conflict (remove-duplicates
                   (append
                    row-l
                    col-l
                    (get-region-at board x-y)))))
    (remove-if
     (lambda (n) (position n conflict)) *all-nums*)))


(defun solve-board-at (board x-y row-list col-list)
  (let ((num (at-pos board x-y)))
    (cond
      ((= num 0)
       (setq num (valid-nums-at board x-y
                                row-list col-list))
       (format t "~a ~a~%" x-y num)
       (if (eq (list-length num) 1)
           (first num)
           (loop :for possi in num
              :when (is-num-valid board x-y possi)
              :return possi
              :finally (return num))))
      (t
       num))))


(defun solve-at (board x y)
  (solve-board-at board (list x y)
                  (get-row-at board y)
                  (get-col-at board x)))


(defun solve (board)
  (loop for y to 8
     for row = (get-row-at board y) do
       (loop for x to 8
          for col = (get-col-at board x) do
            (let* ((x-y (list x y))
                   (num (solve-board-at board x-y
                                        row col)))
              (set-at-pos *solution* x-y num)
              (if (numberp num)
                  (set-at-pos board x-y num)))))

  (format t "~a~%" *solution*)
  (print-board board)
  (if (equal (read-line) "")
      (solve board)))
