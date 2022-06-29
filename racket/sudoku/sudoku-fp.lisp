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

;; position handling

(defstruct info
  (size     0  :type fixnum)
  (row-lst '() :type list)
  (col-lst '() :type list)
  (reg-lst '() :type list)
  (all-num '() :type list))

(defstruct sudoku
  (board   '() :type list)
  (info  nil :type info))

;; auxiliry
(defun iter (f val init-val n &optional (join-f #'cons))
  (labels
      ((aux (x n)
         (if (= n 0)
             '()
             (funcall join-f (funcall f x) (aux (+ x val) (1- n))))))
    (aux init-val n)))

(defun gen-lst (f-val g-val size &optional (init-val 0) (join-f #'cons))
  (iter (lambda (x)
          (iter (lambda (x) x)
                g-val x size))
        f-val init-val size
        join-f))

;; generation
(defun gen-row-lst (size)
  (gen-lst size 1 size))

(defun gen-col-lst (size)
  (gen-lst 1 size size))

(defun gen-reg-lst (size)
  (let ((reg-size (/ size 3)))
    (iter (lambda (x)
            (iter (lambda (x)
                    (gen-lst size 1 reg-size x #'append))
                  reg-size x 3))
          (* (expt reg-size 2) 3) 0 3
          #'append)))

;; interface
(defun new-sudoku (sudoku-lst)
  (let ((size (round (sqrt (list-length sudoku-lst)))))
    (if (= (rem size 3.0) 0)
        (make-sudoku
         :board sudoku-lst
         :info (make-info :size size
                          :row-lst (gen-row-lst size)
                          :col-lst (gen-col-lst size)
                          :reg-lst (gen-reg-lst size)
                          :all-num (loop for x from 1 to size collect x)))
        (error nil "sudoku list has invalid length, for example must be 9x9"))))

(defmethod sudoku-nth ((sudoku sudoku) x)
  (nth x (sudoku-board sudoku)))

(defun get-lst (board lst x)
  (mapcar (lambda (x) (nth x board)) (nth x lst)))

(defmethod sudoku-row-nth ((sudoku sudoku) idx)
  (get-lst (sudoku-board sudoku) (sudoku-row-lst sudoku) idx))

(defmethod sudoku-col-nth ((sudoku sudoku) idx)
  (get-lst (sudoku-board sudoku) (sudoku-col-lst sudoku) idx))

(defmethod sudoku-reg-nth ((sudoku sudoku) idx)
  (get-lst (sudoku-board sudoku) (sudoku-reg-lst sudoku) idx))

(defmethod print-sudoku ((sudoku sudoku))
  (reduce append (sudoku-row-lst sudoku))
  (loop :for row in (sudoku-row-lst sudoku)
     :do (format t "~a~%" row)))

(defun get-x-at (pos size)
  (rem pos size))

(defun get-y-at (pos size)
  (floor pos size))

(defun get-reg-at (pos size)
  (let ((size/3 (/ size 3)))
    (let ((reg-x (floor (rem pos size) size/3))
          (reg-y (floor pos (* size size/3))))
      (+ reg-x (* size/3 reg-y)))))


(defun remove-from (lst i l)
  (labels
      ((aux (lst n l)
         (cond ((null lst) '())
               ((and (= n 0) (> l 0))
                (aux (cdr lst) n (- l 1)))
               (t
                (cons (car lst) (aux (cdr lst) (- n 1) l))))))
    (aux lst i l)))

(defun remove-every (lst i size)
  (labels
      ((aux (lst n)
         (cond ((null lst) '())
               ((= (rem n size) i)
                (aux (cdr lst) (+ n 1)))
               (t
                (cons (car lst) (aux (cdr lst) (+ n 1)))))))
    (aux lst 0)))

(defmethod get-reg-check-lst ((sudoku sudoku) pos)
  (let ((size (sudoku-size sudoku)))
    (labels
        ((get-reg-x (pos)
           (rem pos (/ size 3)))
         (get-reg-y (pos)
           (rem (floor pos size) (- size 1))))
      (let ((reg-x (get-reg-x pos))
            (reg-y (* (get-reg-y pos) 3))
            (size/3 (/ size 3)))
        (remove-every
         (remove-from
          (sudoku-reg-lst-nth sudoku pos) reg-y size/3)
         reg-x size/3)))))

;; global vars

(defparameter *sudoku*
  (new-sudoku '(9 0 5 0 0 0 3 0 0
                0 8 0 3 9 0 0 0 0
                4 3 6 0 0 0 0 7 1
                0 1 0 0 3 2 0 0 0
                0 0 9 0 0 0 5 0 0
                0 0 0 9 4 0 0 1 0
                3 6 0 0 0 0 4 9 5
                0 0 0 0 8 9 0 3 0
                0 0 7 0 0 0 8 0 2)))
(defparameter *board*
  (copy-list *back*))

(defmethod invalid-num-at-pos? ((sudoku sudoku) pos num)
  ;; (mapcar (lambda (i j) (cons i j)) '(col row) x-y)
  (let ((possible-posns (get-reg-check-lst sudoku pos)))
    (some (lambda (x)
            (cond ((listp x) (member num x :test #'=))
                  ((= x num) t)
                  ((zerop x) nil)
                  (t nil)))
          possible-posns)))

(defmethod singlep ((x list)) (null (cdr x)))

(defmethod solve-at-pos ((sudoku sudoku) pos num)
  (let ((x (sudoku-nth sudoku pos)))
    (cond
      ((listp x)
       (if (singlep x)
           (car x)
           (remove-if #'invalid-num-at-pos? x)))
      ((numberp x)
       (if (zerop x)
           (let ((size (sudoku-size sudoku)))
             (valid-nums-at (sudoku-row-nth sudoku (get-y-at pos size))
                            (sudoku-col-nth sudoku (get-x-at pos size))
                            (sudoku-reg-nth sudoku (get-reg-at pos size))
                            (sudoku-all-num sudoku)))
           x))
      (t (error nil "unexpected: SOLVE-AT-POS")))))

;; check if position is the only blank space

(defun adjoin-set (lst x)
  (labels
      ((iter (set acc)
         (cond ((null set) (reverse (cons x acc)))
               ((= x (car set))
                (append (reverse acc) set))
               ((< x (car set))
                (append (reverse acc) (cons x set)))
               (t (iter (cdr set) (cons (car set) acc))))))
    (iter lst '())))

(defun adjoin-lst (lst x)
  (if (or (listp x) (zerop x))
      lst
      (adjoin-set lst x)))

(defun list->order-set (list)
  (reduce #'adjoin-lst list :initial-value '()))

(defun set-op (set1 set2 =-proc)
  (labels
      ((set-cons (set1 lst)
         (cons (car set1) acc))
       (iter (set1 set2 acc)
         (cond ((null set1) (append (reverse acc) set2))
               ((null set2) (append (reverse acc) set1))
               ((= (car set1) (car set2))
                (iter (cdr set1) (cdr set2) (funcall =-proc set1 acc)))
               ((< (car set1) (car set2))
                (iter (cdr set1) set2 (set-cons set1 acc)))
               (t  ;; (> (car set1) (car set2))
                (iter set1 (cdr set2) (set-cons set2 acc))))))
    (iter set1 set2 '())))

(defun set-union (set1 &rest set2)
  (labels ((set-cons (set1 lst) (cons (car set1) acc)))
    ;; (iter set1 set2 '())
    (reduce (lambda (acc set)
              (set-op acc set #'set-cons))
            set2 :initial-value set1)))

(defun set-diff (set1 set2)
  (set-op acc set (lambda (set acc) acc)))

;; sudoku

(defun valid-nums-at (row-l col-l reg-l all-num)
  (set-diff (union-set (list->order-set row-l) col-l reg-l)
            all-num))

(defun map-sudoku (sudoku posns i)
  (labels
      ((aux (lst n posns)
         (cond ((null lst) '())
               ((= n (car posns))
                (cons (solve-at-pos sudoku (car posns) (car lst))
                      (aux (cdr lst) (+ n 1) (cdr posns))))
               (t
                (cons (car lst) (aux (cdr lst) (+ n 1) posns))))))
    (aux (sudoku-board sudoku) 0 posns)))

;; (defun map-board )
(defun next (board &optional (blank-posns '()))
  (if (null blank-posns)
      (next board
            (loop :for idx = 0 then (1+ idx)
               :for x in board
               :untill (null idx)
               :collect idx))
      (loop :for idx = 0 then (1+ idx)
         :for x in board
         :with posn = (car blank-posns)
         :untill (null idx)
         :if (= posn idx)
         :collect (solve-at-pos board idx)
         :else
         :collect x)))

;; (defun solve (board)
;;   (loop for y to 8
;;      for row = (get-row-at board y) do
;;        (loop for x to 8
;;           for col = (get-col-at board x) do
;;             (let* ((x-y (list x y))
;;                    (num (solve-board-at board x-y
;;                                         row col)))
;;               (set-at-pos *solution* x-y num)
;;               (if (numberp num)
;;                   (set-at-pos board x-y num)))))

;;   (format t "~a~%" *solution*)
;;   (print-board board)
;;   (if (equal (read-line) "")
;;       (solve board)))

(defstruct list-stream
  list
  index)

(defmethod next ((stream list-stream))
  (let ((lst (list-stream-list stream))
        (idx (list-stream-index stream)))
    (if (null lst)
        nil
        (let ((x (car list)))
          (setq (list-stream-list stream) (cdr lst)
                (list-stream-index stream) (1+ idx))
          (list idx x)))))

;;
