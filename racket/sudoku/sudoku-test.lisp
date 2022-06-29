
(defmacro test (test-expr expected)
  (let ((var (gensym)))
    `(let ((,var ,test-expr))
       (if (equal ,var ,expected)
           (format t "~a = ~a pass~%" ',test-expr ,expected)
           (format t "~a = ~a expected ~a fail~%" ',test-expr ,var ,expected)))))

(defun test-region ()
  (test (get-region-at 0 9) 0)
  (test (get-region-at 9 9) 0)
  (test (get-region-at 18 9) 0)
  (test (get-region-at 27 9) 3)
  (test (get-region-at 36 9) 3)

  (test (get-region-at 3 9) 1)
  (test (get-region-at 12 9) 1)
  (test (get-region-at 30 9) 4))


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

(defun test-sudoku-interface ()
  (let ((sudoku (new-sudoku '(9 0 5 0 0 0 3 0 0
                              0 8 0 3 9 0 0 0 0
                              4 3 6 0 0 0 0 7 1
                              0 1 0 0 3 2 0 0 0
                              0 0 9 0 0 0 5 0 0
                              0 0 0 9 4 0 0 1 0
                              3 6 0 0 0 0 4 9 5
                              0 0 0 0 8 9 0 3 0
                              0 0 7 0 0 0 8 0 2))))
    (test (sudoku-size sudoku) 9)
    (test (sudoku-row-lst-nth sudoku 4) '(9 0 4 0 0 0 3 0 0))
    (test (sudoku-col-lst-nth sudoku 4) '(0 9 0 3 0 4 0 8 0))
    (test (sudoku-reg-lst-nth sudoku 4) '(0 0 0 3 9 0 0 0 0))
    (test (get-reg-check-lst *sudoku* 4) '((3 0) (0 0)))
    (test (get-reg-check-lst *sudoku* 13) '((0 0) (0 0)))
    (test (get-reg-check-lst *sudoku* 10) '((9 5) (4 6)))))

(defmethod is-num-valid (possible-posns num)
  (every (lambda (x)
           (every (lambda (x)
                    (cond ((listp x) (member num x :test #'/=))
                          ((or (= x num) (zerop x)) nil)
                          (t t)))
                  x))
         possible-posns))