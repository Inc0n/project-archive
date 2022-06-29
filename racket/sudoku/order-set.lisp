
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

(defun list->order-set (list)
  (reduce #'adjoin-set list :initial-value '()))


;; sudoku

(defun adjoin-lst (lst x)
  (if (or (listp x) (zerop x))
      lst
      (adjoin-set lst x)))