
(def (flatmap proc lst)
  (foldl (lambda (x acc)
           (append acc (proc x)))
         []
         lst))

(def car?
  (match <>
    ([x] x)
    (x x)))