
(import :std/sort
        :std/srfi/143)

(def (min-abs-diff lst)
  "returns a list of pairs in the LST each has the minimum absolute difference"
  (let loop ((lst (sort lst >))
             (min-diff fx-greatest)
             (acc []))
    (match lst
      ([x y . xs]
       (let (diff (abs (- x y)))
         (cond ((> diff min-diff)
                (loop (cdr lst)
                      min-diff
                      acc))
               ((= diff min-diff)
                (loop (cdr lst)
                      min-diff
                      (cons [x y] acc)))
               (else ;; use new pairs
                (loop (cdr lst)
                      diff
                      [[x y]])))))
      ((or [] [x]) (reverse acc)))))