#|| Exercise ||#

(define (unique-triplets n s)
  (filter (lambda (triplet) (= (accumulate + 0 triplet) s))
          (flatmap (lambda (i)
                     (flatmap (lambda (j)
                                (map (lambda (k) (list i j k))
                                     (enumerate-interval 1 (- j 1))))
                              (enumerate-interval 1 (- i 1))))
                   (enumerate-interval 1 n))))