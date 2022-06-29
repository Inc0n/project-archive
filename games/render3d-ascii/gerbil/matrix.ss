
(import :std/iter
        :std/srfi/43)

(def matrix-x (cut vetor-ref <> 0))
(def matrix-y (cut vetor-ref <> 1))
(def matrix-z (cut vetor-ref <> 2))
(def matrix-w (cut vetor-ref <> 3))

(def identity-h
  (vector (vector 1. 0. 0. 0.)
          (vector 0. 1. 0. 0.)
          (vector 0. 0. 1. 0.)
          (vector 0. 0. 0. 1.)))

(def (compose m1 m2)
  (def copy (vector-copy m1))
  (for (i (vector-length m1))
    (for (j (vector-length (matrix-x m2)))
      (vector-set!
       copy
       (for/fold (acc 0.) (k (vector-length m2))
         (+ acc (* (vector-ref (vector-ref m1 i) k)
                   (vector-ref (vector-ref m2 k) j)))))))
  copy)

(def (transpose m)
  (apply vector-map make-vector m))

(def (scaling-h k)
  (vector (vector k  0. 0. 0.)
          (vector 0. k  0. 0.)
          (vector 0. 0. k  0.)
          (vector 0. 0. 0. 1.)))

(def (translation-h vec)
  (vector (vector 1. 0. 0. (matrix-x vec))
          (vector 0. 1. 0. (matrix-y vec))
          (vector 0. 0. 1. (matrix-z vec))
          (vector 0. 0. 0. 1.)))

(def (rotation-h theta axis-norm)
  )