
(import :std/srfi/43)

(def vec-x (cut vector-ref <> 0))
(def vec-y (cut vector-ref <> 1))
(def vec-z (cut vector-ref <> 2))

(def vec-abs (cut vector-map abs <>))
(def vec-negate (cut vector-map (cut - <>) <>))
(def (vec-add a b) (vector-map + a b))
(def (vec-sub a b) (vector-map - a b))
(def (vec-mul a k) (vector-map (cut * k <>) a))
(def (vec-dot a b) (vector-map * a b))
(def (vec-distance a b)
  (def vec (vec-sub a b))
  (sqrt (vec-dot vec vec)))

(def (vec-2d->3d v2)
  (make-vector (vec-x v2)
               (vec-y v2)
               0.0))

(def (vec-3d->2d v3)
  (make-vector (vec-x v3) (vec-y v3)))


