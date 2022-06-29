
(import vector)

(def (bound-between? x lower upper)
  (and (< x upper)
       (> x lower)))

(def (aabb-contains-point2d aabb p)
  (and (bound-between? (vec-x p)
                       (vec-x (vec-x aabb))
                       (vec-x (vec-y aabb)))
       (bound-between? (vec-y p)
                       (vec-y (vec-x aabb))
                       (vec-y (vec-y aabb)))))

(def (poly-contains-point2d poly p)
  )

(def --double-area-2d (v0 v1 v2)
  (abs
    (+ (* (vec-x v0) (vec-y v1))
       (* (vec-x v1) (vec-y v2))
       (* (vec-x v2) (vec-y v0))
       (- (* (vec-x v0) (vec-y v2)))
       (- (* (vec-x v2) (vec-y v1)))
       (- (* (vec-x v1) (vec-y v0))))))