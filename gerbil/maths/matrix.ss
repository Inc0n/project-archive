
(import :std/srfi/1)
(import :std/srfi/43)

(defstruct matrix (dimension data)
  constructor: :init!
  final: #t)

;; making field read-only
(def (matrix-dimension-set! self value)
  (error "setting on read-only field dimension of matrix" self value))
(def (matrix-data-set! self value)
  (error "setting on read-only field data of matrix" self value))

(def (matrix-row self row-i)
  (vector-ref (matrix-data self) row-i))

(def (matrix-col self col-i)
  (vector-map (cut vector-ref <> col-j)
              (matrix-data self)))

(defmethod {:init! matrix}
  (lambda rows
    (match rows
      ([row . rest-rows]
       (let (row-len (length row))
         (if (every (is length row-len)
                    rest-rows)
           (make-matrix [row-len . (length rows)]
                        (map list->vector rows))
           (error "inbalance matrix " rows))))
      (else (make-matrix 0 [])))))

(defmethod {print matrix}
  (lambda (self)
    (display "(")
    (display (string-join (map list->string (vector->list (matrix-data self)))
                          #\newline))
    (display ")")))

(def (matrix-op m1 m2 op)
  (vector-map
   (lambda (x y)
     (vector-map op x y))
   (matrix-data m1)
   (matrix-data m2)))

(def (matrix+ m1 m2)
  (if (= (matrix-dimension m1)
         (matrix-dimension m2))
    (make-matrix (matrix-dimension m1)
                 (matrix-op m1 m2 +))
    (error "matrix+ dimension mismatch" m1 m2)))

(def (matrix- m1 m2)
  (if (= (matrix-dimension m1)
         (matrix-dimension m2))
    (make-matrix (matrix-dimension m1)
                 (matrix-op m1 m2 -))
    (error "matrix- dimension mismatch" m1 m2)))

(def (matrix* m1 m2)
  (if (= (matrix-dimension m1)
         (matrix-dimension m2))
    (make-matrix (matrix-dimension m1)
                 (matrix-op m1 m2 *))
    (error "matrix+ dimension mismatch" m1 m2)))

(def (test)
  (let ((m1 (make-matrix [1 2 3]
                         [3 4 5]))
        (m2 (make-matrix [1 2 3]
                         [3 4 5])))
    {print (matrix+ m1 m2)}))