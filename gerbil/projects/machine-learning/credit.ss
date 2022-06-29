
(import :std/misc/func)

(def *data*
  ;; risk history debt collateral income
  '((high bad high none 0-15k)
    (high unknown high none 15k-35k)
    (moderate unknown low none 15k-35k)
    (high unknown low none 0-15k)
    (low unknown low none over-35k)
    (low uknown low adequate over-35k)
    (high bad low none 0-35k)
    (moderate bad low adequate over-35k)
    (low good low none over-35k)
    (low good high adequate over-35k)
    (high good high none 0-15k)
    (moderate good high none 15k-35k)
    (low good high none over-35k)
    (high bad high none 15k-35k)))

(def (data-risk data) (car data))
(def (data-history data) (cadr data))
(def (data-debt data) (caddr data))
(def (data-collateral data) (cadddr data))
(def data-income (cut (compose1 car cddddr) <>))

(def *traits* [data-risk data-history data-debt data-collateral data-income])

(defstruct property (name test values))
;; decision tree
(defstruct d-tree (name test branches))
(defstruct leaf (value))

(def (build-tree data target-attri attris)
  (cond ((null? attris) #f)
        (else
         (let ((label )
               (a (best-attri data target-attri)))
           (make-d-tree a (get-attri-name a)
                        )
           (map (lambda (x)
                  (build-tree data target-attri (remq a attris)))
                (unique (map a data) eq?))))))

(defstruct frame (data attris classifier size info))
(defstruct part (test-name test components info-gain))

(def (gen-parts frame)
  ""
  (frame-classifier frame))

(def (max2-fn-by key-fn)
    (lambda (a b)
      (if (> (key-fn a)
             (key-fn b))
        a
        b)))

(def (choose-part parts)
  (foldr (max2-fn-by part-info-gain)
         (make-part #f #f #f 0)
         parts))

(def (build-tree frame)
  (cond ;;Case 1: empty example set.
   ((null? (frame-data frame))
    (make-leaf "unable to classify: no examples"))
   ;; Case 2: all tests have been used.
   ((null? (frame-attris frame))
    (make-leaf (list-classes data)))
   ;; Case 3: all examples in same class.
   ((zero? (frame-info frame))
    (make-leaf ((property-test (frame-classifier frame))
                (car (frame-data frame)))))
   ;; Case 4: select test and recur.
   (else
    (let ((part (choose-part (gen-parts frame))))
      (make-decision-tree (part-test-name part)
                          (part-test part)
                          (map (lambda (x)
                                 (cons (car x)
                                       (build-tree (cdr x))))
                               (part-components part)))))))

  (build-tree *data* *traits* '())