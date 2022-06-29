
(define (install-scheme-number-package)
  ;; ...
  (define (raise num) (make-rat num 1))
  (put 'raise  'scheme-numer raise)
  'done)

(define (install-rational-package)
  ;; ...
  (define (raise x) (make-real (/ (numer x) (denom x))))
  ;; ...
  (put 'raise 'rational raise)
  'done)

(define (install-real-package)
  ;; ...
  (define (raise x)
    (make-complex-real-imag x 0))
  ;; ...
  (put 'raise real raise)
  'done)
(define (install-complex-package)
  ;; ...
  ;; no need for complex, its the highest level of type
  ;; ...
  'done)

(define (raise x) (apply-generic 'raise x))


(define (type-lvl type)
  (cond ((eq? type 'integer) 0)
        ((eq? type 'rational) 1)
        ((eq? type 'real) 2)
        ((eq? type 'complex) 3)
        (else (error "Invalid type: LEVEL" type))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((x (- (type-lvl type1)
                            (type-lvl type2))))
                  (if (zero? x)
                      (apply-generic op a1 a2)
                      (if (positive? x)
                          (apply-generic op a1 (map-times raise a2 x))
                          (apply-generic op (map-times raise a1 x) a2)))))
              (error "No method for these types"
                     (list op type-tags)))))))

(require (submod "../2-Building-Abstractions-with-Data/2.3.3-example-representing-sets.rkt" binary-tree-set))

(define empty-tree '())
(define empty-tree? null?)
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry (left empty-tree) (right empty-tree))
  (list entry left right))

(define num-type-tree
  (make-tree 'integer
             (make-tree 'rational
                        (make-tree 'real
                                   (make-tree 'complex)))))
(define (tree-op proc x tree)
  (define (aux tree)
    (if (empty-tree? tree)
        #f
        (if (eq? (entry tree) x)
            (proc tree)
            (let ((right (aux (right-branch tree))))
              (if right
                  right
                  (aux (left-branch tree)))))))
  (aux tree))

(define (subtree x tree)
  (tree-op (lambda (tree) tree) x tree))

(define (in-tree? x tree)
  (tree-op (lambda (tree) #t) x tree))

(define (tree-path x tree (final-proc reverse))
  (define (aux tree acc)
    (if (empty-tree? tree)
        #f
        (let ((root (entry tree)))
          (if (eq? root x)
              (final-proc (cons root acc))
              (let ((right (aux (right-branch tree)
                                (cons root acc))))
                (if right
                    right
                    (aux (left-branch tree)
                         (cons root acc))))))))
  (aux tree '()))

(define (raise-type type1 type2)
  (let ((path (tree-path type1 (subree type2 num-type-tree))))
    (if path
        (map (lambda () (raise type2)) path)
        )))