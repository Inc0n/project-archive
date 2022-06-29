;; Order of growth

;; finding the order of growth of the encoding procedure in exercise 2.68

;; by consider the NO. steps for the most frequent and the least frequent
;; symbols

(define (element-in-set? x set)
  (define (aux set acc)
    (if (leaf? set)
        (if (eq? (symbol-leaf set) x)
            (reverse acc)
            '())
        (let ((right? (aux (right-branch set) (cons 1 acc))))
          (if (null? right?)
              (aux (left-branch set) (cons 0 acc))
              right?))))
  (aux set '()))

;; this is 0(n/2)
;; the most frequent symbol would be 0(k)
;; the least frequent symbol would be 0(n)
;; => 0((n+k)/2) = 0(n/2)

(define (encode-symbol sym branch)
  (define (rec-encode branch)
    (if (leaf? branch)
        (if (element-in-set? sym branch)
            '()
            (error "symbol cannot be encoded" sym))
        (let ((right-node (right-branch branch)))
          (if (element-in-set? sym right-node)
              (cons 1 (rec-encode right-node))
              (cons 0 (rec-encode (left-branch branch)))))))
  (rec-encode branch))

;;; the most frequent symbol
;; 0(k) for element-in-set? + 0(k) = 0 (2k)
;;; the least frequent symbol
;; 0(n) + 0(n/2) + 0(n/4) + .. + 0(k) = 0(2n)

(define-syntax let-if
  (syntax-rules ()
    ((_ (proc (binding expression)) first second)
     (let ((binding expression))
       (if (proc binding)
           first
           second)))
    (_ ((binding expression)) first second)
     (let ((binding expression))
       (if binding
           first
           second))))

(define (encode-symbol x set)
  (define (aux set acc)
    (if (leaf? set)
        (if (eq? (symbol-leaf set) x)
            (reverse acc)
            '())
        (let-if (null? (right? (aux (right-branch set)
                                    (cons 1 acc))))
                (aux (left-branch set) (cons 0 acc))
                right?)))
  (let-if (null? (ret (aux set '())))
          (error "symbol cannot be encoded" sym)
          ret))

;;; the most frequent symbol
;; 0(k)
;;; the least frequent symbol
;; 0(n/2) + 0(n/4) + .. + 0(k) = 0(n)

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))


