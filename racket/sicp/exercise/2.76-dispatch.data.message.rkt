As a large system with generic operations
evolves, new types of data objects or new operations may
be needed. For each of the three strategies—generic opera-
tions with explicit dispatch, data-directed style, and message-
passing-style—describe the changes that must be made to a
system in order to add new types or new operations.

;;; explicit dispatch ;;;
;; new type
(define (constructor ...)
  (list 'type ...))
(define (selector1 type) ...)
(define (selector2 type) ...)

;; new operation
(define (operation obj-list)
  body
  ...)

;;; data-directed style ;;;
;; new type:
(put 'new-type new-operation)
;; new operation
(define (new-operation args)
  body
  ...)
(put 'existing-type new-operation)

;;; message-passing-style ;;;

;; new type:
(define (exisiting-operation vars)
  (define (dispatch op)
    ((exisiting-op-check) body ...)
    ((new-op-check) body ...))
  dispatch)
;; new operation
(define (new-operation var)
  (define (dispatch op)
    (cond ((all-previously-defined-type) body ...)
          ((eq? op 'var) var)
          (else (error "Unknown op : MAKE-FROM-REAL-IMAG" op))))
  dispatch)

Which organization would be most appropriate for a system in
which new types must often be added?

=> message-passing
;; it provides a one line interface addition change

Which would be most appropriate for a system in which new operations
must often be added?

=> data-directed
;; it provides a one line interface addition change
