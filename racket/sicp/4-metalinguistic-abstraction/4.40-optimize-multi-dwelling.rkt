
(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

;; it would because distinct? take n^2 time where other requires
;; take constant time, so the procedure would run faster if
;; the distince? require is run after all the other requirement
;; is met

(define (multiple-dwelling)
  (let ((baker    (amb 1 2 3 4)))
    (let ((cooper (amb 2 3 4 5)))
      (let ((fletcher (amb 2 3 4)))
        (require (not (= (abs (- fletcher cooper)) 1)))
        (let ((miller (amb 1 2 3 4 5))
              (smith    (amb 1 2 3 4 5)))
          (require (> miller cooper))
          (require (not (= (abs (- smith fletcher)) 1)))
          (require (distinct? (list baker cooper fletcher miller smith)))
          (list (list 'baker baker)       (list 'cooper cooper)
                (list 'fletcher fletcher) (list 'miller miller)
                (list 'smith smith)))))))