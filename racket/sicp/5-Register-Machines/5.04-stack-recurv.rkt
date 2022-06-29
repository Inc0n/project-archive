(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(controller
 (assign continue (label expt-done))
 ;; set up final return address
 expt-loop
   (test (op =) (reg n) (const 1))
   (branch (label base-case))
   ;; Set up for the recursive call by saving n and continue.
   ;; Set up continue so that the computation will continue
   ;; at after-fact when the subroutine returns.
   (save continue)
   (assign n (op -) (reg n) (const 1))
   (assign continue (label after-expt))
   (goto (label fact-loop))
 after-expt
   (restore n)
   (restore continue)
   (assign val (op *) (reg b) (reg val))  ; val now contains n(n - 1)!
   (goto (reg continue))                  ; return to caller
 base-case
   (assign val (const 1))                 ; base case: 1! = 1
   (goto (reg continue))                  ; return to caller
 expt-done)

(define (expt b n)
  (define (expt-iter counter product)
    (if (= counter 0)
        product
        (expt-iter (- counter 1)
                   (* b product))))
  (expt-iter n 1))

(controller
 (assign continue (label expt-done))
 (assign counter (reg n))
 (assign product (const 1))
 ;; set up final return address
 expt-iter
   (test (op =) (reg counter) (const 1))
   (branch (label expt-done))
   ;; Set up for the recursive call by saving n and continue.
   ;; Set up continue so that the computation will continue
   ;; at after-fact when the subroutine returns.
   (assign counter (op -) (reg counter) (const 1))
   (assign product (op *) (reg b) (reg product))
   (goto (label fact-loop))
 expt-done)