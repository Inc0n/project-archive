(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))


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