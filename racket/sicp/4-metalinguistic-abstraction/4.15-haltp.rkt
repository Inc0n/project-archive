Given a one-argument procedure p and an object a,
p is said to “halt” on a if evaluating the expression
(p a) returns a value (as opposed to terminating with
an error message or running forever).

(define (run-forever) (run-forever))
(define (try p)
  (if (halts? p p) (run-forever) 'halted))

(define (halts? proc arg)
  (proc arg))

(try try)

(if (try try) (run-forever) 'halted)

if halt? proc testing whether given proc is haltable or not by
calling it, then proc try would never return if passed in p
run-forever,