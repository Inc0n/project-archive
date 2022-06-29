#| Iterative-improve
Write a procedure iterative-improve that takes two procedures as
arguments: a method for telling whether a guess is good enough and a
method for improving a guess. iterative-improve should return as its
value a procedure that takes a guess as argument and keeps improving the
guess until it is good enough.
|#

(define (iter-improve good-enough? method)
  (lambda (x)
    (if (good-enough? x)
        x
        (method x))))