#|

The following pattern of numbers is called
 Pascal’s triangle

            1
         1     1
        1   2   1
      1   3   3   1
    1   4   6   4   1
          . . .

 The numbers at the edge of the triangle are all 1, and each
 number inside the triangle is the sum of the two numbers
 above it.

 Write a procedure that computes elements of Pascal’s triangle
 by means of a recursive process

|#

;; pt -> Pascal Triangle

(load-option 'format)

(define (triangle n)
  (define (next first rest acc)
    (if (null? rest)
        (cons first acc)
        (let ((a (car rest)))
          (next a
                (cdr rest)
                (cons (+ first a) acc)))))
  (define (aux lst n)
    (display lst)
    (display #\Newline)
    (if (not (= n 0))
        (aux
         (next 0 lst '())
         (- n 1))))
  (aux '(1) n))
