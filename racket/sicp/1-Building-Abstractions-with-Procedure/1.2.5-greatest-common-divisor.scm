if r is the remainder when a is divided by b , then the common divisors
of a and b are precisely the same as the common divisors of b and r.
Thus, we can use the equation GCD(a,b) = GCD(b,r)
to successively reduce the problem of computing a GCD
to the problem of computing the GCD
of smaller and smaller pairs of integers. For example,
GCD(206,40) = GCD(40,6)
            = GCD(6,4)
            = GCD(4,2)
            = GCD(2,0)
            = 2
reduces GCD(206, 40) to GCD(2, 0), which is 2. It is possible to show
that starting with any two positive integers and performing repeated
reductions will always eventually produce a pair where the second number
is 0. Then the GCD is the other number in the pair. This method for com-
puting the GCD is known as Euclid’s Algorithm.

It is easy to express Euclid’s Algorithm as a procedure:

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))