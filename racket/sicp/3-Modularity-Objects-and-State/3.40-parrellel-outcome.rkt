(define x 10)
(parallel-execute
 (lambda () (set! x (* x x)))
 (lambda () (set! x (* x x x))))

at-the-same-time:
100 or 1000
1000*1000: p1 access x after p2 finished
10*1000: p2 changes x while p1 access x for the 2nd times
100*100*100: p1 changes x before p2 access x
10*100*100: p1 changes x while p2 access x for the 2nd and 3rd times
10*10*100: p1 changes x while p2 access x for the 3rd time
