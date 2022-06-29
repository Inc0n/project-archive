betty: "kitty was second in the examination. I was
third."
Ethel: "You’ll be glad to hear that I was first. Joan
was 2nd."
Joan: "I was third, and poor old Ethel was fifth."
kitty: "I was second. Mary was only fourth."
Mary: "I was fourth. Betty was first."

(loop '((kitty 2) (betty 3))
      (lambda (order)
        (loop '((ethel 1) (joan  2))
              (lambda (order)
                (loop '((joan  3) (ethel 5))
                      (lambda (order)
                        (loop '((kitty 2) (mary  4))
                              (lambda (order)
                                (loop '((mary  4) (betty 1))
                                      (lambda (order)
                                        order)
                                      order))
                              order))
                      order))
              order))
      '())


(define (loop-proc cond-proc)
  (define (loop-inner lst proc acc)
    (if (null? lst)
        '()
        (let ((x (car lst)))
          (if (cond-proc x acc)
              (cons (proc (cons x acc))
                    (loop-inner (cdr lst)
                                proc
                                acc))
              (loop-inner (cdr lst)
                          proc
                          acc)))))
  loop-inner)

(define (in-order? test-proc)
  (define (in-order intel order)
    (cond ((null? order) true)
          ((test-proc intel order) false)
          (else
           (in-order intel (cdr order)))))
  in-order)

(define in-order (in-order?
                  (lambda (intel order)
                    (or
                     (equal? (car intel) (caar order))
                     (equal? (cdr intel) (cdar order))))))

;; (define in-set-num? (in-order?
;;                      (lambda (intel order)
;;                        (equal? (cdr intel) (cdar order)))))

(define in-set-sym?
  (in-order?
   (lambda (intel order)
     (equal? (car intel) (caar order)))))

(define data
    '((betty ((kitty 2) (betty 3)))
      (ethel ((ethel 1) (joan  2)))
      (joan  ((joan  3) (ethel 5)))
      (kitty ((kitty 2) (mary  4)))
      (mary  ((mary  4) (betty 1)))))

(define (test)
  (define loop (loop-proc in-order))
  (define processed
    '(((kitty 2) (betty 3))
      ((ethel 1) (joan 2))
      ((joan 3) (ethel 5))
      ((kitty 2) (mary 4))
      ((mary 4) (betty 1))))
  (define (try-all lst)
    (define (aux lst prevs acc)
      (if (null? lst)
          acc
          (let ((x (car lst))
                (lst (cdr lst)))
            (aux lst
                 (cons x prevs)
                 (cons (cons x (append lst prevs))
                       acc)))))
    (aux lst '() '()))
  (define (loop-auto data order)
    (if (null? data)
        '()
        (loop (car data)
              (lambda (order)
                (loop-auto (cdr data) order))
              order)))
  ;; (loop-auto processed '())
  ;; (map (lambda (x) (loop-auto x '())) (try-all processed))
  ;; (loop '((kitty 2) (betty 3))

  (define (solve data set)
    (if (null? data)
        set
        (let ((statement (car data)))
          (if (in-order statement set)
              (solve (cdr data) (cons statement set))
              (solve (cdr data) set)))))
  (define (reform data)
    (if (null? data)
        '()
        (append (cadar data) (reform (cdr data)))))
  (map (lambda (x) (solve x '()))
       (try-all (set->list (list->set (reform data))))))
