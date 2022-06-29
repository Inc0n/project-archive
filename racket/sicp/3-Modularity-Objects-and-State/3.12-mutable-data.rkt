
;; Exercise 3.12

(define (append x y)
  (if (null? x)
      y
      (cons (car x)
            (append (cdr x) y))))


(define (append! x y)
  (set! (cdr (last-pair x)) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))                 ; '(a b c d)
(cdr x)                                 ; '(b)
(define w (append! x y))                ; '(a b c d)
(cdr x)                                 ; '(b c d)

;; Exercise 3.13
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

;; Draw a box-and-pointer diagram that shows the structure z
(define z (make-cycle (list 'a 'b 'c)))

;; Z:
;;   ,----------------------------------------------,
;;   |                                              |
;;   v                                              ^
;;  +-------+   +-------+   +-------+   +-------+   |
;;  | a | x-+-->| b | x-+-->| c | x +-->| * | \ | --+
;;  +-------+   +-------+   +-------+   +-------+
;;    |           |           |
;;    v           v           v
;;    'a          'b         'c

;; Q: What happens if we try to compute (last-pair z) ?
;; A: an infinite list

(require rnrs/mutable-pairs-6)
(define (square x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

;; while reverse the order of the list, each following item are nests
;; the previous ones
;; eg.
(define v '(a b c d))
;; =>
(define w (mystery v))
'(d (c (b (a ()))))

;; +--------------------------------------------------------------------+
;; |   x:    *                                                          |
;; |   y: /  |                                                          |
;; +--------------------------------------------------------------------+
;;           |                                      +----------------+
;;           |                                      | temp:  *       |
;;           |                                      +------- |-------+
;;           |                                               |
;;           -----------.              ,----------------------
;;                      v              v
;;                  +---+---+      +---+---+      +---+---+     +---+----
;;                  | * | *------->| * | *------->| * | *------>| * | / |
;;                  +-|-+---+      +-|-+---+      +-|-+---+     +-|-+---+
;;                    v              v              v             v
;;                  +---+          +---+          +---+         +---+
;;                  | a |          | b |          | c |         | d |
;;                  +---+          +---+          +---+         +---+
