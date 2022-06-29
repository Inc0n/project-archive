
;;;;;;;;;
;;
;; data structure
;;
;;;;;;;;;

(struct tile (posn color))

(struct tetra (tiles))

(struct world (pile moving))

(struct posn (x y))

(define (posn-x-y posn)
  (values (posn-x posn)
          (posn-y posn)))

(define (close? s g)
    (posn=? s (goo-loc g)))

(define (all-but-last segs)
    (if (null? (cdr segs)) empty
        (cons (car segs) (all-but-last (cdr segs)))))

(define (posn-move p dx dy)
    (posn (+ (posn-x p) dx)
          (+ (posn-y p) dy)))

(define (next-head sn)
    (define head (snake-head sn))
  (define dir (snake-dir sn))
  (cond ((string=? dir "up") (posn-move head 0 -1))
        ((string=? dir "down") (posn-move head 0 1))
        ((string=? dir "left") (posn-move head -1 0))
        ((string=? dir "right") (posn-move head 1 0))))

;;;;;;;;;
;;
;; Goos logic
;;
;;;;;;;;;

(define (fresh-goo)
    (goo (posn (add1 (random (sub1 SIZE)))
               (add1 (random (sub1 SIZE))))
         EXPIRATION-TIME))

(define (age-goo goos)
    (rot (renew goos)))

(define (rotten? goo)
    (zero? (goo-expire g)))

(define (rot goos)
    (for/list ((goo (in-list goos)))
              (decay goo)))

(define (renew goos)
    (for/list ((goo (in-list goos)))
              (if (rotten? goo)
                  (fresh-goo)
                  goo)))

;;;;;;;;;
;;
;; Movement
;;
;;;;;;;;;

(define (dir? x)
    (or (key=? x "up")
        (key=? x "down")
        (key=? x "left")
        (key=? x "right")))

(define (world-change-dir world dir)
    (define the-snake (pit-snake world))
  (if (and (opposide-dir? (snake-dir the-snake) d)
           (cons? (cdr (snake-segs the-snake))))
      (stop-with world)
      (pit (snake-change-dir the-snake d) (pit-goos w))))

(define (opposite-dir? d1 d2)
    (cond ((string=? d1 "up") (string=? d2 "down"))
          ((string=? d1 "down") (string=? d2 "up"))
          ((string=? d1 "left") (string=? d2 "right"))
          ((string=? d1 "right") (string=? d2 "left"))))

(define (direct-snake world key)
    (if (dir? key)
        (world-change-dir world key)
        world))

;;;;;;;;;
;;
;; Render
;;
;;;;;;;;;

(define (goo-list+scene goos scene)
    (define goo-posns
        (for/list ((i (in-list goos)))
                  (goo-loc i)))
  (img-list+scene goo-posns GOO-IMG scene))

(define (img+scene posn img scene)
    (place-image img
                 (* (posn-x posn) SEG-SIZE)
                 (* (posn-y posn) SEG-SIZE)
                 scene))

(define (img-list+scene posns img scene)
    (for ((i (in-list posns)))
         (img+scene i img scene)))

(define (snake+scene snake scene)
    (define snake-body-scene
        (img-list+scene (snake-body snake) SEG-IMG scene))
  (define dir (snake-dir snake))
  (img+scene (snake-head snake)
             (cond ((string=? dir "up") HEAD-UP-IMG)
                   ((string=? dir "down") HEAD-DOWN-IMG)
                   ((string=? dir "left") HEAD-LEFT-IMG)
                   ((string=? dir "right") HEAD-RIGHT-IMG))
             snake-body-scene))

(define (render-pit w)
    (snake+scene (pit-snake w)
                 (goo-list+scene (pit-goos w) MT-SCENE)))

;;;;;;;;;
;;
;; Tile logic
;;
;;;;;;;;;

(define (rotate tetra (direction +))
    (if (or (eq? direction +) (eq? direction -))
        (tile (for*/list ((posn (in-list (posns (tetra-tiles tetra))))
                          ((x y) (posn-x-y posn)))
                         (posn y (- x)))
              (tile-posn tile))))

(define (move tile dx dy)
    )
;;;;;;;;;
;;
;; Game logic
;;
;;;;;;;;;

(define (stop? w)
    (define pile (world-pile w))
  (ceiling-colliding? pile))

(define (ceiling-colliding? pile)
  (for/or ((tile (in-list pile)))
    (set!-values (x y) (posn-x-y tile))
    (<= y 0)))

(define (posn-move p dx dy)
    (posn (+ (posn-x p) dx)
          (+ (posn-y p) dy)))

;;;;;;;;;
;;
;; Main
;;
;;;;;;;;;

(define (handle-key w key)
    (cond
      ((key=? key "q")
       (stop-with w))
      (else
       (world (world-pile w)
              (change-tile tile key)))))

(define (change-tile tile key)
    (cond
      ((key=? key "a")
       (rotate tile +))
      ((key=? key "d")
       (rotate tile -))
      ((key=? key "left")
       (move tile -1 0))
      ((key=? key "right")
       (move tile 1 0))
      ((key=? key "down")
       (move tile 0 1))
      (else tile)))

(define (next-pit w)
    (define snake (pit-snake w))
  (define goos (pit-goos w))
  (define goo-to-eat (can-eat snake goos))
  (if goo-to-eat
      (pit (grow snake) (age-goo (eat goos goo-to-eat)))
      (pit (slither snake) (age-goo goos))))

(define (start-game)
    (big-bang (world empty)
              (on-tick next-pit TICK-RATE)
              (on-key handle-key)
              (to-draw render-pit)
              (stop-when stop? reader-end)))