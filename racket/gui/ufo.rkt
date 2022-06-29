#lang racket
(require 2htdp/universe 2htdp/image)


(define WIDTH 200)
(define HEIGHT 300)

(define (add-3-to-state current-state)
    (+ current-state 3))

;; (define IMAGE-OF-UFO "ufo.png")
(define IMAGE-OF-UFO (circle 10 "solid" "red"))

(define (draw-a-ufo-onto-an-empty-scene current-state)
    (place-image
     IMAGE-OF-UFO (/ WIDTH 2) current-state
     (empty-scene WIDTH HEIGHT)))

(define (state-is-300 current-state)
    (>= current-state 300))

(big-bang 0
          (on-tick add-3-to-state)
          (to-draw draw-a-ufo-onto-an-empty-scene)
          (stop-when state-is-300))
;; (define (main)
(draw-a-ufo-onto-an-empty-scene 33)
;;     )

;; (main)