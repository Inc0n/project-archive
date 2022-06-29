
(require "2.46-vector-data.rkt"
         "2.47-frame-data.rkt"
         "2.48-segment-data.rkt")

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame)
         (start-segment segment))
        ((frame-coord-map frame)
         (end-segment segment))))
     segment-list)))

(define (with-frame frame proc)
  (let ((origin (frame-origin frame))
        (edge1 (frame-edge1 frame))
        (edge2 (frame-edge2 frame)))
    (let ((org->edg1 (sub-vect edge1 origin))
          (org->edg2 (sub-vect edge2 origin))))
    (proc origin
          org->edg1
          org->edg2
          (add-vect org->edg1 org->edg2))))

(define (outline-painter frame)
  (segments->painter
   (with-frame
    frame
    (lambda (org org->edg1 org->edg2 edg1->edg2)
      (list (make-segment org org->edg1)
            (make-segment org org->edg2)
            (make-segment org->edg1 edg1->edg2)
            (make-segment org->edg2 edg1->edg2))))))

(define (corners-x-painter frame)
  (segments->painter
   (with-frame
    frame
    (lambda (org org->edg1 org->edg2 edg1->edg2)
      (list (make-segment org edg1->edg1)
            (make-segment org->edg1 org->edg2))))))