
(require "2.67-decode-tree.rkt"
         "2.68-encode-tree.rkt"
         "2.69-huffman-tree.rkt")

(define lyric-pairs
  '((A 2) (GET 2) (SHA 3) (WAH 1) (BOOM 1) (JOB 2) (NA 16) (YIP 9)))

(define lyric-tree (generate-huffman-tree lyric-pairs))

(define message '(GET A JOB
                      SHA NA NA NA NA NA NA NA NA
                      GET A JOB
                      SHA NA NA NA NA NA NA NA NA
                      WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
                      SHA BOOM))

;; > (encode message lyric-tree)
;; result =>
'(1 1 0 0 1 1 1 1 1 1 1 0 1 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 0 1 1 1 1 1
  1 1 0 1 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 0 0 1 0 1 0 1 0 1 0 1 0 1 0
  1 0 1 0 1 0 1 1 1 0 1 1 1 1 0 1)


;;;; from schemewiki sicp-ex-2.70
;; link: http://community.schemewiki.org/?sicp-ex-2.70
;; date: 2019-05-14 20:56
;;;;;; ;; ;; ;;

 (length encoded-rock-song)
;; 84

 ; If we were to use a fixed-length encoding on that rock song, we would need 3 bits (8 = 2^3) per symbol, i.e.:
 (* 3 (length message))
;;108

;; A 22% gain in size seems to be coherent