
(import :gerbil/gambit/bits)

(def *structure-type*
  '(mineshaft ocean-ruins ocean-features village stronghold mansion ocean-monument slime-chunk biome-data outpost desert-temple jungle-temple buried-treasure shipwreck witch-hut igloo ruined-portal))


;; int, char, char, unsigned char, unsigned char
(defstruct structure (salt region-size chunk-range struct-type properties))


(def int+large_struct+ 1)
(def int+chunk_struct+ 2)

;; for desert temples, igloos, jungle temples and witch huts prior to 1.13 
(def struct+feature+       (make-structure 14357617  32  24  Desert-Temple  0))

;; ocean features before 1.16
(def struct+OCEAN_RUIN+    (make-structure 14357621  16 8  'ocean-ruin  0))
(def struct+SHIPWRECK+     (make-structure 165745295 15 7  'shipwreck  0))

;; 1.13 separated feature seeds by type 
(def struct+desert-temple+ (make-structure 14357617  32 24 'desert-temple  0))
(def struct+igloo+         (make-structure 14357618  32 24 'igloo  0))
(def struct+jungle_temple+ (make-structure 14357619  32 24 'jungle-temple  0)
(def struct+swamp_hut+     (make-structure 14357620  32 24 'witch-hut  0)
;; pillager outpost
(def struct+outpost+       (make-structure 165745296 32 24 'outpost  0)
(def struct+village+       (make-structure 10387312  32 24 'village  0)
(def struct+ocean-ruin+    (make-structure 14357621  20 12 'ocean-ruin  0)
(def struct+shipwreck+     (make-structure 165745295 24 20 'shipwreck  0)
(def struct+monument+      (make-structure 10387313  32 27 'ocean-monument int+large_struct+)
(def struct+mansion+       (make-structure 10387319  80 60 'mansion int+large_struct+)
;; overworld varia
(def struct+ruined-portal+ (make-structure 34222645  40 25 'ruined-portal  0))

;; structures that check each chunk individually
(def struct+treasure+      (make-structure 10387320   1  0 'Treasure int+chunk_struct+)

(defstruct quad_threadinfo_t
  (start end ;; int64_t
   sconf ;; StructureConfig
   threadID ;; int
   quality ;; int 
   fnam ;; string
))

(def +lowerBaseBitsQ1+ '(#x3f18 #x520a #x751a #x9a0a))
;; for quad-structure with quality 2 
(def +lowerBaseBitsQ2*
  '(#x0770 #x0775 #x07ad #x07b2 #x0c3a #x0c58
           #x0cba #x0cd8 #x0e38 #x0e5a #x0ed8 #x0eda #x111c #x1c96 #x2048 #x20e8
           #x2248 #x224a #x22c8 #x258d #x272d #x2732 #x2739 #x2758 #x275d #x27c8
           #x27c9 #x2aa9 #x2c3a #x2cba #x2eb8 #x308c #x3206 #x371a #x3890 #x3d0a
           #x3f18 #x4068 #x40ca #x40e8 #x418a #x4248 #x426a #x42ea #x4732 #x4738
           #x4739 #x4765 #x4768 #x476a #x47b0 #x47b5 #x47d4 #x47d9 #x47e8 #x4c58
           #x4e38 #x4eb8 #x4eda #x5118 #x520a #x5618 #x5918 #x591d #x5a08 #x5e18
           #x5f1c #x60ca #x6739 #x6748 #x6749 #x6758 #x6776 #x67b4 #x67b9 #x67c9
           #x67d8 #x67dd #x67ec #x6c3a #x6c58 #x6cba #x6d9a #x6e5a #x6ed8 #x6eda
           #x7108 #x717a #x751a #x7618 #x791c #x8068 #x8186 #x8248 #x824a #x82c8
           #x82ea #x8730 #x8739 #x8748 #x8768 #x87b9 #x87c9 #x87ce #x87d9 #x898d
           #x8c3a #x8cda #x8e38 #x8eb8 #x951e #x9718 #x9a0a #xa04a #xa068 #xa0ca
           #xa0e8 #xa18a #xa26a #xa2e8 #xa2ea #xa43d #xa4e1 #xa589 #xa76d #xa7ac
           #xa7b1 #xa7ed #xa85d #xa86d #xaa2d #xb1f8 #xb217 #xb9f8 #xba09 #xba17
           #xbb0f #xc54c #xc6f9 #xc954 #xc9ce #xd70b #xd719 #xdc55 #xdf0b #xe1c4
           #xe556 #xe589 #xea5d))

(def (mineshaft-chunk? seed chunk-x chunk-z)
  (set-seed! seed)
  (let ((i (next-long))
        (j (next-long)))
    (chain (logxor (* i chunk-x) (* j chunk-z) seed)
      (set-seed! <>))
    (< (next-double) 0.004)))

(def (teasure-chunk? seed chunk-x chunk-z)
  (chain (+ (* 341873128712 chunk-x) (* 132897987541 chunk-z ) seed TREASURE_CONFIG.salt)
    (set-seed! <>))
  (< (next-float) 0.01))