#!/usr/bin/env gxi

(import :gerbil/expander)
(import :gerbil/gambit/ports)
(import :std/misc/ports)


;; (import ./lib/test)

;; (def current-dir (path-normalize (path-directory (this-source-file))))
(def gerbil-lib-dir (path-expand "lib" (getenv "GERBIL_PATH")))

;; (call-with-input-u8vector
;;  #u8(1 2 3)
;;  (lambda (i) (call-with-output-file "/tmp/foo" (cut copy-port i <>))))

(def (write-u8-to-file out-file u8)
  "Serialize object to a file"
  (call-with-output-file out-file (cut write-u8vector u8 <>)))

;; read-u8
;;   write-u8
;;   read-subu8vector
;;   write-subu8vector

(def (test _)
  (or unbound-var))

(_gx#load-expander!)

(def (main . args)
  (import-module "test")
  (test-main #f)
  (displayln "just loaded test module " args)
  ;; (write-u8-to-file "/tmp/foo" #u8(1 2 3))
  ;; (displayln (read-file-u8vector "/tmp/foo"))
  )

;; (maybe (you (like (this (even (better))))))
