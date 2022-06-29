#!/usr/bin/env gxi

(import :std/make)

;; the source directory anchor
(def srcdir
  (path-normalize (path-directory (this-source-file))))
;; (def mods-dir (string-append srcdir "mods"))

;; the build specification
(def lib-build-spec
  '("list"
    "hash-struct"))

;; the main function of the script
(def (main . args)
  (match args
    (["lib"]
     ;; this action builds the library modules -- with static compilation artifacts
     (make srcdir: srcdir
           bindir: srcdir
           optimize: #t
           debug: #f
           static: #t ; generate static compilation artifacts; required!
           ;; build-deps: "build-deps" ; this value is the default
           ;; prefix: "you-get"
           lib-build-spec))
    ([]
     (main "lib"))))