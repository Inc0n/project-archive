#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import :std/make)


(def lib-build-spec
  '("utils"))

(def bin-build-spec
  '((exe: "simpled")))

(def srcdir
  (path-normalize (path-directory (this-source-file))))

(def (main . args)
  (match args
    (["lib"]
     ;; this action builds the library modules -- with static compilation artifacts
     (make srcdir: srcdir
           bindir: srcdir
           optimize: #t
           debug: 'src ; enable debugger introspection for library modules
           static: #t ; generate static compilation artifacts; required!
           ;; build-deps: "build-deps" ; this value is the default
           lib-build-spec))
    (["bin"]
     (make srcdir: srcdir
           bindir: srcdir
           bin-build-spec))
    ([]
     (main "lib")
     (main "bin"))))
