#!/usr/bin/env gxi

(import :std/make)

;; the library module build specification
(def lib-build-spec
  '("test"))

(def bin-build-spec
  '((static-exe: "main")))

;; the source directory anchor
(def srcdir
  (path-normalize (path-directory (this-source-file))))

;; the main function of the script
(def (main . args)
  (match args
    (["lib"]
     ;; this action builds the library modules -- with static compilation artifacts
     (make srcdir: srcdir
           bindir: srcdir
           optimize: #t
           debug: 'src ; enable debugger introspection for library modules
           static: #t ; generate static compilation artifacts; required!
           ;; prefix: "parser"
           ;; build-deps: "build-deps" ; this value is the default
           lib-build-spec))
    ;; this is the default action, builds libraries and executables
    (["bin"]
     (make srcdir: srcdir
           bindir: srcdir
           optimize: #t
           debug: #f                  ; no debug bloat for executables
           static: #t ; generate static compilation artifacts; required!
           prefix: "test"
           build-deps: "build-deps-bin" ; importantly, pick a file that differs from above
           bin-build-spec))
    ([]
     (main "lib")
     ;; (main "bin")
     )))