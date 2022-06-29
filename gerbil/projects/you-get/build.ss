#!/usr/bin/env gxi

(import :std/make)

;; the source directory anchor
(def srcdir
  (path-normalize (path-directory (this-source-file))))
(def mods-dir (path-expand "mods" srcdir))

;; the build specification
(def lib-build-spec
  '("utils"
    "common"))

(def mod-build-spec
  (directory-files "mods"))

(def bin-build-spec
  `((exe: "you-get"
          ;; "-cc-options" ,(cppflags "zlib" "")
          ;; "-ld-options" ,(ldflags "zlib" "-lz")
          )))

;; the main function of the script
(def (main . args)
  (match args
    (["mods"]
     (make srcdir: mods-dir
           bindir: mods-dir
           optimize: #t
           debug: #f ;; disable debugger introspection for library modules
           static: #t
           mod-build-spec))
    (["lib"]
     ;; this action builds the library modules -- with static compilation artifacts
     (make srcdir: srcdir
           bindir: srcdir
           optimize: #t
           debug: 'src
           static: #t ;; generate static compilation artifacts; required!
           build-deps: "build-deps-lib"
           lib-build-spec))
    (["bin"]
     (make srcdir: srcdir
           bindir: srcdir
           optimize: #t
           debug: #f
           static: #t ;; generate static compilation artifacts; required!
           build-deps: "build-deps-bin" ; importantly, pick a file that differs from above
           ;; prefix: "you-get"
           bin-build-spec))

    ;; this is the default action, builds libraries and executables
    ([]
     (main "lib")
     ;; (main "mods")
     ;; (main "bin")
     )))