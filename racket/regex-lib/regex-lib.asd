#|
Copyright (c) 2019, inc0ns1st3nt.

This program is free software; you can redistribute it and/or modify
it under the terms of the Lisp Lesser General Public License version 2, as published by
the Free Software Foundation and with the following preamble:
http://opensource.franz.com/preamble.html

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Lisp Lesser General Public License for more details.
|#
(in-package :cl-user)

(defpackage :regex-lib-asd
  (:use :cl :asdf))

(in-package :regex-lib-asd)

(defsystem :regex-lib
  :version "0.0.1"
  :description "regular expression library"
  :author "inc0ns1st3nt"
  :components ((:module "src"
		                :components
		                ((:file "regex" :depends-on ("parser" "core"))
		                 (:file "parser" :depends-on ("core"))
		                 (:file "core")
                         (:file "test")
                         ;; (:module "compile"
                         ;;          :components
                         ;;          ((:file "package")
                         ;;           ;; (:file "regex" :depends-on ("compiler" "core"))
                         ;;           (:file "compiler" :depends-on ("core"))
                         ;;           (:file "core")))
                         ))))