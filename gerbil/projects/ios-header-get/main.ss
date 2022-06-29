#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import :gerbil/gambit/ports) ;; write-u8vector
(import :std/format ;; format
        :std/getopt ;; unix opt
        :std/pregexp ;; regex
        :std/sugar ;; try
        :std/iter ;; for
        :std/srfi/13 ;; string-suffix?
        :std/net/request ;; http requests
        :std/misc/ports)

(export main)

(def (get-line-first-word line)
  (let* ((line (string-trim line #\space))
         (index (string-index line #\space)))
    (if index
      (values (substring line 0 index)
              (substring line (1+ index) (string-length line)))
      (values line ""))))

;;; utils

(def (download-url! url file-path)
  (let ((request (http-get url)))
    (displayln request)
    (let ((u8 (request-text )))
      (displayln u8)
      (call-with-output-file file-path (cut write-u8vector u8 <>)))))

(def (append-header-suffix header)
  (if (string-suffix? ".h" header)
    header
    (string-append header ".h")))

(def (guess-framework header)
  (error "need to pass in a framework path"))

;;; getter
(def +url-search-format+ "https://developer.limneos.net/search.php?keyword=~a")
(def +url-header-format+ "https://developer.limneos.net/headers/13.1.3/~a/Headers/~a")
(def (get-search-url query)
  (format +url-search-format+ query))
(def (get-header-url framework header)
  (format +url-header-format+ framework header))

(def (get-theos-env-var)
  (or (getenv "THEOS")
      (error "$THEOS not defined")))
(def +theos-path-format+ (string-append (get-theos-env-var) "/include/~a/~a"))
(def (get-theos-header-path framework header)
  (format +theos-path-format+ framework header))

(def (you-get framework header force: (override #f))
  (let* ((framework (or framework (guess-framework header)))
         (header (append-header-suffix header))
         (header-path (get-theos-header-path framework header)))
    (when (and (file-exists? header-path)
               (not override))
      (error "cannot override " header-path " without '-f'"))
    (create-directory* (path-directory header-path))
    (displayln "getting " framework "/" header)
    (download-url! (get-header-url framework header)
                   (get-theos-header-path framework header))))

(def (parse-header file)
  (def (re-match-include-header-exp exp)
    (alet (x (pregexp-match "#include <(\\w+)/(\\w+\\.h)>"
                            exp))
      ;; keep only the group matched parts
      (cdr x)))
  (filter-map re-match-include-header-exp
              (read-file-lines file)))

(def (you-get-file file)
  (for ([framework header] (parse-header file))
    (displayln "found " framework "/" header)
    (you-get framework header)))

;;; command line

(def (opt-get table key)
  (hash-get table key))
(defalias opt-get? hash-get)

;; (def (session gopt)
;;   "provides the repl interface for `you-get'"
;;   (chain (read-line)
;;     (string-split <> #\space)
;;     (you-get-eval gopt <>)))

(def +file-name+ "main")

(def (you-get-eval gopt args)
  (let ((values cmd opt) (getopt-parse gopt args))
    (case cmd
      ((get) (you-get (opt-get opt 'framework)
                      (opt-get opt 'header)
                      force: (opt-get? opt 'force)))
      ((parse-get) (you-get-file (opt-get opt 'path)))
      ;; ((session) (session gopt))
      ((help) (getopt-display-help-topic gopt
                                         (opt-get? opt 'command)
                                         +file-name+)))))

(def (main . args)
  (def you-get-cmd
    (command 'get help: "get ios header from limneos.net"
             (argument 'framework help: "url to you-get")
             (argument 'header help: "url to you-get")
             (flag 'force "-f" "--force" help: "override existing header")))
  (def parse-get-cmd
    (command 'parse-get help: "get all the headrs in the file"
             (argument 'path help: "path to the file to parse")))
  (def help-cmd
    (command 'help help: "display this help message"
             (optional-argument 'command value: string->symbol)))
  (def gopt (getopt you-get-cmd
                    parse-get-cmd
                    help-cmd))
  (try
   (you-get-eval gopt args)
   (catch (getopt-error? exn)
     (getopt-display-help exn +file-name+ (current-error-port))
     (exit 1))
   (catch (e)
     (display-exception e (current-error-port))
     (exit 2))))