
(import :parser/parser)
(import :std/misc/repr)


(export regex)

(def-parse-rule (escape-reader)
  (lambda (_stm _token _acc)
    (displayln "escape-reader: unimplemented")))

(def-token 'front '(token: #\^))
(def-token 'back '(token: #\$))
(def-token 'any '(token: #\.))

(def-token 'repeat
  '(token: (#\* #\+ #\?) parse-rules: pop-before))

(def-token 'match
  '(token: #\[ parse-rules: (read-until #\]))
  '(token: #\( parse-rules: (read-until #\)))
  '(token: #\{ parse-rules: pop-before (read-until #\})))

(def-token 'brkt-delim
  '(token: (#\] #\} #\)) parse-rules: ignore))

(def-token 'or
  '(token: #\| parse-rules: pop-before next-token))

(def-token 'escape
  '(token: #\\ parse-rules: escape-reader))

(def-token 'string
  '(token: #t))

(def regex parse-it)

(def (test)
  (regex "[123]+|d+"))

;; (def (main . args)
;;   (def parse-org-cmd
;;     (command 'parse help: "parse org file from limneos.net"
;;              (argument 'file-path help: "path to org file"
;;                        ;; (flag 'force "-f" "--force" help: "override existing header")
;;                        )))
;;   (def test-regex-cmd (command 'test help: "test regex parsing"))
;;   (def gopt (getopt parse-org-cmd
;;                     test-regex-cmd))
;;   (try
;;    (let ((values cmd opt) (getopt-parse gopt args))
;;      (case cmd
;;        ('parse (parse-org
;;                 (or (hash-get opt 'file-path)
;;                     +test-string+)))
;;        ('test (test))))
;;    (catch (getopt-error? exn)
;;      (getopt-display-help exn "parse-org" (current-error-port))
;;      (exit 1))
;;    (catch (e)
;;      (display-exception e (current-error-port))
;;      (exit 2))))
(print-representation (test))

[[['or . #\|]
  [['repeat . #\+] [['match . #\[] [['string . "123"]]]]
  [['repeat . #\+] ['string . "d"]]]]
