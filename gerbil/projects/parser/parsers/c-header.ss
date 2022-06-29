
(import :parser/parser)

(export c-header)


(def-parse-rule (macro-reader)
  (lambda (stm _token _acc)
    (match (next-token strm)
      ())
    (read-until #\newline)
    (displayln "macro-reader: unimplemented")))

;; bin op
(def-token 'char-op
  '(token: (#\^ #\~ #\| #\&) parse-rules: as-is)
  '(token: (#\* #\+ #\? #\!) parse-rules: as-is))

(def-token 'match
  '(token: #\[ parse-rules: (read-until #\]))
  '(token: #\( parse-rules: (read-until #\)))
  '(token: #\{ parse-rules: pop-before (read-until #\})))

(def-token 'delim
  ;; need to implement group prev read, similar to pop-before, but pop group
  '(token: #\, parse-rules: as-is)
  '(token: #\; parse-rules: as-is)
  ;;
  '(token: #\space parse-rules: ignore))

(def-token 'macro
  '(token: #\# parse-rules: macro-reader))

;; (def-token 'brkt-delim
;;   '(token: (#\] #\} #\)) parse-rules: ignore))

(def-token 'string
  '(token: #t))

(def (c-header str)
  (parse str))