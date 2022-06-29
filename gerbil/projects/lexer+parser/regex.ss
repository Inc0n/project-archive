
(import :parser/lexer)
(import :parser/parser)
(import :std/misc/repr)

(export regex)

;; (set-lexer-trace-on)
;; (set-lexer-debug-on)

(def-token 'or #\|)

(def-token 'front #\^)
(def-token 'back #\$)
(def-token 'any #\.)

(def-token 'repeat (or #\* #\+ #\?))

(def-token 'match #\[)
(def-token 'group #\()
(def-token 'brkt #\{)

(def-token 'match-delim #\])
(def-token 'brkt-delim (or #\} #\)))

(def-token 'escape (and #\\ any))

(def-token 'string (1+ any))

(def (test)
  (lex-it "[123]+|d+"))

;; parser

(set-parser-debug-on)

(def-parser
  ;; (lex: (repeat match match-delim))
  (repeat : (lex: repeat))
  (Match  : #\[ ($1 (0+ Pat)) #\] -> ['Match $1])
  (Repeat : ($2 Pat) ($1 repeat)  -> ['Rep $1 $2])
  (Or     : ($1 Pat) #\| ($2 Pat) -> ['Or $1 $2])
  (Pat    : any))


(displayln (map parser-name (all-parsers)))
;;

(print-representation (test))
(display #\newline)
(print-representation (parse-it (test)))
(display #\newline)

;; lexer
[#\[ ['string "123"] #\] ['repeat "+"]
 ['or "|"]
 ['string "d"] ['repeat "+"]]

;; parser
[[['or . #\|]
  [['repeat . #\+] [['match . #\[] [['string . "123"]]]]
  [['repeat . #\+] ['string . "d"]]]]

[[['@list ['quote 'Match] '$1]]
 ['repeat ['repeat #\+]]
 ['Pat ['or #\|]]
 [['@list ['quote 'Rep] '$1 '$2]]]