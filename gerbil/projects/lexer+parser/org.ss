
(import :parser/lexer)
(import :parser/parser)
(import :std/misc/repr)

(export org)

;; (set-lexer-trace-on)
;; (set-lexer-debug-on)

(def-token 'SPC #\space)

;; (def-token 'stars #\* :text #t)
(def-token 'text-attri (or #\* #\_ #\+))
(def-token 'text-attri (or #\* #\_ #\+))
(def-token 'text-attri #\.)

(def-token 'repeat (or #\* #\+ #\?))

(def-token 'match #\[)
(def-token 'group #\()
(def-token 'brkt #\{)

(def-token 'match-delim #\])
(def-token 'brkt-delim (or #\} #\)))

(def-token 'escape (and #\\ any))

(def-token 'string (1+ any))

(def +test-string+ #<<EOF
* meat
** add salt + say sauce + white pepper + 料酒 + mix well
** egg white + mix well
** corn flour + mix well + oil + mix well

* 水超一下食蔬

* 煎土豆
  冷水
  大火烧开, 转小火煮五分钟
EOF
)

(def (test)
  (lex-it +test-string+))

;; parser

(set-parser-debug-on)

(def-parser
  ;; (lex: (repeat match match-delim))
  (repeat : (lex: repeat))
  (Heading : ($1 Level i) SPC (? ($2 Status) SPC) ($3 Line) ($4 (or (Heading (> i))
                                                                    ListItem
                                                                    Line))*
           -> ['Heading $1 $2 $3 $4])
  (ListItem : Indent SPC ($1 ListBullet) ($2 Line) -> ['ListItem $1 $2])
  ;; literal tokens
  (Status     : (or "TODO" "DONE" "WAITING" "PROJECT" "STARTED" "SOMEDAY" "CANCELLED"))
  (ListBullet : (or "-" "+" "*")))


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