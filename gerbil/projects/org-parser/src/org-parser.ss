
(import :std/parser
        :std/parser/base
        <expander-runtime>
        "org-EBNF")

;; (export (import: :gerbil/core))

(def +test-string+ #<<EOF
* meat

EOF
)

(def +test-full-string+ #<<EOF
* meat
** add salt + soy sauce + white pepper + 料酒 + mix well
** egg white + mix well
** corn flour + mix well + oil + mix well

* 水超一下食蔬

* 煎土豆
冷水
大火烧开, 转小火煮五分钟
EOF
)

(def (test-parse)
  (let (progn (parse-org +test-string+))
    (case (token-t progn)
      ((Org)                            ; we parsed a program
       (displayln [(syntax->datum (token-e progn))]))
      (($$)                             ; the empty program
       [])
      (else            ; that really shouldn't happen with our grammar
       (displayln "error")))))

(def (token->datum t)
  (syntax->datum (token-e t)))

(def (test-lex)
  (displayln (token->datum
              (token-stream-next (lex-org +test-string+)))))

(test-lex)