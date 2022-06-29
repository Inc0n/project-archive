#lang :std/parser/grammar

(import :std/parser/base
        :std/srfi/13) ;; string-count

;; clojure? org EBNF implementation
;; https://gist.github.com/alexkehayias/634b7ea74b034ec22dc0ac127d6d01d1

%%parser parse-org lexer: lex-org

Org <- (Expr $1)*
    => [$1 ...]

Expr    <- Heading | Line

Heading <- (Level $1) (Status $2) SPC (Line $3) (Content $4)*
        => ['heading [level: $1 status: $2 title: $3] $4 ...]
        |  (Level $1) (Line $2) (Content $3)*
        => ['heading [level: $1 title: $2] $3 ...]

Content <- ListItem | Line | Heading

ListItem <- (Indent $3) SPC (ListBullet $1) (Line $2)
         => ['item $1 $2]

;; literal tokens
Status <- 'TODO' | 'DONE' | 'WAITING' | 'PROJECT' | 'STARTED' | 'SOMEDAY' | 'CANCELLED'
ListBullet <- '-' | '+' | '*'

;; lexer tokens
Indent  <- %
Level   <- %
SPC     <- %
Line    <- %

EOF     <- $$

%%lexer lex-org

\s+                             -> (Indent (string-length @@))   ; indent
\*+[\ ]                         -> (Level (string-count @@ #\*)) ; level
[\ ]                            -> (SPC " ")                     ; space
;; [\ \t\n]+                       -> $                             ; whitespace
;; \;[^\n]*\n                      -> $                             ; comments
;; "([^"]|\\")*"                   -> (String (string-e @@ @loc))   ; strings
.+\n                            -> (Line @@)                     ; line