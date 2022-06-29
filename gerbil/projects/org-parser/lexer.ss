#!/usr/bin/env gxi

(import :org-parser/utils)

(import :std/misc/repr) ;; print-representation
(import :std/pregexp ;; regex
        :std/format ;; format
        :std/sugar)
(import :std/misc/func) ;; compose1
(import :std/misc/ports) ;; read-line
;; (import :std/srfi/13) ;; string-contains

(export lexify)

;; utils

(def car?
  (match <>
    ([x] x)
    (x x)))

(def (pair xlst ylst)
  (map list xlst ylst))

;;

(def (compose-pregex . pat)
  (let (pat (pregexp (apply string-append pat)))
    (lambda (line)
      (match (pregexp-match-positions pat line)
        ([[start . match-end] . rest-pos] ; matched X group positions
         (values (substring line 0 start)
                 (map (match <>
                        ([x . y] (substring line x y))
                        (x x))
                      rest-pos)
                 (substring line match-end (string-length line))))
        (#f #f)                         ; no match
        (x (error "pregexp-match unexpected pat for result" pat x))))))

(def status-regexp "(TODO|DONE|WAITING|PROJECT|STARTED|SOMEDAY|CANCELLED)?[ ]*")
(def text-regexp "(.*)$")

(def headingp (compose-pregex "^(\\*+)[ ]*"
                              status-regexp
                              text-regexp))

(def headerp (compose-pregex "^#\\+"
                             "(\\w+):"
                             "[ ]*"
                             text-regexp))

(def commentp (compose-pregex "^[ ]*#[^ ]" text-regexp))

;;
(def (text-attri-compose delim)
  (compose-pregex (string-append "(" delim ")")
                  "([^ ]+)"
                  "\\1"))

(def +body-lexer+
  [[(text-attri-compose "\\*") 'bold]
   [(text-attri-compose "\\+") 'strike-through]
   [(text-attri-compose "_")   'underline]
   [(lambda (line) (values "" [line] "")) 'text]])

(def (lexify-line-body line)
  (let loop ((lexer +body-lexer+)
             (line line)
             (acc []))
    (if (string-empty? line)
      (if (null? acc)
        [['newline]]
        (reverse acc))
      (match lexer
        ([lex . rest-lexer]
         (match ((lex-regexp lex) line)
           ((values prev match rest)
            (append (if (string-empty? prev)
                      (reverse acc)
                      (loop rest-lexer prev acc))
                    (loop lexer rest [(lex-token lex match)])))
           (else (loop rest-lexer line acc))))
        ([] 
         (error "lexify-line-body: unmatched " line))))))

(def +lexer+
  [[headingp 'heading (cut pair '(level: status: text:) <>)]
   [headerp 'header]
   [commentp 'comment]])

(def (lex-regexp lex) (car lex))
(def (lex-tag lex) (cadr lex))
(def (lex-attri-proc lex) (caddr lex))

(def (lex-token lex match)
  (if (null? (cddr lex))
    (make-elmt (lex-tag lex) match)
    (make-elmt (lex-tag lex) ((lex-attri-proc lex) match) [])))

(def (lexify-line line)
  (let loop ((lexer +lexer+))
    (match lexer
      ([] (lexify-line-body line))
      ([lex . lexer]
       (match ((lex-regexp lex) line)
         ((values "" match "")
          [(lex-token lex match)])
         (else
          (loop lexer)))))))

;;

(def (lexify-lines lines)
  (match lines
    ([] [])
    ([line . lines]
     (append (lexify-line line)
             (lexify-lines lines)))))
;; (def (lexify-lines lines (acc []))
;;   (match lines
;;     ([] (reverse acc))
;;     ([line . lines]
;;      (lexify-lines lines
;;                    (append (lexify-line line) acc)))))

(def (lexify-port port)
  (let (line (read-line port))
    (match line
      ((? eof-object?) [])
      (else (append (lexify-line line)
                  (lexify-port port))))))

;; (def (lexify-port port (acc []))
;;   (let (line (read-line port))
;;     (match line
;;       (#!eof (reverse acc))
;;       (else (lexify-port port
;;                          (append (lexify-line line) acc))))))

(def (lexify x)
  (match x
    ((? list?) (lexify-lines x))
    ((? input-port?) (lexify-port x))
    ((? string?)
     (if (file-exists? x)
       (if (eq? (file-type x) 'regular)
         (call-with-input-file x lexify-port)
         (error "path does not point to a regular file " x))
       (lexify-lines (string-split x #\newline))))))