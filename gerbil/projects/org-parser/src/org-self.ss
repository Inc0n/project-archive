#!/usr/bin/env gxi

;; (import :parser/parser)
(import :std/getopt) ;; unix opt
(import :std/pregexp) ;; regex
(import :std/sugar) ;; try
;; (import :std/srfi/13) ;; string-contains
(import :std/misc/ports) ;; read-file-lines
(import :std/misc/repr) ;; print-representation
(import :gerbil/gambit/exceptions) ;; display-exception

(export parse-org main)

;; utils

(def (tag tag-name token)
  [tag-name token])

;; string utils

;; (def (string-slice str1 str2 (start 0))
;;   (let (i (string-contains str1 str2 start))
;;     (if i
;;       (values (substring str1 0 i)
;;               (substring str1
;;                          (+ i (string-length str2))
;;                          (string-length str1)))
;;       (values str1 ""))))

(def (string-slice str1 char (start 0))
  (let (i (string-index str1 char start))
    (if i
      (values (substring str1 0 i)
              (substring str1
                         (fx1+ i)
                         (string-length str1)))
      (values str1 ""))))

(def (make-node atag text body)
  (list (tag atag text) body))

;;

(def starts-with-*? (cut pregexp-match "^\\*" <>))
(def starts-with-*-more? (cut pregexp-match "^\\*+ " <>))
(def (heading-text line)
  (substring line
             (1+ (or (string-index line #\space)
                     -1))
             (string-length line)))
(def (heading-level line)
  (substring line 0 (string-index line #\space)))

(def starts-with-+? (cut pregexp-match "^\\+[^ ]" <>))
(def starts-with-_? (cut pregexp-match "^_[^ ]" <>))
(def starts-with-#? (cut pregexp-match "^#[^ ]" <>))
(def (attri-text line delim)
  (substring line
             1
             (string-index line delim 1)))

;; lexify

(def (debugln . args)
  (apply displayln args))

(def (lex-text-line name line delim)
  (let ((values text line) (string-slice line delim))
    (cons (tag name text)
          (lexify-line line))))

(def (lex-text-w-attri name line delim)
  (let ((values text line) (string-slice line delim))
    (debugln "text (" delim "): " line)
    (cons (tag name text)
          (lexify-line line))))

(def (lexify-line line)
  (match line
    ((? starts-with-*-more?)
     (debugln "lexify heading: " line)
     ['heading (heading-level line) (heading-text line)])
    ((? starts-with-*?)
     (if (null? acc)
       (tag 'heading (heading-level line)
            (lexify-line (heading-text line)))
       (lex-text-w-attri 'bold line #\*)))
    ((? starts-with-_?)
     (debugln "lexify _: " line)
     (lex-text-w-attri 'underline line #\_))
    ((? starts-with-+?)
     (debugln "lexify +: " line)
     (lex-text-w-attri 'strike-through line #\+))
    ((? starts-with-#?)
     (let ((values _ text) (string-slice line #\#))
       (tag 'comment text)))
    ((or "\n" (? string-empty?)) ['newline #\newline])
    (else (tag 'String line))))

(def (lexify lines)
  (let %lexify ((lines lines) (acc []))
    (match lines
      ([line . lines]
       (%lexify lines
                (cons (lexify-line line) acc)))
      ([] (reverse acc)))))

;;
(def (tagged? tag x)
  (and (pair? x)
       (eq? (car x) tag)))

(def (orgify tokens)
  (match (car tokens)
    (['heading level heading-text]
     )
    ((? starts-with-*?)
     (if (null? acc)
       (tag 'heading (heading-level line)
            (lexify-line (heading-text line)))
       (lex-text-w-attri 'bold line #\*)))
    ((? starts-with-_?)
     (debugln "lexify _: " line)
     (lex-text-w-attri 'underline line #\_))
    ((? starts-with-+?)
     (debugln "lexify +: " line)
     (lex-text-w-attri 'strike-through line #\+))
    ((? starts-with-#?)
     (let ((values _ text) (string-slice line #\#))
       (tag 'comment text)))
    ((or "\n" (? string-empty?)) ['newline #\newline])
    (else (tag 'String line))))

(def (parse tokens)
  (let aux ((tokens tokens)
            (acc []))
    (if (null? tokens)
      (reverse acc)
      (let ((values new-token tokens)
            (orgify tokens))
        (aux tokens
             (cons new-token acc))))))

(def (parse-org path)
  (lexify ;; (read-file-lines path)
   (string-split path #\newline)))

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

(def (main . args)
  (def parse-org-cmd
    (command 'parse help: "parse org file from limneos.net"
             (argument 'file-path help: "path to org file")
             ;; (flag 'force "-f" "--force" help: "override existing header")
             ))
  ;; (def parse-get-cmd
  ;;   (command 'parse-get help: "get all the headrs in the file"
  ;;            (argument 'path help: "path to the file to parse")))
  ;; (def help-cmd
  ;;   (command 'help help: "display this help message"
  ;;            (optional-argument 'command value: string->symbol)))
  (def test-regex-cmd (command 'test help: "test regex parsing"))
  (def gopt (getopt parse-org-cmd
                    test-regex-cmd))
  (try
   (let ((values cmd opt) (getopt-parse gopt args))
     (case cmd
       ((parse)
        (displayln
         (parse-org
          (or (hash-get opt 'file-path)
              +test-string+))))
       ((test)
        (display (parse-org +test-string+))
        (display #\newline))))
   (catch (getopt-error? exn)
     (getopt-display-help exn "parse-org" (current-error-port))
     (exit 1))
   (catch (e)
     (display-exception e (current-error-port))
     (exit 2))))