#!/usr/bin/env gxi

(import :parser/parser)
(import :std/getopt) ;; unix opt
(import :std/sugar) ;; try
(import :gerbil/gambit/exceptions) ;; display-exception
(import :std/misc/repr)

(export parse-org main)

;; utils

(def (tag tag-name token)
  [tag-name token])

(def (tagged? x lst)
  (and (pair? lst)
       (eq? (car lst) x)))

;; token utils

(def (string-token? x)
  (eq? (car x) 'string))

;;

(def (get-next-if-string strm)
  (val-op-token? {peek-chr strm}))

(def (to-string x)
  (match x
    ((? string?) x)
    ((? char?) (string x))
    ((? number?) (number->string x))
    (else (error "to-string unexpected datum: " x))))

(def (append-to-prev-if-string strm token acc)
  (let (prev-token (car acc))
    (displayln "token: " token)
    (cond ((tagged? 'string prev-token)
           (values #f
                   (cons (cons 'string
                               (string-append (token-val prev-token)
                                              (to-string (token-val token))
                                              (if (peek-token-type-eq? strm 'string)
                                                (next-token strm tokenize?: #f)
                                                "")))
                         (cdr acc))))
          (else (values token acc)))))

(def (parse-bold strm token acc)
  (values (tag 'bold (token-val token))
          acc))

(def (read-heading->level strm (base-level 0))
  (let aux ((level base-level))
    (match {peek-chr strm}
      (#\*
       {next-chr strm}
       (aux (1+ level)))
      (#\space
       level)
      (else
       {back-chr strm #f}
       level))))

(def (parse-heading strm acc base-level)
  ;; (displayln {sub-seq strm (@ strm index) (+ (@ strm index) 10)})
  (values (op-token-add-arg
           (make-op-token
            (tag 'heading
                 (read-heading->level strm base-level)))
           (next-token-until strm #\newline consume: #t))
          acc))

(def-parse-rule (parse-heading-or-bold)
  (lambda (strm token acc)
    (def start-of-line? null?)
    (if (start-of-line? acc)
      (parse-heading strm acc 1)
      (parse-bold strm token acc))))


(def-parse-rule (parse-maybe-heading)
  (lambda (strm token acc)
    (match {peek-chr strm}
      ((and (? char?) #\*)
       (parse-heading strm acc 0))
      (else
       ;; TODO - add newline to previous string
       (append-to-prev-if-string strm (car token) acc)))))

(def-parse-rule (read-if-attri-until delim)
  (lambda (strm token acc)
    (match {peek-chr strm}
      ((and char? (not #\space))
       (op-token-add-arg token
                         {read-until strm delim #t}))
      (else
       (append-to-prev-if-string strm (car token) acc)))))

(def-token 'newline '(token: #\newline parse-rules: parse-maybe-heading))
(def-token 'star '(token: #\* parse-rules: parse-heading-or-bold))

(def-token 'underline '(token: #\_ parse-rules: (read-if-attri-until #\_)))
(def-token 'strike-through '(token: #\+ parse-rules: (read-if-attri-until #\+)))

(def-token 'meta ;; comments?
  '(token: #\# parse-rules: (read-until #\newline)))

(def-token 'string
  '(token: #t))

(def parse-org parse-it)

(def +test-string+ #<<EOF
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
        (displayln (parse-org +test-string+)))))
   (catch (getopt-error? exn)
     (getopt-display-help exn "parse-org" (current-error-port))
     (exit 1))
   (catch (e)
     (display-exception e (current-error-port))
     (exit 2))))


(((heading 1) ((string .  meat)))
 ((heading 2) ((string .  add salt + soy sauce + white pepper + 料酒 + mix well)))
 ((heading 2) ((string .  egg white + mix well)))
 ((heading 2) ((string .  corn flour + mix well + oil + mix well)))
 (newline .)
 ((heading 1) ((string .  水超一下食蔬)))
 (newline .)
 ((heading 1) ((string .  煎土豆)))
 (newline .)
 (string . 冷水
 大火烧开, 转小火煮五分钟))

(((heading 1) ((string .  meat)))
 (bold ()) (bold ()) (string .  add salt + soy sauce + white pepper + 料酒 + mix well) ((heading 2) ((string .  egg white + mix well
                                                                                                             ))) (bold ()) (bold ()) (string .  corn flour + mix well + oil + mix well
) ((heading 1) ((string .  水超一下食蔬
))) ((heading 1) ((string .  煎土豆
))) (string . 冷水
大火烧开, 转小火煮五分钟))