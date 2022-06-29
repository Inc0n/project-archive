
;; (import :std/parser)

(import :parser/utils)
(import :parser/stream)

(import :std/srfi/143) ;; fx-greatest

(import :std/sugar) ;; is
(import :std/pregexp) ;; regex

(import :std/misc/list) ;; push!
(import :std/misc/func) ;; compose1
(import :std/misc/ports) ;; read-file-string

(export get-lexer
        lex-it lex-string lex-file
        def-token next-token token-tag
        ;;
        set-lexer-trace-on set-lexer-debug-on)

;;; utils

(def *lexers* [])

(def (all-lexers) *lexers*)
(def (set-lexer! name lexer)
  (let (lexers
        (member (compose1 (is name) token-tag)
                *lexers*))
    (if lexers
      (set! (car lexers) lexer)
      (push! lexer *lexers*))))
(def (get-lexer name)
  (find (compose1 (is name) token-tag)
        *lexers*))

;;; EBNF / regex like sexp pattern matching DSL

;; debugger options

(def-debug-option *trace* set-lexer-trace-on) ;; trace proc path and result
(def-debug-option *debug* set-lexer-debug-on) ;; trace proc path and result
;; (def *trace* #f)
;; (def (set-lexer-trace-on) (set! *trace* #t))

(def (debugln . args)
  (when *debug*
    (apply displayln args)))

;; lexer DSL


(def (compile-not procs)
  (lambda (strm c)
    (every (lambda (proc)
             (not (proc strm c)))
           procs)))

(def (compile-and procs)
  (lambda (strm c)
    (every (lambda (proc)
             (proc strm c))
           procs)))

(def (compile-or procs)
  (lambda (strm c)
    (any (lambda (proc)
           (proc strm c))
         procs)))

(def (compile-char exp)
  (lambda (_strm c)
    (and (char? c) (char=? c exp))))

(def (compile-rep lexemep min (max fx-greatest))
  (let (precedencep (compile-precedency))
    (lambda (strm c)
      (let loop ((i 0) ;; number of successful matches
                 (c c))
        (cond ((precedencep strm c) =>
               (lambda (next-token)
                 (debugln "matched " c " to another " (token-tag next-token))
                 {back-char strm c}
                 (if (or (fx< i min) (fx> i max))
                   #f
                   #t)))
              ({empty? strm} #t)
              ((lexemep strm c)
               (loop (fx1+ i) {next-char strm}))
              ((or (fx< i min) (fx> i max)) #f)
              (else #t))))))

(def (compile-dsl exp)
  (match exp
    ([(or '? '0+) exp] (compile-rep (compile-dsl exp) 0))
    (['1+ exp] (compile-rep (compile-dsl exp) 1))
    (['+ exp min] (compile-rep (compile-dsl exp) min))
    (['+ exp min max] (compile-rep (compile-dsl exp) min max))
    ;;
    (['not . exp] (compile-not (map compile-dsl exp)))
    (['or . exp] (compile-or (map compile-dsl exp)))
    (['and . exp] (compile-and (map compile-dsl exp)))
    ;; ((and lst list? (every-of char?)) (compile-or (map compile-char exp)))
    ('any (lambda (_strm _c) #t))
    ((? char?) (compile-char exp))
    (else (error "malformed exp: " exp))))

;;

(def (compile-precedency)
  (let (token-precedencies (list-copy (all-lexers)))
    (lambda (strm c)
      (find (lambda (token)
              ((token-matchp token) strm c))
            token-precedencies))))

;;; token

(defstruct token (tag matchp)
  constructor: :init!)

(defmethod {:init! token}
  (lambda (self tag match-exp)
    (set! (token-tag self) tag)
    (set! (token-matchp self) (compile-dsl match-exp))))

;;; - define token`

(defsyntax (def-token stx)
  (syntax-case stx ()
    ((_ name lexeme-exp)
     #'(set-lexer! name (make-token name `lexeme-exp)))))

(def (next-token strm)
  (def (tokenize token val)
    (tag (token-tag token) val))
  (let (start (@ strm index))
    (let aux
        ((tokens (all-lexers)))
      (if (or {empty? strm}
              (null? tokens))
        (error "unmatched string: " (@ strm seq))
        (let ((c {next-char strm})
              (token (car tokens)))
          (debugln "trying to match " c " to " (token-tag token))
          (cond (((token-matchp token) strm c) ;; the order of this predicate has importance
                 (debugln "gotten a match " c " to " (token-tag token))
                 (tokenize token (if (= 1 (- (@ strm index) start))
                                   c
                                   {sub-seq strm start})))
                (else
                 {reset-loc strm start}
                 (aux (cdr tokens)))))))))

;;;

(def (reduce-strm fn seq)
  "streamed reduce variant for sequence"
  (let iter ((strm (make-str-stream seq))
             (acc []))
    (if {empty? strm}
      (reverse acc)
      (iter strm
            (fn strm acc)))))

;;; lex interface

(def (lex-string str)
  "parse string into structured tokens"
  (reduce-strm (lambda (strm acc)
                 (cons (next-token strm) acc))
               str))

(def (lex-file str)
  (if (eq? (file-type str) 'regular)
    (lex-string (read-file-string str))
    (error "lexer: path does not points to a regular file")))

(def (lex-it str)
  (if (string? str)
    (if (file-exists? str)
      (lex-file str)
      (lex-string str))
    (error "lexer: expected string not " str)))