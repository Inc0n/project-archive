
(import :std/misc/list) ;; push!
(import :std/misc/func) ;; compose1
(import :std/misc/ports) ;; read-file-string

(import :std/sugar) ;; is

(import :std/srfi/143) ;; fx-greatest

(import :parser/utils) ;; tagged? tag every
(import :parser/lexer)
(import :parser/parser-stream)

(export def-parser
        parse-it
        parser-name
        all-parsers
        compile-parser-exp)

;; debug option

(def-debug-option *debug* set-parser-debug-on) ;; trace proc path and result

(def (debugln . args)
  (when *debug*
    (apply displayln args)))

;;

(def *parsers* [])

(def (all-action-parsers)
  (filter parser-actionp *parsers*))
(def (all-parsers) *parsers*)
(def (set-parser! name parser)
  (let (parsers
        (member (compose1 (is name) parser-name)
                *parsers*))
    (if parsers
      (set! (car parsers) parser)
      (push-back! parser *parsers*))))
(def (get-parser name)
  (find (compose1 (is name) parser-name)
        *parsers*))

;;

(def (always-false _) #f)

(def (compile-rep exp min
                  max: (max fx-greatest)
                  cdr-matchp: (next-matchp always-false))
  (debugln "compile-rep, cdr-matchp: " exp " " next-matchp)
  (let (matchp (compile-parser-exp exp))
   (lambda (strm)
      (let loop ((i 0)) ;; number of successful matches
        (cond ((next-matchp strm) #t)
              ((matchp strm) (loop (fx1+ i)))
              (else (and (fx>= i min) (fx<= i max))))))))

(def (compile-one proc)
  (lambda (strm)
    (let (x {car strm})
      (and (proc x)
           {cdr! strm}
           x))))

(def (compile-symbol exp)
  (let (parserp
        (delay (or (get-parser exp)
                   (error "parser: symbol does not exist " exp))))
    (lambda (strm)
      (debugln exp " -> " (@ strm tokens))
      ((parser-matchp (force parserp)) strm))))

(def (compile-assign matchp var)
  (lambda (strm)
    (cond ((matchp strm) =>
           (lambda (token)
             {set-env-var! strm var token}
             token))
          (else #f))))

(def (compile-or exps)
  (let (exp-procs (map compile-parser-exp exps))
    (lambda (strm)
      (any (lambda (p) (p strm))
           exp-procs))))

(def (compile-parser-exp exp)
  (match exp
    ([lex: lexeme]
     (let* ((token (get-lexer lexeme))
            (tag (token-tag token)))
       (compile-one (cut tagged? tag <>))))
    (['or . exps] (compile-or exps))
    ((and [var exp] (? dsl-assign?))
     (compile-assign (compile-parser-exp exp) var))
    ('any        (compile-one identity))
    ((? char?)   (compile-one (compose1 (is exp) cadr)))
    ((? symbol?) (compile-symbol exp))
    ;;
    ([exp '* . exps]
     (if (dsl-assign? exp)
       (compile-assign (compile-rep (assign-match-exp exp) 0
                                    cdr-matchp: (compile-parser-exp exps))
                       (assign-var exp))
       (compile-rep exp 0 cdr-matchp: (compile-parser-exp exps))))
    ([exp '* . exps] (compile-pair (compile-parser-exp exp) exps))
    (else (error "parser compiler: unrecognized exp: " exp))))

(def (compile-pair exp-proc exps)
  (if (null? exps)
    exp-proc
    (let (cdr-proc (compile-parser-exp exps))
      (lambda (strm)
        (and (exp-proc strm)
             (cdr-proc strm))))))

;;

(def (dsl-assign? x)
  (and (pair? x)
       (dsl-variable? (car x))))
(def assign-var car)
(def assign-match-exp cadr)

(def (dsl-rep? x)
  (and (pair? x)
       (tagged? '0+ x)))
(def dsl-rep-exp cadr)

(def (dsl-variable? sym)
  (and (symbol? sym)
       (string-prefix? "$" (symbol->string sym))))

;; action DSL

(def (compile-action-exp exp)
  (debugln "compile action: " exp)
  (match exp
    (['@list . exp]
     (let (exp-procs (map compile-action-exp exp))
       (lambda (env)
         (map (lambda (p) (p env))
              exp-procs))))
    ([exp] (compile-action-exp exp))
    ((? dsl-variable?) (lambda (env) (get-env-var env exp)))
    (else (lambda (_env) exp))))

;; token in lexer is [tag string/char]

(defstruct parser (name matchp actionp)
  constructor: :init!)

(defmethod {:init! parser}
  (lambda (self name match-exp action-exp)
    (set! (parser-name self) name)
    (set! (parser-matchp self) (compile-parser-exp match-exp))
    (if action-exp
      (set! (parser-actionp self) (compile-action-exp action-exp))
      (set! (parser-actionp self) #f))))

;; def-parser interface

(begin-syntax
  (def (list-split x lst)
    (let aux ((lst lst)
              (acc []))
      (cond ((null? lst)
             (values (reverse acc) #f))
            ((eq? x (car lst))
             (values (reverse acc) (cdr lst)))
            (else
             (aux (cdr lst)
                  (cons (car lst) acc))))))
  ;; (def (make-dsl-var-sym num)
  ;;   (make-symbol '$ num))
  ;; (def (map-times proc lst (base 0))
  ;;   (let aux ((i 0)
  ;;             (lst lst))
  ;;     (if (null? lst)
  ;;       []
  ;;       (cons (proc (car lst) i)
  ;;             (aux (1+ i) (cdr lst))))))
  ;; (def (default-exps name match-exp action-exp)
  ;;   (let (lst (map-times (lambda (exp i) [=: (make-dsl-var-sym i) exp])
  ;;                        match-exp
  ;;                        1))
  ;;     (values lst `[,name ,@(map cadr lst)])))
  (def (expand-def-parser stx)
    (match (syntax->datum stx)
      ([name ': . exps]
       (when (eq? (car exps) '->)
         (error "empty match-exp " exps))
       (let ((values match-exp action-exp)
             (list-split '-> exps)) ;; split up match-exp and action-exp
         `(set-parser! ',name (make-parser ',name ',match-exp ',action-exp))))
      (exp (error "def-parser: expected ('Name : . form) got " exp)))))

(defsyntax (def-parser stx)
  (syntax-case stx ()
    ((_ . clauses)
     (with-syntax ((form
                    `(begin
                       ,@(stx-map expand-def-parser #'clauses))))
       #'form))))

;;

(def (next-sexp parsers tokens)
  (let aux ((parsers parsers))
    (debugln "parsers: " (map parser-name parsers))
    (if (or (null? tokens)
            (null? parsers))
      (error "unmatched tokens" tokens)
      (let* ((parser (car parsers))
             (strm (make-parser-stream tokens)))
        (debugln "BEFORE env: " (parser-stream-env strm))
        (cond (((parser-matchp parser) strm)
               (debugln "AFTER env: " (parser-name parser) " " (parser-stream-env strm))
               (values ((parser-actionp parser) (parser-stream-env strm))
                       (parser-stream-tokens strm)))
              (else
               (aux (cdr parsers))))))))

;;; parse interface

(def (parse-tokens tokens)
  "parse string into structured tokens"
  (def parsers (all-action-parsers))
  (let iter ((tokens tokens)
             (acc []))
    (debugln "iter: " tokens)
    (if (null? tokens)
      (reverse acc)
      (let ((values sexp tokens) (next-sexp parsers tokens))
        (iter tokens (cons sexp acc))))))

(def (parse-file str)
  (if (eq? (file-type str) 'regular)
    (parse-it (read-file-string str))
    (error "parse: path does not points to a regular file")))

(def (parse-it str)
  (cond ((string? str)
         (if (file-exists? str)
           (parse-file str)
           (parse-tokens (lex-string str))))
        ((pair? str)
         (parse-tokens str))
        (else (error "parse: expected string not " str))))