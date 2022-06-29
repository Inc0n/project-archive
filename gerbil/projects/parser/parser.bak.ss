
;; (import :std/parser)
(import :parser/stream)
(import :std/sugar)
(import :std/misc/list) ;; push! 
(import :std/misc/ports) ;; read-file-string

(export get-parse-rule parse-it parse-token def-parse-rule def-token
        next-token next-token-until
        ;; token
        token-val
        ;; op-token
        make-op-token
        op-token-val
        op-token-add-arg
        ;;
        peek-token-type-eq?)

(def (tag x y) (cons x y))

(def *parser-alist* [])

(def (put! key1 key2 val)
  (def x (assoc key1 *parser-alist*))
  (if x
    (let (y (assoc key2 (cdr x)))
      (if y
        (set! (cdr y) val)
        (push! [key2 val] (cdr x))))
    (push! [key1 [key2 val]] *parser-alist*))
  'ok)

(def (get key1 key2)
  (alet* ((x (assoc key1 *parser-alist*))
          (y (assoc key2 (cdr x))))
    (match y
      ((? list?) (cadr y))
      ((? pair?) (cdr y))
      (else (error "unexpected datum " y)))))

;; token

(def token-tag car)
(def token-val cdr)
(def (val-op-token? val)
  (alet (token (val->parse-t val))
    (parse-rules token)))

(def make-op-token list)
(def (op-token-val op-token)
  (token-val (car op-token)))
(def (op-token-add-arg op-token arg)
  (append op-token [arg]))

(def (val->parse-t token-val)
  (or (get 'token token-val)
      (get 'token #t)))

(def (val->parse-rules token-val)
  (parse-rules (val->parse-t token-val)))

(def (peek-token-type-eq? strm name)
  (alet* ((c {peek-chr strm})
          (parse-t (val->parse-t c)))
    (eq? (parse-name parse-t) name)))

;; getting token

(def (next-token strm tokenize?: (tokenize? #t))
  (def (aux start)
    (cond ((not {empty? strm})
           (let (c {next-chr strm})
             (cond ((not (val-op-token? c))
                    (aux start))
                   ((= (1- (@ strm index)) start)
                    c)
                   (else
                    {back-chr strm c}
                    {sub-seq strm start}))))
          ((not (= (@ strm index) start))
           {sub-seq strm start})
          (else #f)))
  (def (tokenize token)
    (cond ((not tokenize?) token)
          (token (tag (parse-name
                       (val->parse-t token))
                      token))
          (else #f)))
  (tokenize (aux (@ strm index))))

(def (next-token-until strm delim consume: (consume #f))
  (let (read {read-until strm delim consume})
    ;; left this let for debugging purpose
    ;; (displayln "read (" delim "): " read)
    (parse-string read)))

;; rules

(defsyntax (def-parse-rule stx)
  (syntax-case stx ()
    ((_
      (name . args)
      (lambda (strm token acc) body ...))
     ;; (unless (= (length arg-list) 3)
     ;;   (error "def-parse-rule lambda function needs to take 3 arguments instead got: " arg-list))
     #'(put! 'parse-rule `name
             (lambda args (lambda (strm token acc) body ...))))))

(def-parse-rule (read-until x)
  (lambda (strm token acc)
    (values (op-token-add-arg
             token
             (next-token-until strm x consume: #t))
            acc)))

;; precedency construct

(def (def-parse-rule-precedence! rule-name rules)
  (get-parse-rule rule-name) ;; will signal error if not found
  (put! 'parse-rule-precedence rule-name rules))
(def (get-rule-precedence rule-name)
  (get 'parse-rule-precedence rule-name))


(def (higher-parse-rule-precedence? c _rule-name)
  "true if token has higher parse rule precedence compare to rule"
  (match (val->parse-rules c)
    ([rule . _]
     (memq (rule-name rule)
           (get-rule-precedence _rule-name)))
    (#f #f)
    (else #t)))

(def-parse-rule (next-token)
  (lambda (strm token acc)
    (let (next (next-token strm))
      (values (op-token-add-arg
               token
               (let (peek {peek-chr strm})
                 (if (and peek
                          (higher-parse-rule-precedence? peek 'next-token))
                   next
                   (car
                    (parse-token strm
                                 (next-token strm)
                                 (list next))))))
              (if (null? acc)
                []
                (car acc))))))

;; need to put this after next-token
;; pop-before
(def-parse-rule-precedence! 'next-token '(next-token until))

(def-parse-rule (pop-before)
  (lambda (_strm token acc)
    (match acc
      ([prev . acc]
       (values (op-token-add-arg token prev)
               acc))
      (else (error "pop-before: empty token acc when parsing " token)))))

(def-parse-rule (ignore)
  (lambda (_strm _token acc)
    (values #f acc)))

;; syntax

(def (get-parse-rule rule-name)
  "parse-rule function with error on not found"
  (or (get 'parse-rule rule-name)
      (error "undefined parse rule " rule-name)))
(def (rule-exp-name rule)
  (if (pair? rule)
      (car rule)
      (error "'rule-exp-name' expects a pair but got: " rule)))
(def (rule-exp-args rule)
  (if (pair? rule)
      (cdr rule)
      (error "'rule-exp-args' expects a pair but got: " rule)))

(defstruct rule (exp  ;; type (or symbol cons)
                 proc) ;; type type function
  constructor: :init!)

(def (rule-name rule)
  (if (rule? rule)
    (rule-exp-name (rule-exp rule))
    #f))

(defmethod {:init! rule}
  (lambda (self exp: exp proc: proc)
    (set! (rule-exp self) exp)
    (set! (rule-proc self) proc)))

(def (eval-rule-exp rule)
  (make-rule
   exp: rule
   proc: (apply (get-parse-rule
                 (rule-exp-name rule))
           (rule-exp-args rule))))

(defstruct parse (name rules)
  constructor: :init!)

;; (defstruct lex-token (token token-val)
;;   )

(defmethod {:init! parse}
  (lambda (self name rules)
    (set! (parse-name self) name)
    (set! (parse-rules self) (and rules (map eval-rule-exp rules)))))

;; (def (prognify expr)
;;   (if (null? (cdr expr))
;;       (car expr)
;;       (cons 'progn expr)))

(def (def-token name . clauses)
  (def (listify x)
    (if (list? x)
      x
      (list x)))
  (def exp-token
    (match <>
      ([token: token . _] token)
      (else #f)))
  (def exp-parse-rules
    (match <>
      ([_ _ parse-rules: . parse-rules] parse-rules)
      (else #f)))
  (for-each (lambda (clause)
              (let ((tokens (exp-token clause))
                    (rules (exp-parse-rules clause)))
                (when (and (not tokens) rules)
                  (error "def-token: parse-rules defined but no tokens defined - " clause))
                (for-each (lambda (token)
                            (put! 'token token
                                  (make-parse name rules)))
                          (listify tokens))))
            clauses))

;; parsing

(def (parse-token strm token acc)
  "parse the lexical token with stream and built up acc lst (in reverse order)"
  (let (parse-rules (val->parse-rules (token-val token)))
    (if parse-rules
      (let aux ((token (make-op-token token))
                (acc acc)
                (rules parse-rules))
        ;; left this displayln for debugging purpose
        ;; (displayln "parsing " token " acc: " acc)
        (cond ((not (null? rules))
               (let ((values token acc)
                     ((rule-proc (car rules)) strm token acc))
                 (aux token acc (cdr rules))))
              (token (cons token acc))
              (else acc)))
      (cons token acc))))

;;;

(def (reduce-strm fn seq)
  "streamed reduce variant for sequence"
  (let iter ((strm (make-str-stream seq))
             (acc []))
    (if {empty? strm}
      (reverse acc)
      (iter strm
            (fn strm acc)))))

(def (parse-string str)
  "parse string into structured tokens"
  (reduce-strm (lambda (strm acc)
                 (parse-token strm (next-token strm) acc))
               str))

(def (parse-file str)
  (if (eq? (file-type str) 'regular)
    (parse-string (read-file-string str))
    (error "parse: path does not points to a regular file")))

(def (parse-it str)
  (if (string? str)
    (if (file-exists? str)
      (parse-file str)
      (parse-string str))
    (error "parse: expected string not " str)))