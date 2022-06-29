
(def (tagged? lst x)
  (and (pair? lst)
       (eq? (car lst) x)))
;; (def (tagged? lst x)
;;   (match lst
;;     ((and [y . _] y (eq? x)) #t)
;;     (else #f)))

(import :std/lazy ;; lazy
        :std/misc/list ;; unique
        :std/srfi/41 ;; stream
        :std/sugar) ;; try

(import :gerbil/gambit/exceptions)

;; (import :drewc/r7rs/gerbil-swank)

(export main)

;;; stream

(defalias stream-cdr-no-force stream-cdr)
(def (stream-cdr-force x)
  (force (stream-cdr x)))

(def (stream-append-delayed s1 delayed-s2)
  (if (stream-null? s1)
    (force delayed-s2)
    (stream-cons (stream-car s1)
                 (stream-append-delayed (stream-cdr-force s1)
                                        delayed-s2))))
(def (interleave-delayed s1 delayed-s2)
  (if (stream-null? s1)
    (force delayed-s2)
    (stream-cons (stream-car s1)
                 (interleave-delayed (force delayed-s2)
                                     (stream-cdr-no-force s1)))))

(def (flatten-stream stream)
  (if (stream-null? stream)
    stream-null
    (interleave-delayed
     (stream-car stream)
     (delay (flatten-stream (stream-cdr-force stream))))))

(def (stream-flatmap proc s)
  (flatten-stream (stream-map proc s)))

;;; syntax

(def empty-conjunction? (cut null? <>))
(def first-conjunct (cut car <>))
(def rest-conjuncts (cut cdr <>))
(def empty-disjunction? (cut null? <>))
(def first-disjunct (cut car <>))
(def rest-disjuncts (cut cdr <>))
(def negated-query (cut car <>))
(def predicate (cut car <>))
(def args (cut cdr <>))

(def rule? (cut tagged? <> 'rule))
(def conclusion (cut cadr <>))
(def (rule-body rule)
  (if (null? (cddr rule))
    '(always-true)
    (caddr rule)))

;;; assertions utils

(def (use-index? pat)
  (symbol? (car pat)))

(def (indexable? pat)
  (or (symbol? (car pat))
      (query-var? (car pat))))

(def (index-key-of pat)
  (def key (car pat))
  (if (query-var? key)
    '?
    key))

(def (get-stream key1 key2)
  (or (get key1 key2)
      stream-null))

;;; assertions

(def *the-assertions* stream-null)
(def (get-all-assertions) *the-assertions*)

(def (get-indexed-assertions pattern)
  (get-stream (index-key-of pattern) 'assertion-stream))

(def (fetch-assertions pattern)
  (if (use-index? pattern)
    (get-indexed-assertions pattern)
    (get-all-assertions)))

(def *the-rules* stream-null)
(def (get-all-rules) *the-rules*)

(def (get-indexed-rules pattern)
  (stream-append (get-stream (index-key-of pattern)
                             'rule-stream)
                 (get-stream '? 'rule-stream)))

(def (fetch-rules pattern)
  (if (use-index? pattern)
    (get-indexed-rules pattern)
    (get-all-rules)))

;;

(def *logic-alist* [])

(def (put! key1 key2 val)
  (def x (assoc key1 *logic-alist*))
  (if x
    (let (y (assoc key2 (cdr x)))
      (if y
        (set! (cdr y) val)
        (push! [key2 val] (cdr x))))
    (push! [key1 [key2 val]] *logic-alist*))
  'ok)

(def (get key1 key2)
  (alet* ((x (assoc key1 *logic-alist*))
          (y (assoc key2 (cdr x))))
    (match y
      ((? list?) (cadr y))
      ((? pair?) (cdr y))
      (else (error "unexpected datum " y)))))

;;

(def (store-index! x-to-key x place)
  (when (indexable? x-to-key)
    (let* ((key (index-key-of x-to-key))
           (current-assertion-stream
            (get-stream key place)))
      (put! key
           place
           (stream-cons x current-assertion-stream)))))
(def (store-assertion-in-index! assertion)
  (store-index! assertion assertion
               'assertion-stream))
(def (store-rule-in-index! rule)
  (store-index! (conclusion rule) rule
               'rule-stream))

(def (add-assertion! assertion)
  (store-assertion-in-index! assertion)
  (let (old-assertions *the-assertions*)
    (set! *the-assertions*
      (stream-cons assertion old-assertions))
    'ok))

(def (add-rule! rule)
  (store-rule-in-index! rule)
  (let (old-rules *the-rules*)
    (set! *the-rules* (stream-cons rule old-rules))
    'ok))

(def (add-rule-or-assertion! assertion)
  (if (rule? assertion)
    (add-rule! assertion)
    (add-assertion! assertion)))

;;; assertion (database) pattern match

(def (check-an-assertion assertion query-pat query-frame)
  (def match-result
    (pattern-match query-pat assertion query-frame))
  (if (eq? match-result 'failed)
    stream-null
    (stream match-result)))

(def (find-assertions pattern frame)
  (stream-flatmap (lambda (datum)
                    (check-an-assertion datum pattern frame))
                  (fetch-assertions pattern)))

(def (pattern-match pat dat frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? pat dat) frame)
        ((query-var? pat)
         (extend-if-consistent pat dat frame))
        ((and (pair? pat) (pair? dat))
         (pattern-match (cdr pat)
                        (cdr dat)
                        (pattern-match (car pat) (car dat) frame)))
        (else 'failed)))

;;

(def (tree-walk e found-query-var-fn)
  (cond ((query-var? e)
         (found-query-var-fn e))
        ((pair? e)
         (or (tree-walk (car e) found-query-var-fn)
             (tree-walk (cdr e) found-query-var-fn)))
        (else #f)))

(def (depends-on? exp var frame)
  (tree-walk exp
             (lambda (e)
               (if (equal? var e)
                 #t
                 (alet (b (bind-in-frame e frame))
                   (tree-walk (bind-val b)))))))

(def (unify-match p1 p2 frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? p1 p2) frame)
        ((query-var? p1) (extend-if-possible p1 p2 frame))
        ((query-var? p2) (extend-if-possible p2 p1 frame))
        ((and (pair? p1) (pair? p2))
         (unify-match (cdr p1)
                      (cdr p2)
                      (unify-match (car p1)
                                   (car p2)
                                   frame)))
        (else 'failed)))

;;; bind

(def (make-bind var val) (cons var val))
(def bind-var (cut car <>))
(def bind-val (cut cdr <>))

(def (bind-in-frame var frame)
  (assoc var frame equal?))

(def (extend var val frame)
  (cons (make-bind var val) frame))

(def (extend-if-consistent var dat frame)
  (let (bind (bind-in-frame var frame))
    (if bind
      (pattern-match (bind-val bind) dat frame)
      (extend var dat frame))))

(def (extend-if-possible var val frame)
  (let (bind (bind-in-frame var frame))
    (cond (bind
           (unify-match (bind-val bind) val frame))
          ((query-var? val)             ; ***
           (let (bind (bind-in-frame val frame))
             (if bind
               (unify-match var (bind-val bind)
                            frame)
               (extend var val frame))))
          ((depends-on? val var frame)  ; ***
           'failed)
          (else (extend var val frame)))))

;;; rules

(def *rule-counter* 0)
(def (new-rule-application-id)
  (set! *rule-counter* (1+ *rule-counter*)))

(def (make-new-var var rule-application-id)
  `(? ,rule-application-id ,@(cdr var)))

;;

(def (apply-rule rule query-pattern query-frame)
  (let* ((clean-rule (rename-vars-in rule))
         (unify-result
          (unify-match query-pattern
                       (conclusion clean-rule)
                       query-frame)))
    (if (eq? unify-result 'failed)
      stream-null
      (qeval (rule-body clean-rule)
             (stream unify-result)))))

(def (apply-rules pattern frame)
  (stream-flatmap (lambda (rule)
                    (apply-rule rule pattern frame))
                  (fetch-rules pattern)))

(def (rename-vars-in rule)
  (tree-walk rule
             (lambda (exp)
               (make-new-var exp (new-rule-application-id)))))

;;; query

(def query-var? (cut tagged? <> '?))

(def (expand-question-mark symbol)
  (def chars (symbol->string symbol))
  (if (char=? (string-ref chars 0) #\?)
    (list '? (make-symbol
              (substring chars 1 (string-length chars))))
    symbol))

(def (contract-question-mark var)
  (if (number? (cadr var))
    (make-symbol "?" (caddr var) "-" (cadr var))
    (make-symbol "?" (cadr var))))

;;

(def (query-tag exp)
  (if (pair? exp)
    (car exp)
    (error "Unknown expression QUERY-TAG " exp)))

(def (query-contents exp)
  (if (pair? exp)
    (cdr exp)
    (error "Unknown expression QUERY-CONTENTS " exp)))

(def (simple-query query-pattern frame-stream)
  ;; (displayln "simple-query " query-pattern)
  (stream-flatmap
   (lambda (frame)
     (stream-append-delayed
      (find-assertions query-pattern frame)
      (delay (apply-rules query-pattern frame))))
   frame-stream))

(def (qeval query frame-stream)
  (let (qproc (get (query-tag query) 'qeval))
    (if qproc
      (qproc (query-contents query) frame-stream)
      (simple-query query frame-stream))))

(def (instantiate exp frame unbound-var-handler)
  (def (copy exp)
    (cond ((query-var? exp)
           (let (bind (bind-in-frame exp frame))
             ;; (displayln "instantiate " frame)
             (if bind
               (copy (bind-val bind))
               (unbound-var-handler exp frame))))
          ((pair? exp)
           (cons (copy (car exp))
                 (copy (cdr exp))))
          (else exp)))
  (copy exp))

;;; compound query

(defsyntax (def-query stx)
  (syntax-case stx ()
    ((def-query name lambda-exp)
     #'(put! name 'qeval lambda-exp))))

(def (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
    frame-stream
    (conjoin (rest-conjuncts conjuncts)
             (qeval (first-conjunct conjuncts)
                    frame-stream))))

(put! 'and 'qeval conjoin)

(def (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
    stream-null
    (interleave-delayed (qeval (first-disjunct disjuncts)
                               frame-stream)
                        (delay (disjoin (rest-disjuncts disjuncts)
                                        frame-stream)))))
(put! 'or 'qeval disjoin)

(def (negate operands frame-stream)
  (stream-flatmap (lambda (frame)
                    (if (stream-null?
                         (qeval (negated-query operands)
                                (stream frame)))
                      (stream frame)
                      stream-null))
                  frame-stream))
(put! 'not 'qeval negate)

(def (execute exp)
  (let (symbol-fn (eval (predicate exp)))
    (if (procedure? symbol-fn)
      (apply symbol-fn (args exp))
      (error (predicate exp) " is required to be a function, but it's not"))))

(def (lisp-val call frame-stream)
  (stream-flatmap (lambda (frame)
                    (if (execute (instantiate
                                  call frame
                                  (lambda (v _frame)
                                    (error "Unknown pat var: LISP-VAL " v))))
                      (stream frame)
                      stream-null))
                  frame-stream))
(put! 'lisp-val 'qeval lisp-val)

(put! 'always-true 'qeval
      (lambda (_ frame-stream)
        frame-stream))

(put! 'exit 'qeval
      (lambda (_ _)
        (exit 1)))

;;

(def (assertion-to-be-added? exp)
  (tagged? exp 'assert!))
(def (assertion-body exp)
  (car (query-contents exp)))

;;

(def (query-syntax-process exps)
  (map (lambda (exp)
         (match exp
           ((? pair?) (query-syntax-process exp))
           ((? symbol?) (expand-question-mark exp))
           (else exp)))
       exps))

(def (%query query)
  (let (q (query-syntax-process query))
    (stream-map
     (lambda (frame) ;; expand back into assertion from frame
       (instantiate q frame
                    (lambda (v _f)
                      (contract-question-mark v))))
     (qeval q (stream [])))))

(def (query q)
  (stream->list (%query q)))

(def (add! assertion)
  "add assertion or rule"
  (add-rule-or-assertion!
   (assertion-body (query-syntax-process assertion))))

(def +input-prompt+ ";;; Query input:")
(def +output-prompt+ ";;; Query results:")
(def (query-driver-loop)
  "query system repl"
  (displayln +input-prompt+)
  (def q (read))
  (cond ((assertion-to-be-added? q)
         (add-rule-or-assertion!
          (assertion-body (query-syntax-process q)))
         (displayln "Assertion added to data base.")
         (query-driver-loop))
        (else
         (displayln +output-prompt+)
         (stream-for-each displayln (%query q))
         (query-driver-loop))))

;;;

(def *database*
  '((supervisor (Bitdiddle Ben) (Warbucks Oliver))
    (address (Warbucks Oliver) (Swellesley (Top Heap Road)))
    (job (Warbucks Oliver) (administration big wheel))
    (salary (Warbucks Oliver) 150000)
    ;;
    (address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
    (job (Bitdiddle Ben) (computer wizard))
    (salary (Bitdiddle Ben) 60000)

    (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78))
    (job (Hacker Alyssa P) (computer programmer))
    (salary (Hacker Alyssa P) 40000)
    (supervisor (Hacker Alyssa P) (Bitdiddle Ben))

    (address (Fect Cy D) (Cambridge (Ames Street) 3))
    (job (Fect Cy D) (computer programmer))
    (salary (Fect Cy D) 35000)
    (supervisor (Fect Cy D) (Bitdiddle Ben))

    (address (Tweakit Lem E) (Boston (Bay State Road) 22))
    (job (Tweakit Lem E) (computer technician))
    (salary (Tweakit Lem E) 25000)
    (supervisor (Tweakit Lem E) (Bitdiddle Ben))
    (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))
    (job (Reasoner Louis) (computer programmer trainee))
    (salary (Reasoner Louis) 30000)
    (supervisor (Reasoner Louis) (Hacker Alyssa P))
    ;;
    (can-do-job (computer wizard) (computer programmer))
    (can-do-job (computer wizard) (computer technician))
    (can-do-job (computer programmer) (computer programmer trainee))
    (can-do-job (administration secretary) (administration big wheel))))

(def (setup-database database)
  (for-each
   (lambda (key) (put! key 'assertion-stream #f))
   (unique
    (stream->list
     (stream-map index-key-of *the-assertions*))))
  (set! *the-assertions* stream-null)
  (for-each add-assertion! database)
  'ok)

(setup-database *database*)

(def (test)
  (list (query '(and (salary ?person ?amount) (lisp-val > ?amount 30000)))
        ;; (query '(and (salary ?person ?amount) (same ?person ?person)))
        (query '(and (supervisor ?x (Bitdiddle Ben))
                     (not (job ?x (computer programmer)))))
        (query '(and (job ?person (computer programmer))
                     (address ?person ?where)))
        ;; (query '(lives-near ?x (Bitdiddle Ben)))
        ))

(def (pprintln exp)
  "pretty print new line variant"
  (match exp
    ((? list?) (for-each displayln exp))
    ((? pair?) (displayln exp))
    (else (displayln exp))))


(def ans #f)
(def (repl)
  (display "query> ")
  (try
   (let* ((exp (read))
          (exp-1 (eval exp)))
     ;; (displayln exp-1)
     (set! ans exp-1)
     (pprintln exp-1))
   (catch (e)
     (display-exception e (current-error-port))))
  (repl))

(def (main)
  (repl))

(main)

;;

(def (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
    stream-null
    (interleave-delayed (qeval (first-disjunct disjuncts)
                               frame-stream)
                        (delay (disjoin (rest-disjuncts disjuncts)
                                        frame-stream)))))
(put! 'or 'qeval disjoin)

