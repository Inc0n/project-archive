
;; utils

(def (tagged? x tag)
  (match x
    ([(eq? tag) . _] #t)
    (else #f)))

;; machine assembly

'(read-eval-print-loop:
  (perform (op initialize-stack))
  (perform (op displayln) (const ";;EC-Evalinput:"))
  (assign exp (op read))
  (assign env (op get-global-environment))
  (assign continue (label print-result:))
  (goto (label eval-dispatch:))
  print-result:
  (perform (op displayln) (const ";;EC-Evalvalue:"))
  (perform (op displayln) (reg val))
  (goto (label read-eval-print-loop:)))

(def self-evaluating? (? (or string? number?)))

(def variable? (? symbol?))
(def (lookup-variable-value var env)
  (assoc var env))

(def quoted? (cut tagged? <> 'quote))
(def text-of-quotation cadr)

(def assignment? (cut tagged? <> 'set!))
(def definition? (cut tagged? <> 'def))
(def if? (cut tagged? <> 'if))

(def lambda? (cut tagged? <> 'lambda))
(def lambda-parameters cadr)
(def lambda-body cddr)
(def (make-procedure unev exp env)
  ;; TODO
  )

(def begin? (cut tagged? <> 'begin))
(def application? (? pair?))

'(eval-dispatch:
  (test (op self-evaluating?) (reg exp))
  (branch (label ev-self-eval:))
  (test (op variable?) (reg exp))
  (branch (label ev-variable:))
  (test (op quoted?) (reg exp))
  (branch (label ev-quoted:))
  (test (op assignment?) (re gexp))
  (branch (label ev-assignment:))
  (test (op definition?) (reg exp))
  (branch (label ev-definition:))
  (test (op if?) (reg exp))
  (branch (label ev-if:))
  (test (op lambda?) (reg exp))
  (branch (label ev-lambda:))
  (test (op begin?) (reg exp))
  (branch (label ev-begin:))
  (test (op application?) (reg exp))
  (branch (label ev-application:))
  (goto (label unknown-expression-type:))
  ev-self-eval:
  (assign val (reg exp))
  (goto (reg continue))
  ev-variable:
  (assign val (op lookup-variable-value) (reg exp) (reg env))
  (goto (reg continue))
  ev-quoted:
  (assign val (op text-of-quotation) (reg exp))
  (goto (reg continue))
  ev-lambda:
  (assign unev (op lambda-parameters) (reg exp))
  (assign exp (op lambda-body) (reg exp))
  (assign val (op make-procedure) (reg unev) (reg exp) (reg env))
  (goto (reg continue))
  ev-application:
  (save continue)
  (save env)
  (assign unev (op operand) (reg exp))
  (save unev)
  (assign exp (op operator) (reg exp))
  (assign continue (label ev-did-operator:))
  (goto (label eval-dispatch:))
  ev-did-operator:
  (restore unev)
  (assign argl (op empty-argl))
  (assign proc (reg val))
  (test (op no-operand?) (reg unev))
  (branch (label apply-dispatch)))