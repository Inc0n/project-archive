
(import :std/misc/list ;; push! pop!
        :std/misc/func ;; compose1
        :std/sugar
        :std/misc/number) ;; increment! decrement!

;; register
(defclass register (name val tracing?)
  constructor: :init!
  final: #t)

(defmethod {:init! register}
  (lambda (self register-name)
    (@-set! self name register-name)
    (@-set! self val '*unassigned*)
    (@-set! self tracing? #f)))

(defmethod {register-val-set! register}
  (lambda (self new-val)
    (when (@ self tracing?)
      (displayln "setting reg " (@ self name) " from " (@ self val) " to " new-val))
    (@-set! self val new-val)))

;; stack
(defclass stack (s num-pushes max-depth current-depth)
  constructor: :init!
  final: #t)

(defmethod {:init! stack}
  (lambda (self)
    (@-set! self s [])
    (@-set! self num-pushes 0)
    (@-set! self max-depth 0)
    (@-set! self current-depth 0)))

(defmethod {push-stack! stack}
  (lambda (self x)
    (push! (@ self s) x)
    (increment! (stack-num-pushes self))
    (let (current-depth
          (increment! (stack-current-depth self)))
      (set! (stack-max-depth self) (max (@ self max-depth) current-depth)))))


(defmethod {pop-stack! stack}
  (lambda (self x)
    (match (pop! (@ self s))
      (#f (error "Empty stack: pop-stack!"))
      (top
       (decrement! (stack-current-depth self))
       top))))

(defmethod {print-statistics stack}
  (lambda (self)
    (displayln "total-pushes = " (@ self num-pushes))
    (displayln "maximum-depth = " (@ self max-depth))))

;; machine

(defclass machine (registers breakpoints tracing? ops stack insts-seq)
  constructor: :init!)

(defmethod {:init! machine}
  (match <>
    ([self]
     (@-set! self tracing? #f)
     (@-set! self breakpoints #f)
     (let ((pc (make-register 'pc))
           (flag (make-register 'flag)))
       (@-set! self stack (make-stack))
       (@-set! self insts-seq [])
       ;;
       (@-set! self ops `((initialize ,(lambda () {:init! stack}))
                          (print-stack-statistics ,(lambda () {print-statistics stack}))))
       (@-set! self registers [pc flag])))
    ;;
    ([self register-names ops controller-text]
     (for-each (lambda (reg-name)
                 {allocate-reg self reg-name})
               register-names)
     (install-operations machine ops)
     (install-instruction-sequence
      machin
      (assemble controller-text machine)))))

;; - registers

(defmethod {allocate-reg machine}
  (lambda (self reg-name)
    (push! (make-register reg-name)
           (@ self registers))))

(defmethod {get-reg machine}
  (lambda (self reg-name)
    (find (compose1 (is reg-name) register-name)
          (@ self registers))))

(defmethod {set-reg-val! machine}
  (lambda (self reg-name new-val)
    (chain {get-reg self reg-name}
      (register-val-set! <> new-val))))

(defmethod {get-reg-val machine}
  (lambda (self reg-name)
    (alet (reg {get-reg self reg-name})
      (register-val reg))))

;; - installations

(defmethod {install-insts-seq machine}
  (lambda (self insts-seq)
    (machine-insts-seq-set! self insts-seq)))

(defmethod {install-ops machine}
  (lambda (self ops)
    (machine-ops-set! self (append (@ self ops)
                                        ops))))

;; - start

(defmethod {start machine}
  (lambda (self)
    (let (pc {get-reg self 'pc})
      (register-val-set! pc (@ self insts-seq))
      (execute))))

(defmethod {execute machine}
  (lambda (self)
    (def pc {get-reg self 'pc})
    (let execute-loop ()
      (match (register-val pc)
        ([] 'done)
        ([inst . _]
         (when (@ machine tracing?)
           (displayln "exec: " (instruction-text inst)))
         (unless (find-breakpoint self inst)
           ((instruction-exec-proc inst))
           (execute-loop)))))))

(defstruct breakpoint (inst label-n))

(defmethod {find-breakpoint machine}
  (lambda (self inst)
    (find (compose1 (is inst) breakpoint-inst)
          (machine-breakpoints self))))

(defmethod {set-breakpoint machine}
  (lambda (self label n)
    (match (lookup-label self label)
      ((? pair?)
       (match (list-ref insts n)
         ((? instruction?)
          (push! (make-breakpoint inst label n)
                 (machine-breakpoints self)))
         (else (error "out of bound n for label: " label))))
      (else (error "label not found: " label)))))


(defmethod {proceed-machine machine}
  (lambda (self)
    {execute self}))

(defmethod {cancel-breakpoint machine}
  (lambda (self label n)
    (set! (machine-breakpoints self)
      (remf (lambda (bp)
              (and (eq? (breakpoint-label bp) label)
                   (= (breakpoint-n bp) n)))
            (machine-breakpoints self)))))

(defmethod {cancel-all-breakpoints machine}
  (lambda (self)
    (set! (machine-breakpoints self) [])))

;; assembler

(defstruct instruction (exec-proc text)
  constructor: :init!)

(defmethod {:init! instruction}
  (match <>
    ([self arg]
     (match arg
       ((? list?)
        (slot-set! self 'text text))
       ((? procedure?)
        (slot-set! self 'exec-proc exec-proc))))))

(def (lookup-label labels label-name)
  (match (assoc label-name labels)
    ([(eq? label-name) . val]
     val)
    (else (error "Undefined label: ASSEMBLE " label-name))))

;; -> assembly text -> [list of machine instructions]
(def (assemble text machine)
  (let ((values insts labels) (extract-labels text))
    (update-insts! insts labels machine)
    insts))

(def (extract-labels text)
  (def (aux text insts labels)
    (match text
      ([]
       (values (reverse insts) (reverse labels)))
      ([inst . text]
       (if (symbol? inst)
         ;; exercise 5.8 duplicated labels
         (if (lookup-label labels inst)
           (error "Duplicated labels: ASSEMBLE " inst)
           (aux text
                insts
                (cons [inst text] labels)))
         (aux text
              (cons (make-instruction inst) insts)
              labels)))))
  (aux text [] []))

(def (update-insts! insts labels _machine)
  (with ((machine pc: pc flag: flag stack: stack ops: ops) _machine)
    (for-each (lambda (inst)
                {:init!
                 inst
                 (make-exec-proc (instruction-text inst)
                                 labels _machine pc flag stack ops)})
              insts)))

;; -

(def (advance-pc pc)
  (register-val-set! pc (cdr (register-val pc))))

;; - basic proc

(def (tagged? x tag)
  (match x
    ([(eq? tag) . _] #t)
    (else #f)))

(def register-exp? (cut tagged? <> 'reg))
(def register-exp-reg cadr)
(def constant-exp? (cut tagged? <> 'const))
(def constant-exp-val cadr)
(def label-exp? (cut tagged? <> 'label))
(def label-exp-label cadr)

(def (make-primitive-exp exp machine labels)
  (match exp
    ((? constant-exp?)
     (let (val (constant-exp-val exp))
       (lambda () val)))
    ((? label-exp?)
     (let (insts (lookup-label labels (label-exp-label exp)))
       (lambda () insts)))
    ((? register-exp?)
     (let (reg {get-reg machine (register-exp-reg exp)})
       (lambda () (register-val reg))))))

;; - op proc

(def (operation-exp? exp)
  (and (pair? exp) (tagged? (car exp) 'op)))
(def operation-exp-op cadr)
(def operation-exp-operands cddr)

(def (make-operation-exp exp machine labels ops)
  (let ((op
         (lookup-prim (operation-exp-op exp) ops))
        (aprocs
         (map (lambda (e)
                (make-primitive-exp e machine labels))
              (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

(def (make-machine-exp exp machine labels ops))

;; - exec proc

(def (make-assign inst machine labels ops pc)
  (match inst
    (['assign reg-name . exp]
     (let ((target {get-reg machine reg-name})
           (value-proc
            (if (operation-exp? exp)
              (make-operation-exp exp machine labels ops)
              (make-primitive-exp (car exp) machine labels))))
       (lambda ()
         (set-reg-val! machine reg-name (value-proc))
         (advance-pc pc))))
    (else (error "Malformed inst for 'assign: ASSEMBLE " inst))))

(def (make-test inst machine labels ops flag pc)
  (match inst
    (['test . condition]
     (let (condition-proc
            (make-operation-exp condition machine labels ops))
       (lambda ()
         (register-val-set! flag (condition-proc))
         (advance-pc pc))))
    (else (error "Malformed inst for 'test: ASSEMBLE " inst))))

(def (make-branch inst machine labels flag pc)
  (match inst
    ((and ['branch dest]
          (label-exp? dest))
     (let (insts
           (lookup-label labels (label-exp-label dest)))
       (lambda ()
         (if (@ flag val)
           (register-val-set! pc insts)
           (advance-pc pc)))))
    (else (error "Malformed inst for 'branch: ASSEMBLE " inst))))

(def (make-goto inst machine labels pc)
  (match inst
    ((and ['goto dest])
     (match dest
       ((? label-exp?)
        (let (insts
              (lookup-label labels (label-exp-label dest)))
          (lambda () (register-val-set! pc insts))))
       ((? register-exp?)
        (let (reg {get-reg machine (register-exp-reg dest)})
          (lambda () (register-val-set! pc (@ reg val)))))))
    (else (error "Malformed inst for 'goto: ASSEMBLE " inst))))

(def (make-save inst machine stack pc)
  (match inst
    (['save reg-name]
     (let (reg {get-reg machine reg-name})
       (lambda ()
         (push-stack! stack (@ reg val))
         (advance-pc pc))))
    (else (error "Malformed inst for 'save: ASSEMBLE " inst))))

(def (make-restore inst machine stack pc)
  (match inst
    (['restore reg-name]
     (let (reg {get-reg machine reg-name})
       (lambda ()
         (register-val-set! reg (pop-stack! stack))
         (advance-pc pc))))
    (else (error "Malformed inst for 'restore: ASSEMBLE " inst))))

(def (make-perform inst machine labels ops pc)
  (match inst
    (['perform . action]
     (if (operation-exp? action)
       (let (action-proc
             (make-operation-exp action machine labels ops))
         (lambda ()
           (action-proc)
           (advance-pc pc)))))
    (else (error "Malformed inst for 'perform: ASSEMBLE " inst))))

(def (make-exec-proc inst labels machine pc flag stack ops)
  (match (car inst)
    ('assign (make-assign inst machine labels ops pc))
    ('test (make-test inst machine labels ops flag pc))
    ('branch (make-branch inst machine labels flag pc))
    ('goto (make-goto inst machine labels pc))
    ('save (make-save inst machine pc))
    ('restore (make-restore inst machine pc))
    ('perform (make-perform inst machine labels ops pc))
    (else (error "Unknown instruction type: ASSEMBLE " inst))))