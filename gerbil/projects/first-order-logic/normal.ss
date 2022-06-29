
;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- File: logic/normal.lisp

;;;; Convert Expressions to Normal Form (Conjunctive, Implicative or Horn)

;;; This could be done much more efficiently by using a special
;;; representation for CNF, which eliminates the explicit ANDs
;;; and ORs.  This code is meant to be informative, not efficient.

;;;; Top-Level Functions
(import :std/sugar)
(import :std/misc/func)

(export op arg1 arg2)
(def op (cut car <>))
(def arg1 (cut cadr <>))
(def arg2 (cut caddr <>))

(def (mklist obj)
  (if (list? obj)
    obj
    (list obj)))

(def (sublis alist tree (test eq?))
  (match tree
    ([a . tree]
     (chain (match a
              ((? symbol?)
               (assoc a alist test))
              ((? list?)
               (sublis alist a test))
              (else a))
       (cons <> (sublis alist tree test))))
    (else [])))

(def (sublis! alist tree (test eq?))
  (match tree
    ([a . _tree]
     (match a
       ((? symbol?)
        (set! (car tree) (assoc a alist test)))
       ((? list?)
        (sublis! alist a test)))
     (sublis! alist _tree test)))
  tree)

;;;

(export ->cnf)

(def (->cnf p (vars []))
  "Convert a sentence p to conjunctive normal form [p 279-280]."
  ;; That is, return (and (or ...) ...) where
  ;; each of the conjuncts has all literal disjuncts.
  ;; VARS is a list of universally quantified variables that P is in scope of.
  (set! p (eliminate-implications (logic p)))
  (match (op p)
    (NOT (let ((p2 (move-not-inwards (arg1 p))))
           (if (literal-clause? p2)
             p2
             (->cnf p2 vars))))
    (AND (chain (args p)
           (map (lambda (q) (conjuncts (->cnf q vars)))
                <>)
           (foldl append [] <>)
           (conjunction <>)))
    (OR  (merge-disjuncts (map (cut ->cnf <> vars)
                               (args p))))
    (FORALL (let ((new-vars (map new-variable (mklist (arg1 p)))))
              (->cnf (sublis (map cons (mklist (arg1 p)) new-vars)
                             (arg2 p))
                     (append new-vars vars))))
    (EXISTS (->cnf (skolemize (arg2 p) (arg1 p) vars)
                   vars))
    (else p) ; p is atomic
    ))

(def (->inf p)
  "Convert a sentence p to implicative normal form [p 282]."
  (conjunction (map cnf1->inf1 (conjuncts (->cnf p)))))

(def (->horn p)
  "Try to convert sentence to a Horn clause, or a conjunction of Horn clauses.
  Signal an error if this cannot be done."
  (let ((q (->inf p)))
    (when (not (andmap horn-clause? (conjuncts q)))
      (displayln "WARN: " p ", converted to " q ", is not a Horn clause."))
    q))

(def (logic sentence)
  "Canonicalize a sentence into proper logical form."
  (cond ((stringp sentence) (->prefix sentence))
        (t sentence)))

;;;; Auxiliary Functions

(def (cnf1->inf1 p)
  ;; P is of the form (or (not a) (not b) ... c d ...)
  ;; Convert to: (=> (and a b ...) (or c d ...))
  ;; where a,b,c,d ... are positive atomic clauses
  (let ((lhs (map arg1 (filter negative-clause? (disjuncts p))))
        (rhs (filter (compose not negative-clause?) (disjuncts p))))
    `(=> ,(conjunction lhs) ,(disjunction rhs))))

(def (eliminate-implications p)
  (if (literal-clause? p)
    p
    (match (op p)
      (=>  `(or ,(arg2 p) (not ,(arg1 p))))
      (<=> `(and (or ,(arg1 p) (not ,(arg2 p)))
                 (or (not ,(arg1 p)) ,(arg2 p))))
      (else (cons (op p)
                  (map eliminate-implications (args p)))))))

(def move-not-inwards (p)
  "Given P, return ~P, but with the negation moved as far in as possible."
  (match (op p)
    (TRUE 'false)
    (FALSE 'true)
    (NOT (arg1 p))
    (AND (disjunction (map move-not-inwards (args p))))
    (OR  (conjunction (map move-not-inwards (args p))))
    (FORALL (make-exp 'EXISTS (arg1 p) (move-not-inwards (arg2 p))))
    (EXISTS (make-exp 'FORALL (arg1 p) (move-not-inwards (arg2 p))))
    (else (make-exp 'not p))))

(def (merge-disjuncts disjuncts)
  "Return a CNF expression for the disjunction."
  ;; The argument is a list of disjuncts, each in CNF.
  ;; The second argument is a list of conjuncts built so far.
  (match (length disjuncts)
    (0 'false)
    (1 (first disjuncts))
    (else (conjunction
           (for/fold (acc []) (y (conjuncts (merge-disjuncts (rest disjuncts))))
             (append acc
                     (for/collect (x (conjuncts (first disjuncts)))
                       (disjunction (append (disjuncts x) (disjuncts y))))))))))

(def (skolemize p vars outside-vars)
  "Within the proposition P, replace each of VARS with a skolem constant,
  or if OUTSIDE-VARS is non-null, a skolem function of them."
  (sublis (map (lambda (var)
                 (cons var
                       (if (null outside-vars)
                         (skolem-constant var)
                         (cons (skolem-constant var) outside-vars))))
               (mklist vars))
          p))

(defsyntax (1+! stx)
  (syntax-case stx ()
   ((macro expr)
    #'(set! expr (1+ expr)))))

(def (skolem-constant name)
  "Return a unique skolem constant, a symbol starting with '$'."
  (make-symbol "$" name "_" (1+! *new-variable-counter*)))

(def (renaming? p q (bindings +no-bindings+))
  "Are p and q renamings of each other? (That is, expressions that differ
  only in variable names?)"
  (cond ((eq? bindings +fail+) +fail+)
        ((equal? p q) bindings)
        ((and (consp p) (consp q))
         (renaming? (rest p) (rest q)
                    (renaming? (first p) (first q) bindings)))
        ((not (and (variable? p) (variable? q)))
         +fail+)
        ;; P and Q are both variables from here on
        ((and (not (get-binding p bindings)) (not (get-binding q bindings)))
         (extend-bindings p q bindings))
        ((or (eq? (lookup p bindings) q)
             (eq? p (lookup q bindings)))
         bindings)
        (else +fail+)))

;;;; Utility Predicates and Accessors

(def +logical-connectives+ '(and or not => <=>))
(def +logical-quantifiers+ '(forall exists))

(def (atomic-clause? sentence)
  "An atomic clause has no connectives or quantifiers."
  (not (or (memq (op sentence) +logical-connectives+)
           (memq (op sentence) +logical-quantifiers+))))

(def (literal-clause? sentence)
  "A literal is an atomic clause or a negated atomic clause."
  (or (atomic-clause? sentence)
      (and (negative-clause? sentence)
           (atomic-clause? (arg1 sentence)))))

(export negative-clause?)

(def (negative-clause? sentence)
  "A negative clause has NOT as the operator."
  (eq? (op sentence) 'not))

(def (horn-clause? sentence)
  "A Horn clause (in INF) is an implication with atoms on the left and one
  atom on the right."
  (and (eq? (op sentence) '=>)
       (andmap atomic-clause? (conjuncts (arg1 sentence)))
       (atomic-clause? (arg2 sentence))))

(export conjuncts disjuncts)

(def (conjuncts sentence)
  "Return a list of the conjuncts in this sentence."
  (cond ((eq? (op sentence) 'and) (args sentence))
        ((eq? sentence 'true) nil)
        (else (list sentence))))

(def (disjuncts sentence)
  "Return a list of the disjuncts in this sentence."
  (cond ((eq? (op sentence) 'or) (args sentence))
        ((eq? sentence 'false) nil)
        (else (list sentence))))

(def (conjunction args)
  "Form a conjunction with these args."
  (match (length args)
    (0 'true)
    (1 (first args))
    (else (cons 'and args))))

(def (disjunction args)
  "Form a disjunction with these args."
  (match (length args)
    (0 'false)
    (1 (first args))
    (else (cons 'or args))))
