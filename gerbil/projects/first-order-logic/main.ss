
(import :std/iter)
(import :logic/normal)

(defstruct fol-kb (positive-clauses negative-clauses frame)
  (constructor: %make-fol-kb))

(def (make-fol-kb)
  (%make-fol-kb (make-hash-table test: eq?)
                (make-hash-table test: eq?)
                []))

(defmethod {tell fol-kb}
  (lambda (self sentence)
    "Add a sentence to a FOL knowledge base."
    (for (clause (->minimal-cnf sentence))
      (tell-minimal-cnf-clause self clause))))

(defmethod {retract fol-kb}
  (lambda (self sentence)
    "Delete each conjunct of sentence from KB."
    (retract-minimal-cnf-clauses self (->minimal-cnf sentence))))

(defmethod {ask-each fol-kb}
  (lambda (self query fn)
    "Use resolution to decide if sentence is true."
    (prove-by-refutation kb (->minimal-cnf `(not ,query)) fn)))

;;; FOL Knowledge Base Util

(def (possible-resolvers kb literal)
  (if (negative-clause? literal)
    (hash-get (fol-kb-negative-clauses kb) (op (arg1 literal)))
    (hash-get (fol-kb-positive-clauses kb) (op literal))))

(def (tell-minimal-cnf-clause! kb clause)
  ;; We don't add tautologies like "P | ~P".
  ;; It would be good to eliminate subsumed clauses like
  ;; Eq(1,1) when Eq(x,x) is already in the kb.
  ;; Currently we don't check for that.
  (unless (tautology? clause)
    (for (literal clause)
      (if (negative-clause? literal)
        (push clause (hash-get (fol-kb-negative-clauses kb)
                               (op (arg1 literal))))
        (push clause (hash-get (fol-kb-positive-clauses kb)
                               (op literal)))))))

(defun retract-minimal-cnf-clauses! (kb clauses)
  "Remove the minimal-cnf clauses from the KB."
  (for (clause clauses)
    (for (literal clause)
      (if (negative-clause? literal)
        (deletef clause
                 (hash-get (fol-kb-negative-clauses kb)
                           (op (arg1 literal))))
        (deletef clause (hash-get (fol-kb-positive-clauses kb)
                                  (op literal)))))))

(def (->minimal-cnf sentence)
  "Convert a logical sentence to minimal CNF (no and/or connectives)."
  ;; E.g., (and (or P (not Q) R) S) becomes ((P (not Q) R) (S))
  ;; Everything internal in the FOL module uses minimal-cnf
  ;; Only tell, retract, and ask-* use the regular logical form.
  (map disjuncts (conjuncts (->cnf sentence))))

(def (undo-changes kb)
  "Undo the changes that were temporarilly made to KB."
  (retract-minimal-cnf-clauses! kb (fol-kb-frame kb))
  (set! (fol-kb-frame kb) []))

(def (tautology? clause)
  "Is clause a tautology (something that is always true)?"
  (ormap (lambda (literal)
           (and (negative-clause? literal)
                (member (arg1 literal) clause equal?)))
         clause))

