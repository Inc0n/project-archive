
(defun self-evaluating? (exp)
  (cond ((numberp exp) t)
        ((stringp exp) t)
        (t nil)))

(defun variable? (exp) (symbolp exp))

(defun quoted? (exp) (tagged-list? exp 'quote))
(defun text-of-quotation (exp) (cadr exp))

(defun tagged-list? (exp tag)
  (if (consp exp)
      (eq (car exp) tag)
      nil))


(defun assignment? (exp) (tagged-list? exp 'set!))
(defun assignment-variable (exp) (cadr exp))
(defun assignment-value (exp) (caddr exp))

(defun definition? (exp)
  (tagged-list? exp 'define))
(defun definition-variable (exp)
  (if (symbolp (cadr exp))
      (cadr exp)
      (caadr exp)))
(defun definition-value (exp)
  (if (symbolp (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)          ; formal parameters
                   (cddr exp)))) ; body

(defun lambda? (exp) (tagged-list? exp 'lambda))
(defun lambda-parameters (exp) (cadr exp))
(defun lambda-body (exp) (cddr exp))

(defun make-lambda (parameters body)
  (cons 'lambda (cons parameters body)))

(defun if? (exp) (tagged-list? exp 'if))
(defun if-predicate (exp) (cadr exp))
(defun if-consequent (exp) (caddr exp))
(defun if-alternative (exp)
  (if (not (null (cdddr exp)))
      (cadddr exp)
      'false))
(defun make-if (predicate consequent alternative)
  ;; `(if ,predicate ,consequent ,alternative)
  (list 'if predicate consequent alternative))

(defun begin? (exp) (tagged-list? exp 'begin))
(defun begin-actions (exp) (cdr exp))
(defun last-exp? (seq) (null (cdr seq)))
(defun first-exp (seq) (car seq))
(defun rest-exps (seq) (cdr seq))

(defun sequence->exp (seq)
  (cond ((null seq) seq)
        ((last-exp? seq) (first-exp seq))
        (t (make-begin seq))))
(defun make-begin (seq) (cons 'begin seq))

(defun application? (exp) (consp exp))
(defun operator (exp) (car exp))
(defun operands (exp) (cdr exp))
(defun no-operands? (ops) (null ops))
(defun first-operand (ops) (car ops))
(defun rest-operands (ops) (cdr ops))

(defun cond? (exp) (tagged-list? exp 'cond))
(defun cond-clauses (exp) (cdr exp))
(defun cond-else-clause? (clause)
  (eq (cond-predicate clause) 'else))

(defun cond-predicate (clause) (car clause))
(defun cond-actions (clause) (cdr clause))
(defun cond->if (exp)
  (labels
      ((expand-clauses (clauses)
         (if (null clauses)
             'false                     ; no else clause
             (let ((first (car clauses))
                   (rest (cdr clauses)))
               (if (cond-else-clause? first)
                   (if (null rest)
                       (sequence->exp (cond-actions first))
                       (error nil
                              "ELSE clause isn't last: COND->IF"
                              clauses))
                   (make-if (cond-predicate first)
                            (sequence->exp (cond-actions first))
                            (expand-clauses rest)))))))
    (expand-clauses (cond-clauses exp))))

