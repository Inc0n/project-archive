
(load "data.lisp")

(defun true? (x) (not (eq x nil)))
(defun false? (x) (eq x nil))

(defun make-procedure (parameters body env)
  ;; `(procudure ,parameters ,body ,env)
  (list 'procedure parameters body env))

(defun compound-procedure? (p)
  (tagged-list? p 'procedure))
(defun procedure-parameters (p) (cadr p))
(defun procedure-body (p) (caddr p))
(defun procedure-environment (p) (cadddr p))

(defun enclosing-environment (env) (cdr env))
(defun first-frame (env) (car env))
(defparameter the-empty-environment '())

(defun make-binding (var val) (cons var val))
(defun binding-var (bind) (car bind))
(defun binding-value (bind) (cdr bind))

(defun make-frame (variables values)
  (mapcar #'make-binding variables values))
(defun frame-variables (frame) (mapcar #'binding-var frame))
(defun frame-values (frame) (mapcar #'binding-value frame))
(defun add-binding-to-frame! (var val env)
  (setf (car env) (cons (make-binding var val)
                                (first-frame env))))

(defun extend-environment (vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error nil "Too many arguments supplied" '(vars vals))
          (error nil "Too few arguments supplied"
                 '(vars vals)))))

(defun scan (var bindings)
  (cond ((null bindings) nil)
        ((eq var (binding-var (car bindings)))
         (car bindings))
        (t (scan var (cdr bindings)))))

(defun lookup-variable-value (var env)
  (labels
      ((env-loop (env)
         (if (eq env the-empty-environment)
             (error nil "Unbound variable" var)
             (let ((binding (scan var (first-frame env))))
               (if binding
                   (binding-value binding)
                   (env-loop
                    (enclosing-environment env)))))))
    (env-loop env)))

(defun define-variable! (var val env)
  (let ((frame (first-frame env)))
    (let ((binding (scan var frame)))
      (if binding
          (setf (cdr binding) val)
          (add-binding-to-frame! var val env)))))

(defun set-variable-value! (var val env)
  (labels
      ((env-loop (env)
         (if (eq env the-empty-environment)
             (error nil
                    "Cannot set variable before its definition"
                    var)
             (let ((binding (scan var (first-frame env))))
               (if binding
                   (setf (cdr binding) val)
                   (env-loop (enclosing-environment env)))))))
    (env-loop env)))
