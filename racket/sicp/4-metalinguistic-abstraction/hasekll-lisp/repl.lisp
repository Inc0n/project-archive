

(defparameter primitive-procedures
  (list (list 'car #'car)
        (list 'cdr #'cdr)
        (list 'cons #'cons)
        (list 'null? #'null)
        (list 'eq? #'eq)
        ;; ⟨more primitives⟩
        ))
(defun primitive-procedure-names ()
  (mapcar #'car primitive-procedures))
(defun primitive-procedure-objects ()
  (mapcar (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(defun setup-environment ()
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true t initial-env)
    (define-variable! 'false nil initial-env)
    initial-env))

(defparameter the-global-environment (setup-environment))

;;

(defun primitive-procedure? (proc)
  (tagged-list? proc 'primitive))
(defun primitive-implementation (proc) (cadr proc))


(defparameter apply-in-underlying-scheme #'apply)

(defun apply-primitive-procedure (proc args)
  (funcall apply-in-underlying-scheme
   (primitive-implementation proc)
   args))

(defparameter input-prompt ";;; M-Eval input:")
(defparameter output-prompt ";;; M-Eval value:")

(defun driver-loop (eval-proc)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (funcall eval-proc
                           input
                           the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop eval-proc))

(defun prompt-for-input (string)
  (format t "~%~a~%" string))
(defun announce-output (string)
  (format t "~%~a~%" string))

(defun display (object)
  (format t "~a~%" object))

(defun user-print (object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))