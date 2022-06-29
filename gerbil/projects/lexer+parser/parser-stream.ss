
(export get-env-var
		;; parser-stream
		parser-stream make-parser-stream
        parser-stream-env parser-stream-tokens)

;; env
(def make-bind cons)
(def (bind-val-set! bind val)
  (set! (cdr bind) val))

(def (make-new-env) [])

(def (get-env-var env var)
  (def (not-found-handler _)
    (error "get-env-var: var not found in env - " var env))
  (assgetq var env not-found-handler))

(def (set-env-var! env var val)
  "may or maynot modify the env list"
  (let (x (assoc var env))
    (if x
      (begin (set! (bind-val x) val)
             env)
      (cons (make-bind var val) env))))

;;

(defclass parser-stream (tokens env)
  constructor: :init!
  final: #t)

(defmethod {:init! parser-stream}
  (lambda (self tokens)
    (set! (@ self tokens) tokens)
    (set! (@ self env) (make-new-env))))

(defmethod {car parser-stream}
  (lambda (self)
    (car (@ self tokens))))

(defmethod {cdr! parser-stream}
  (lambda (self)
    (set! (@ self tokens)
      (cdr (@ self tokens)))
    self))

(defmethod {set-env-var! parser-stream}
  (lambda (self var val)
    (set! (@ self env)
      (set-env-var! (@ self env) var val))))

(defmethod {get-env-var parser-stream}
  (lambda (self var)
    (get-env-var (@ self env) var)))