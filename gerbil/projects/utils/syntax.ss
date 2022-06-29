
(export hash->val!
        def-hash-struct
        defp)

;;;

(defsyntax (pred-lambda stx)
  (def (split-parg stx)
    (datum->syntax stx
      (stx-map string->symbol
               (string-split (symbol->string (stx-e stx)) #\:))))

  ;; same form as in gerbil/expander/stx.ss
  (def (stx-cadr stx)
    (declare (safe))
    (cadr (stx-e stx)))

  (syntax-case stx ()
    ((macro pargs body ...)
     (identifier-list? #'pargs)
     (with-syntax* ((split (stx-map split-parg #'pargs))
                    (args (stx-map stx-car #'split))
                    (preds (stx-map stx-cadr #'split))
                    (clauses (cons 'and (stx-map (lambda (x y) [y x]) #'args #'preds))))
       #'(lambda args
           (unless clauses
             (error "Arguments do not respect signature."))
             body ...)))))

(defrules defp ()
  ((_ (id . args) body ...)
   (identifier? #'id)
   (define-values (id)
     (pred-lambda args body ...)))
  ((_ id expr)
   (identifier? #'id)
   (define-values (id) expr)))

;;

(defsyntax (def-hash-struct stx)
  (def (gen-init-exp name)
    (lambda (slot key (proc 'identity))
      (with-syntax ((accessor (make-symbol name "-" slot))
                    (proc proc)
                    (key key))
        #'(set! (accessor self)
            (proc (hash-get hash key))))))
  (def (gen-def-hash-struct-exp proc x)
    (match x
      ([x . y]
       [x (proc x) . y])
      (x
       [x (proc x)])))
  (syntax-case stx ()
    ((_ name slots)
     (with-syntax ((slots (stx-map stx-car #'slots))
                   (expr (cons 'begin
                               (stx-map (compose
                                         (cut apply (gen-init-exp (syntax->datum #'name))
                                              <>)
                                         syntax->datum)
                                        #'slots))))
       #'(begin (defstruct name slots
                  constructor: :init!)
                (defmethod {:init! name}
                  (lambda (self hash)
                    expr)))))
    ((_ name slots convertp: convertp)
     (let (convertp (eval-syntax #'convertp))
       (with-syntax ((slots (stx-map (compose1
                                      (cut gen-def-hash-struct-exp convertp <>)
                                      syntax->datum)
                                     #'slots)))
         #'(def-hash-struct name slots))))))

(def (test)
  (def-hash-struct site
    (title init-date
           (nav (cut call-with-input-string <> read))
           index-path post-path)
    convertp: symbol->string)
  (let* ((yaml (yaml-load "~/sources/gerbil/projects/static-site-gen/site.yaml"))
         (site (make-site (car yaml))))
    (display* (site-title site)
              (site-init-date site)
              (site-nav site))))