
(export hash->val!
        def-hash-struct
        hash-struct
        hash-struct-%hash
        hash-struct)

;;;

(defstruct hash-struct (%hash))

(def (hash->val! hash proc key)
  (if (procedure? proc)
    (hash-update! hash key proc)
    (hash-put! hash key (hash-ref hash key proc))))

(begin-syntax
  (def (struct-field-gen name slot)
    `(def (,(make-symbol name "-" (car slot)) self)
       (hash-ref (hash-struct-%hash self)
                 ,(cadr slot))))
  (def (init-gen name slots)
    `(defmethod {:init! ,name}
       (lambda (self hash)
         ,@(map (lambda (slot)
                  (unless (null? (cddr slot))
                    `(hash->val! hash ,(caddr slot) ,(cadr slot))))
                slots)
         (set! (hash-struct-%hash self) hash)))))
;; (defsyntax (def-hash-struct stx)
;;   (syntax-case stx ()
;;     ((_ name slot . slots)
;;      (let (_slots (syntax->datum #'slots))
;;        (with-syntax ((magic
;;                       (cons 'begin 
;;                             (map (lambda (slot)
;;                                    (unless (null? (cddr slot))
;;                                      ['hash->val! 'hash (caddr slot) (cadr slot)]))
;;                                  _slots)))
;;                      (accessors
;;                       (cons #'begin
;;                             (stx-map (lambda (slot)
;;                                        (with-syntax ((id (stx-car slot))
;;                                                      (key (stx-car (stx-cdr slot))))
;;                                          #'(defmethod {id name}
;;                                              (lambda (self)
;;                                                (hash-ref (hash-struct-%hash self)
;;                                                          key)))))
;;                                      #'slots))))
;;          #'(begin
;;              (defstruct (name hash-struct) ()
;;                constructor: :init!)
;;              (defmethod {:init! name}
;;                (lambda (self hash)
;;                  magic
;;                  (set! (hash-struct-%hash self) hash)))
;;              accessors))))
;;     ((_ slots: slot . slots))
;;     ((_ slots: slot)
;;      #')))
(defrules def-hash-struct ()
  ((_ name slots: ((id key) . slots))
   (begin
     (def (id self)
       (hash-ref (hash-struct-%hash self)
                 key))
     (def-hash-struct name slots: slots)))
  ((_ name slots: ((id key proc) . slots))
   (begin
     (def (id self)
       (hash-ref (hash-struct-%hash self)
                 key))
     (def-hash-struct name slots: slots)))
  ((_ name slots: ())
   #!void))

(defsyntax (def-hash-struct stx)
  (syntax-case stx ()
    ((_ name slots: (slot . slots))
     (with-syntax ((id (stx-car #'slot))
                   (key (stx-car (stx-cdr #'slot)))
                   (more (if (stx-null? #'slots)
                           #!void
                           (def-hash-struct #'name slots: #'slots))))
       #'(begin
           (def (id self)
             (hash-ref (hash-struct-%hash self)
                       key))
           more)))
    ((_ name . slots)
     (let (_slots (syntax->datum #'slots))
       (with-syntax ((magic
                      (cons 'begin 
                            (map (lambda (slot)
                                   (unless (null? (cddr slot))
                                     ['hash->val! 'hash (caddr slot) (cadr slot)]))
                                 _slots))))
         #'(begin
             (defstruct (name hash-struct) ()
               constructor: :init!)
             (defmethod {:init! name}
               (lambda (self hash)
                 magic
                 (set! (hash-struct-%hash self) hash)))
             (def-hash-struct name slots: slots)))))))


     
     ;; (let ((name (syntax->datum #'name))
     ;;       (slots (syntax->datum #'slots)))
     ;;   (with-syntax ((magic
     ;;                  `(begin
     ;;                     (defstruct (,name hash-struct) ()
     ;;                       constructor: :init!)
     ;;                     ,(init-gen name slots)
     ;;                     ,@(map (cut struct-field-gen name <>)
     ;;                            slots))))
     ;;     #'magic))

