
(import :std/sugar) ;; is 
(import :std/misc/func) ;; compose1 
(import :std/misc/list) ;; snoc
(import :std/srfi/1) ;; every any

(export tag tagged?
        every any list-copy append! ;; std/srfi/1
        push-back!
        def-debug-option)

(def (tagged? tag x)
  (and (pair? x)
       (eq? (car x) tag)))
(def (tag x y) [x y])

(defsyntax (def-*name* stx)
  (syntax-case stx ()
    ((_ *name* get-all setter getter getter-key-fn)
     #'(begin
         (def *name* [])
         (def (get-all) *name*)
         (def (setter x obj)
           (let (lst
                 (member (compose1 (is x) getter-key-fn)
                         *name*))
             (if lst
               (set! (car lst) obj)
               (append! obj *name*))))
         (def (getter x)
           (find (compose1 (is x) getter-key-fn)
                 *name*))))))

(defsyntax (def-debug-option stx)
  (syntax-case stx ()
    ((_ var-name setter-name)
     #'(begin (def var-name #f)
              (def (setter-name) (set! var-name #t))
              (export setter-name)))))

(defsyntax (cache stx)
  (syntax-case stx ()
    ((_ exp)
     (with-syntax ((id (gentemps 'simp))
                   (done? (gentemps 'done?)))
       #'(let ((id #f)
               (done? #f))
           (lambda ()
             (when (not done?)
               (set! done? #t)
               (set! id exp))
             id))))))

(defsyntax (push-back! stx)
  (syntax-case stx ()
    ((_ val place)
     #'(set! place (snoc val place)))))

;; (def *parser-alist* [])

;; (def (put! key1 key2 val)
;;   (def x (assoc key1 *parser-alist*))
;;   (if x
;;     (let (y (assoc key2 (cdr x)))
;;       (if y
;;         (set! (cdr y) val)
;;         ;; append!
;;         (append! [key2 val] (cdr x))))
;;     (push! [key1 [key2 val]] *parser-alist*))
;;   'ok)

;; (def (apair-val x)
;;   (match x
;;     ((? pair?) (cadr x))
;;     ((? list?) (cdr x))
;;     (else #f)))

;; (def* get
;;   ((key1)
;;    (alet (x (assgetq key1 *parser-alist*))
;;      (map apair-val x)))
;;   ((key1 key2)
;;    (alet (x (assgetq key1 *parser-alist*))
;;      (assgetq key2 x))))