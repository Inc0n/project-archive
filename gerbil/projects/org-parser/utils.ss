
(import :std/misc/list) ;; butlast

(export #t)

(def (tag tag-name token)
  [tag-name token])

(def debugln displayln)

(def (tagged? tag x)
  (and (pair? x)
       (eq? (car x) tag)))

(def (pair-assq key alst)
  "use cadr for pair alist, cadr version of assgetq"
  (alet (p (assq key alst))
    (cadr p)))

;; token construct

(def (make-token tag item) (cons tag item))
(def token-tag car)
(def token-val cdr)

(def (token-attri-get tokn attri)
  (pair-assq attri (token-val tokn)))

;; elmt construct

(def* make-elmt
  ((tag lst)
   (match lst
     ([x] [tag . x])
     (else ;; [tag . lst]
      (error "make-elmt: unhandled elmt" lst))))
  ((tag attris body)
   [tag ['@ . attris] . body]))

(def has-attri?
  (match <>
    ([_ ['@ . _] . _] #t)
    (else #f)))

(def (elmt-tag elmt) (car elmt))
(def (elmt-attri elmt)
  (if (has-attri? elmt)
    (cdadr elmt)
    []))
(def (elmt-body elmt)
  (if (has-attri? elmt)
    (cddr elmt)
    (cdr elmt)))

(def (elmt-attri-get elmt attri)
  (pair-assq attri (elmt-attri elmt)))

;;

(def (elmt-add-body elmt body)
  (if (null? (elmt-body elmt))
    (append elmt body)
    (error "elmt-add-body: adding body to elmt has body" elmt body)))

(def (elmt-replace-body elmt proc)
  (make-elmt (elmt-tag elmt)
             (elmt-attri elmt)
             (proc (elmt-body elmt))))

(def (elmt-replace-tag-compose new-tag (attri []))
  (lambda (elmt)
    (make-elmt new-tag
               (append (elmt-attri elmt) attri)
               (elmt-body elmt))))