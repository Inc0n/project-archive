
(import :gerbil/expander/stx)
(import :std/sugar) ;; is
(import :std/misc/iter) ;; for/collect
(import :std/misc/func)

(def (default-action-exp match-exp action-exp)
  (let* ((lst (for/collect ((x (in-naturals))
                            (y match-exp))
                [(make-var-sym x) y]))
         (vars (map car lst)))
    (values lst
            `(lambda (env)
               (cons (get-env-var env 'name)
                     (map (lambda (var)
                            (get-env-var env var))
                          vars))))))

(begin-syntax
  (def (list-split x lst)
    (let aux ((lst lst)
              (acc []))
      (cond ((null? lst)
             (values (reverse acc) #f))
            ((eq? x (car lst))
             (values (reverse acc) (cdr lst)))
            (else
             (aux (cdr lst)
                  (cons (car lst) acc))))))
  (def (make-var-sym num)
    (make-sybmol '$ num))
  (def (default-action-exp match-exp action-exp)
    (if action-exp
      (values match-exp
              action-exp)
      (values (for/collect ((x (in-naturals))
                            (y match-exp))
                (list (make-var-sym x) y))
              action-exp)))
  (def (expand-def-parser stx)
    (displayln "start " (syntax->datum stx))
    (match (syntax->datum stx)
      ([name ': . exps]
       (let* (((values match-exp action-exp)
               (list-split '=> exps))
              ((values match-exp action-exp)
               (default-exps match-exp action-exp)))
         ;; `',(list match: match-exp action: action-exp)
         `(list match: ',match-exp action: ',action-exp)))
      (exp (error "def-parser: expected ('Name : . form) got " exp)))))

(defsyntax (def-parser stx)
  (syntax-case stx ()
    ((_ . clauses)
     (with-syntax ((form
                    (cons 'list (stx-map expand-def-parser #'clauses))))
       #'form))))

(displayln
 (def-parser
   (repeat : (lex: repeat))
   (Match  : #\[ (=: $1 (0+ Pat)) #\] => ['Match $1])
   (Repeat : (=: $1 Pat) repeat       => ['Rep $1])
   (Or     : (=: $1 Pat) #\| (=: $2 Pat) => ['Or $1 $2])
   (Pat    : any)))