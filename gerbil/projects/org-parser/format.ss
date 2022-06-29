
(import :std/sugar)
(import :std/misc/func)

(import :std/misc/list) ;; push! snoc

(import :org-parser/utils)

;; utils
(defsyntax (append! stx)
  (syntax-case stx ()
    ((_ obj place)
     #'(set! place (snoc obj place)))))

;; alist utils
(def *parser-alist* [])

(def (put! key1 key2 val)
  (def x (assoc key1 *parser-alist*))
  (if x
    (let (y (assoc key2 (cdr x)))
      (if y
        (set! (cdr y) val)
        ;; append!
        (append! [key2 . val] (cdr x))))
    (push! [key1 [key2 . val]] *parser-alist*))
  'ok)

(def* get
  ((key1) (assgetq key1 *parser-alist*))
  ((key1 key2)
   (alet (x (assgetq key1 *parser-alist*))
     (assgetq key2 x))))

;; exports

(export to-format
        set-format
		;; def-format
		def-formatter)

;; framework

(def +elmt-tags+
  '(heading newline text))

(def *format-name* #f)
(def (set-format format-name)
  (unless (symbol? format-name)
	(error "set-format expects a symbol but got " format-name))
  (set! *format-name* format-name)
  (for-each (cut put! *format-name* <> #f)
			+elmt-tags+))

(def (def-formatter tag proc)
  (if *format-name*
	(put! *format-name* tag proc)
    (error "set-format-name first it's " *format-name*)))

;; interface

(defsyntax (def-format stx)
  (syntax-case stx ()
    ((_ name: format-name clauses ...)
     #'(begin (displayln format-name)
              (def-format clauses ...)))
    ((_ (name exp) clauses ...)
     #'(begin (displayln name exp)
              (def-format clauses ...)))))

;; api

(def (to-format name lst)
  (let (formatters (get name))
	(for-each (lambda (formatter)
                (when (null? (cdr formatter))
				  (error name " formatter not implemented for " (car formatter))))
			  formatters)
	(let aux ((elmt lst))
      (cond ((not (pair? elmt)) elmt)
            ((null? elmt) [])
            ((symbol? (car elmt))
             (displayln elmt)
             (let ((elmt (elmt-replace-body elmt aux))
                   (formatterp (assgetq (car elmt) formatters)))
	           (formatterp elmt)))
            (else (map aux elmt))))))