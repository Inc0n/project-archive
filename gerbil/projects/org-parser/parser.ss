#!/usr/bin/env gxi

(import :org-parser/utils)
(import :org-parser/lexer)

(import :std/misc/ports) ;; read-file-lines

(export parse-org parse-tokens
        heading-level heading-title)

;; utils

(def (list-split pred tokens)
  (let aux ((tokens tokens)
            (acc []))
    (match tokens
      ([] (values (reverse acc) []))
      ([token . cdr-tokens]
       (if (pred token)
         (values (reverse acc) tokens)
         (aux cdr-tokens
              (cons token acc)))))))
;;

(def heading? (cut tagged? 'heading <>))
(def heading-level (cut elmt-attri-get <> level:))
(def heading-status (cut elmt-attri-get <> status:))
(def heading-text (cut elmt-attri-get <> text:))

(def (heading-title heading)
  (string-append (heading-level heading) " " (heading-text heading)))

(def (split-headings tokens level)
  (list-split (lambda (token)
                (and (heading? token)
                     (string<=? (heading-level token) level)))
              tokens))

(def (orgify tokens)
  (match (car tokens)
    ((and elmt (? heading?))
     (let ((values heading-body rest-tokens)
           (split-headings (cdr tokens) (heading-level elmt)))
       (values (elmt-add-body elmt
                              (parse-tokens heading-body))
               rest-tokens)))
    (elmt
     (values elmt (cdr tokens)))))

(def (parse-tokens tokens)
  (let aux ((tokens tokens)
            (acc []))
    (if (null? tokens)
      (reverse acc)
      (let ((values new-token tokens)
            (orgify tokens))
        (aux tokens
             (cons new-token acc))))))

(def (parse-org x)
  (if (or (string? x) (list? x))
    (parse-tokens (lexify x))
    (error "parse-org - expected string or list-of-strin" x)))