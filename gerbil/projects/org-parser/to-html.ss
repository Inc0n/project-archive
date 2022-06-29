
(import :org-parser/utils)
(import :org-parser/parser)
(import :org-parser/format)

(export org->sxml)

(def (id-generator)
  (let (i 0)
    (lambda ()
      (def id (string-append "heading" (number->string i)))
      (set! i (fx1+ i))
      id)))

(def *id-gennerator* (id-generator))
(def (get-new-id) (*id-gennerator*))

(def (heading->h1 elmt)
  (let ((title (heading-title elmt))
        (body (elmt-body elmt)))
    (if (null? body)
      `(button (@ (class "btn btn-link text-left p-0 disabled"))
          (h5 ,title))
      (let (id (get-new-id))
        `(div
          (a (@ (href ,(string-append "#" id))
                (data-toggle "collapse"))
             (h5 ;; (@ (class ""))
              ,title))
          (div (@ (id ,id)
                  (class "collapse data"))
               ,@body))))))


(set-format 'sxml)

(def-formatter 'heading heading->h1)
(def-formatter 'newline (lambda (_elmt) '(br)))
(def-formatter 'text  (elmt-replace-tag-compose 'p))

(def (org->sxml lst)
  (to-format 'html lst))