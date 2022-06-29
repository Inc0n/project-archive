
(import :std/getopt ;; unix opt
        :std/xml ;; sxml->xhtml-string
        :gerbil/gambit/exceptions) ;; display-exception
(import :std/misc/ports) ;; read-line

(import :org-parser/parser)
(import :org-parser/to-html)

(def (org->html title lst)
  (def org-sxml (org->sxml lst))
  (sxml->xhtml-string
   `(html (@ (lang "en"))
          (head (title ,title)
                (meta (@ (content "text/html; charset=utf-8")
                         (http-equiv "content-type")))
                (meta (@ (name "viewport")
                         (content "width=device-width, initial-scale=1")))
                (meta (@ (name "theme-color") (content "#4F4F4F")))
                (link (@ (rel "stylesheet")
                         (href "/static/bootstrap-4.5.2-dist/css/bootstrap.min.css"))))
          (body (div (@ (class "container mt-3"))
                     (div (@ (class "btn-toolbar"))
                          ,@(gen-navi-bar navi-name))
                     (div (@ (class "card"))
                          (div (@ (class "card-body"))
                               (h5 ,title)
                               ,@org-sxml))
                     (div (hr) ;; visual line
                          (p "@ 2020 Home Page")))))))

(def (main . args)
  (def to-html-cmd
    (command 'to-html help: "format org-file to html"
             (argument 'in-path help: "path to org file")
             (option 'output "-o" "--output" help: "output to path")))
  (def help-cmd
    (command 'help help: "display this help message"
             (optional-argument 'command value: string->symbol)))
  (def gopt (getopt parse-org-cmd
                    test-lexer-cmd
                    test-parse-cmd
                    help-cmd))
  (try
   (let ((values cmd opt) (getopt-parse gopt args))
     (case cmd
       ((to-html)
        (let (x (org->html
                 (parse-org (hash-ref opt 'in-path))))
          (if (hash-get opt 'output)
            (displayln x)
            (call-with-output-file (hash-get opt 'output)
              (cut write-string (list->string x) <>)))))))
   (catch (getopt-error? exn)
     (getopt-display-help exn "parse-org" (current-error-port))
     (exit 1))
   (catch (e)
     (display-exception e (current-error-port))
     (exit 2))))

(def (main . args)
  (org->html (parse-org )))