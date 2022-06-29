
(import :std/xml
        :std/iter ;; for
        :std/text/json
        :std/net/httpd)

(export #t)

(def (sort-list pred lst)
  (let aux ((lst lst)
            (acc []))
    (match lst
      ([] (values #f (reverse acc)))
      ([x . lst]
       (if (pred x)
         (values x (append (reverse acc) lst))
         (aux lst (cons x acc)))))))

(def (html-template title . body)
  (sxml->xhtml-string
   `(html (@ (lang "en"))
          (head (title ,title)
                (meta (@ (content "text/html; charset=utf-8")
                         (http-equiv "Content-Type")))
                (meta (@ (name "viewport")
                         (content "width=device-width, initial-scale=1")))
                (meta (@ (name "theme-color") (content "#4F4F4F")))
                (link (@ (rel "stylesheet")
                         (href "/static/bootstrap-4.5.2-dist/css/bootstrap.min.css")))
                (link (@ (rel "stylesheet")
                         (href "/static/my-style.css")))
                (script (@ (src "/static/jquery/3.5.1/jquery.min.js")))
                (script (@ (src "/static/bootstrap-4.5.2-dist/js/bootstrap.min.js"))))
          (body (div (@ (class "container mt-3"))
                     ,@body
                     (div (hr) ;; visual line
                          (p "@ 2020 Home Page")))))))

(def (content-type file)
  (match (path-extension file)
    (".html" "text/html")
    ;; ("ncx" "application/x-dtbncx+xml")
    (".js" "application/javascript")
    (".css" "text/css")
    (".jpg" "image/jpeg")
    (else "text/plain")))

;;

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

;; list utils

(def (flatmap proc lst)
  (foldl (lambda (x acc)
           (append acc (proc x)))
         []
         lst))

(def (unwanted-code) ;; /echo
  (def (echo-handler req res)
    (let* ((content-type
            (assget "Content-Type" (http-request-headers req)))
           (headers
            (if content-type
              [["Content-Type" . content-type]]
              [])))
      (http-response-write res 200 headers
                           (http-request-body req))))

  ;; /headers[?json]
  (def (headers-handler req res)
    (let (headers (http-request-headers req))
      (if (equal? (http-request-params req) "json")
        (write-json-headers res headers)
        (write-text-headers res headers))))

  (def (write-json-headers res headers)
    (let (content
          (json-object->string
           (list->hash-table headers)))
      (http-response-write res 200 '(("Content-Type" . "application/json"))
                           content)))

  (def (write-text-headers res headers)
    (http-response-begin res 200 '(("Content-Type" . "text/plain")))
    (for ([key . val] headers)
      (http-response-chunk res (string-append key ": " val "\n")))
    (http-response-end res))
  (error "not meant to be called"))