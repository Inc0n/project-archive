;;; -*- Gerbil -*-
;;; (C) 1nc0n
;;; Simple web server
(import :std/net/httpd
        :std/net/request
        :std/net/address
        :std/misc/func ;; compose1
        :std/sugar
        :std/getopt
        :std/xml
        :std/srfi/13 ;; string-contains
        :std/misc/string ;; string-trim-prefix
        :gerbil/gambit/threads
        :gerbil/gambit/ports) ;; write-string

(import :std/text/base64)
(import :std/misc/ports) ;; read-file-u8vector 

(import :website/utils
        :org-parser/parser
        :org-parser/to-html)
;; 
;; :std/format

;; https://getbootstrap.com/docs/4.5/getting-started/theming/#grays

(export main)

;;; main

    ;; (http-register-handler httpd "/echo" echo-handler)
    ;; (http-register-handler httpd "/headers" headers-handler)
(def (run address)
  (let (httpd (start-http-server! address mux: (make-recursive-http-mux default-handler)))
    (http-register-handler httpd "/" root-handler)
    (http-register-handler httpd "/files" files-handler)
    (http-register-handler httpd "/static" serve-static-handler)
    (http-register-handler httpd "/uploaded" serve-static-handler)
    (http-register-handler httpd "/upload" upload-handler)
    (http-register-handler httpd "/submit.ss" submit-handler)
    (http-register-handler httpd "/about" about-handler)
    (thread-join! httpd)))

(def *root-paths-pair* [["Home" . "/"]
                        ["Upload" . "/upload"]
                        ["Uploaded" . "/uploaded"]
                        ["Files" . "/files"]
                        ["About" . "/about"]])

(def (gen-navi-bar this-path)
  (let ((values match lst)
        (sort-list (match <>
                     ([name . path] (string=? path this-path)))
                   *root-paths-pair*))
    (cons `(h1 (@ (class "pr-2"))
               ,(car match))
          (map (lambda (x)
                 (let ((name (car x))
                       (path (cdr x)))
                   `(div (@ (class "btn-group"))
                         (form (@ (action ,path))
                               (button (@ (class "btn btn-link mt-3 px-1")
                                          (type "sumbit"))
                                       (u ,name))))))
               lst))))

(def *root-html-response*
  (html-template
   "home"
   `(div (@ (class "btn-toolbar"))
         ,@(gen-navi-bar "/"))
   `(div (@ (class "card"))
         (div (@ (class "card"))
              (div (@ (class "card-body"))
                   (h5 "Readme")
                   (p (@ (class "card-text"))
                      "This is some text within a card body."))))))

;; /
(def (root-handler req res)
  (def addr (inet-address->string (http-request-client req)))
  (displayln "connected from " addr)
  (http-response-write res 200 '(("Content-Type" . "text/html"))
                       *root-html-response*))

;;

(def (gen-dir-content navi-name disk-path url-path)
  (html-template
   "files"
   `(div (@ (class "btn-toolbar"))
         ,@(gen-navi-bar navi-name))
   `(div (@ (class "card"))
         (div (@ (class "card-body"))
              (h5 ,(string-append url-path ":"))
              (div (@ (class "btn-group-vertical"))
                   ,(map (lambda (path)
                           `(a (@ (href ,(path-expand path url-path))
                                  (class "btn btn-link mt-0 p-1 card-text text-left"))
                               ,path))
                         (directory-files disk-path)))))))

(def (serve-error res)
  (def html-response
    (html-template
     "Page not found"
     `(p "404 Page not found")
     `(button (@ (onclick "window.history.back()")
                 (class ""))
              "Go Back")))
  (http-response-write res 404 '(("Content-Type" . "text/html"))
                       html-response))

;;

(def (serve-htmls-dir url-path disk-path)
  (html-template
   "files"
   `(div (@ (class "btn-toolbar"))
         ,@(gen-navi-bar "/files"))
   `(div (@ (class "card"))
         (div (@ (class "card-body"))
              (h5 ,(string-append url-path ":"))
              (div (@ (class "btn-group-vertical"))
                   ,@(map (lambda (path)
                            `(div
                              (h5 (@ (class "card-title"))
                                  ,(string-append url-path ":"))
                              (p (@ (href ,(path-expand (path-expand path url-path)
                                                        "/"))
                                    (class "card-text mt-0 p-1 text-left"))
                                 ,path)))
                          (directory-files disk-path)))))))

;; (equal?
;;  (string->bytes (read-file-string "test.org"))
;;  (read-file-u8vector "test.org"))

;; (cut read-all-as-u8vector <> 8192)
;; (call-with-input-string (read-file-string "test.txt")
;;                         (cut read-char <>))

(def (read-all-as-u8vector port (bufsize 8192))
  (let lp ((buf (make-u8vector bufsize))
           (u8s []))
    (let (len (read-subu8vector buf 0 bufsize port))
      (if (= len bufsize)
        (lp (make-u8vector bufsize) (cons buf u8s))
        (begin
          (u8vector-shrink! buf len)
          (append-u8vectors (reverse (cons buf u8s))))))))

;;

(def (bad->chinese bytes)
  (def (buf-setter buf)
    (let (i 0)
      (lambda (u)
        (when u
          (u8vector-set! buf i u)
          (set! i (1+ i)))
        i)))
  (let* ((len (u8vector-length bytes))
         (buf (make-u8vector len))
         (setter! (buf-setter buf)))
    (let lp ((i 0) (is-195 #f))
      (if (= i len)
        (begin
          (u8vector-shrink! buf (setter! #f))
          buf)
        (let (u (u8vector-ref bytes i))
          (cond ((= u 195) (lp (1+ i) #t))
                ((= u 194) (lp (1+ i) #f))
                (is-195
                 (setter! (+ u 64))
                 (lp (1+ i) #f))
                (else
                 (setter! u)
                 (lp (1+ i) #f))))))))

;;

(def (gen-org navi-name disk-path url-path)
  (html-template
   url-path
   `(div (@ (class "btn-toolbar"))
         ,@(gen-navi-bar navi-name))
   `(div (@ (class "card card-body"))
         (h4 ,(path-strip-directory url-path))
         ,@(org->sxml
            (parse-org disk-path)))))

(def (serve-static navi-name url-path res)
  (let* (((values base path) (string-slice url-path "/"))
         (disk-base-path (lookup-file-path base)))
    (if (eq? disk-base-path #f)
      (serve-error res)
      (let (disk-path (path-expand path disk-base-path))
        (displayln "getting " disk-path " " url-path)
        (match disk-path
          ((not (? file-exists?))
           (serve-error res))
          ((? (is file-type 'directory))
           (http-response-write res 200 '(("Content-Type" . "text/html"))
                                (gen-dir-content
                                 "/files" disk-path url-path)))
          ((? (is path-extension ".org"))
           (let (html (string->bytes
                       (gen-org
                        "/files" disk-path url-path)))
             (http-response-write res 200 '(("Content-Type" . "text/html"))
                                  (bad->chinese html))))
          ((? (is path-extension ".html"))
           (http-response-file res `(("Content-Type" . "text/html"))
                               (path-expand disk-path)))
          (else
           (http-response-file res `(("Content-Type" . ,(content-type disk-path)))
                               (path-expand disk-path))))))))

(def (serve-static-handler req res)
  (let (path (string-trim-prefix "/" (http-request-path req)))
    (if (and (file-exists? path) (eq? (file-type path) 'regular))
      (http-response-file res `(("Content-Type" . ,(string-append (content-type path)
                                                                  "; charset=utf-8")))
                          (path-expand path))
      (serve-error res))))

;; file serving
(def *file-serving-path* ;; allowed file serving pathes
  [["cooking" . "~/sources/notes/cooking/"]
   ["files" . "files/"]])

(def lookup-file-path (cut assget <> *file-serving-path*))

;; /files
(def (files-handler req res)
  (let (path (http-request-path req))
    (serve-static "/files" (substring path 1 (string-length path)) res)))

;; /upload
(def (upload-handler req res)
  (def html-response
    (delay
      (html-template
       "upload"
       `((div (@ (class "btn-toolbar"))
              ,@(gen-navi-bar "/upload"))
         (div (@ (class "card"))
              (div (@ (class "card-body"))
                   (h5 "choose file to upload")
                   (form (@ (action "/submit.ss")
                            (method "post")
                            (enctype "multipart/form-data"))
                         (div (@ (class "form-group"))
                              (input (@ (type "file") (id "file")
                                        (value "file") (name "filename")))
                              (input (@(type "submit") (value "submit")))))))))))
  (http-response-write res 200 '(("Content-Type" . "text/html"))
                       (force html-response)))

(def (get-boundary content-type)
  (alet (start (string-contains content-type "boundary="))
    (substring content-type
               (+ start (string-length "boundary="))
               (string-length content-type))))

;; (def +x+ "\nContent-Disposition: form-data; name=\"filename\"; filename=\"recover.sh\"\nContent-Type: application/x-shellscript")
(defstruct form-header (name content params))

(def (string-slice str1 str2 (start 0))
  (let (i (string-contains str1 str2 start))
    (if i
      (values (substring str1 0 i)
              (substring str1
                         (+ i (string-length str2))
                         (string-length str1)))
      (values str1 ""))))

(def (split-string str1 str2)
  (let (len (string-length str2))
    (let loop ((str1 str1))
      (let (i (string-contains str1 str2))
        (if i
          (cons (substring str1 0 i)
                (loop (substring str1
                                 (+ i len)
                                 (string-length str1))))
          [str1])))))

(def (parse-form-header form)
  (def (parse-param param)
    (def (unstrify val)
      (substring val 1 (1- (string-length val))))
    (let ((values name val) (string-slice param "="))
      [name (unstrify val)]))
  (def (parse-header header)
    (if (string-empty? header)
      #f
      (let* (((values name attri) (string-slice header ": "))
             (args (split-string attri "; ")))
        (make-form-header name
                          (substring (car args) 0 (string-length (car args)))
                          (flatmap parse-param (cdr args))))))
  (filter-map parse-header
              (string-split form #\newline)))

(def (parse-form-data body)
  (let (border (string-contains body "\n\n"))
    (values (parse-form-header (substring body 0 border))
            (substring body (+ border 2) (string-length body)))))

(def (parse-post-request req)
  (def body (bytes->string (http-request-body req)))
  (or (alet* ((content-type (assoc "Content-Type" (http-request-headers req)))
              (boundary (get-boundary (cdr content-type)))
              (boundary (string-append "--" boundary))
              (_ (and (string-prefix? boundary body)
                      (string-suffix? (string-append boundary "--\n") body))))
        (let* ((len (string-length boundary))
               (body (substring body
                                len
                                (- (string-length body)
                                   (+ len 3)))))
          (parse-form-data body)))
      (values 'error "bad file post request")))

;; /submit.ss
(def (submit-handler req res)
  (let (headers (http-request-headers req))
    (let ((values forms data) (parse-post-request req))
      (displayln forms)
      (if (eq? forms 'error)
        (http-response-write res 200 '(("Content-Type" . "text/plain"))
                             data)
        (let* ((form (find (compose1 (is "Content-Disposition") form-header-name)
                           forms))
               (filename (pget "filename" (form-header-params form))))
          (create-directory* "files")
          (call-with-output-file (string-append "files/" filename)
            (cut write-string data <>))
          (http-response-write res 200 '(("Content-Type" . "text/plain"))
                               (string-append filename " uploaded")))))))

(def about-html-response
  (html-template
   "About"
   `(div (@ (class "btn-toolbar"))
         ,@(gen-navi-bar "/about"))
   `(div (@ (class "card"))
         (div (@ (class "card-body"))
              (h5 "")
              (p (@ (class "card-text"))
                 "My website made with gerbil")))))

;; /about
(def (about-handler req res)
  (http-response-write res 200 '(("Content-Type" . "text/html"))
                       about-html-response))

;; default
(def (default-handler req res)
  (serve-error res))

(def (main . args)
  (def gopt
    (getopt (option 'address "-a" "--address"
                    help: "server address"
                    default: "0.0.0.0:8080")))

  (displayln "running at 0.0.0.0:8080")
  (try
   (let (opt (getopt-parse gopt args))
     (run (hash-get opt 'address)))
   (catch (getopt-error? exn)
     (getopt-display-help exn "hellod" (current-error-port))
     (exit 1))))

(main)