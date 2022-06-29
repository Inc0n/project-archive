
;; (import :std/misc/ports)
(import :gerbil/gambit/ports) ;; write-u8vector

(import :std/format) ;; format printf
(import :std/pregexp)
(import :std/net/request)

(export #t)

(def (get-content url)
  (request-text (http-get url)))

(def (download-url! url file-path)
  (let ((u8 (request-content (http-get url))))
    (call-with-output-file
        file-path
      (cut write-u8vector u8 <>))))

(def (get-elm-by-tagname str-xml tag)
  (let ((regex
         (format "<~a>(.*)</~a>" tag tag)))
    (match-grouped1 regex str-xml)))

(def (match-grouped1 pat str)
  (match (pregexp-match pat str)
    ([_ x . _] x)
    (else #f)))

;;; url

(def (parse-query-param url param)
  "Parses the query string of a URL and returns the value of a parameter.
   Args: url: A URL.  param: A string representing the name of the
parameter.  Returns: The value of the parameter.
  "
  (match (string-split url #\?)
    ([_] #f)
    ([_url . params]
     (alet ((matched-param (find (cut string-prefix? param <>)
                                 params)))
       (substring matched-param
                  (1+ (string-length param))
                  (string-length matched-param))))
    (_ #f)))


;; ;; "([\\w\\d]+\\.)?([\\w\\d]+\\.[\\w\\d]+)/?"

(def (url-domain url)
  (let* ((end (string-rindex url #\.))
         (start (string-rindex url #\. (1- end)))
         (end (or (string-index url #\/ end)
                  (string-length url))))
    (substring url (1+ start) end)))

(def (url-last-path-component url)
  (and (string? url)
       (let ((start (string-rindex url #\/)))
         (substring url (1+ start) (string-length url)))))

;; ;;

(def (log-url-media-info url)
  (printf "from ~s got ~a~%"
          (url-domain url)
          (url-last-path-component url)))