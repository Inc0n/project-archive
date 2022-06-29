
(import :std/format
        :std/getopt
        :std/text/json
        :std/net/request
        :std/pregexp)

(def +time-offset+ "(Z|[+-]\\d{2}:\\d{2})")
(def +date-time-format+
  (string-append "\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}\\d{2}"
                 +time-offset+))
(def +date-time-regexp+ (pregexp +date-time-format+))

(export main)

(def +key+ "AIzaSyDbcPuYHirnI6JM2lnv5u14BUew8cqkCXk")

(defclass youtube-api (key)
  constructor: :init!
  final: #t)

(defmethod {:init! youtube-api}
  (lambda (self key)
    (set! (youtube-api-key self) key)))

;; (defsyntax (assert! stx)
;;   (syntax-case stx ()
;;     ((_ expr . message)
;;      #'(when expr
;;          (apply error message)))))

;;

(def (valid-args? args schematic)
  )

(def channel-type? (cut memq <> '(any show)))
(def event-type? (cut memq <> '(completed live upcoming)))
(def location?
  (match <>
    ([lat lon] (and (number? lat)
                    (number? lon)))
    (else #f)))

(def location-radius?
  ;; <number><unit>
  )

(def (between? num min max)
  (and (number? num)
       (>= num min)
       (<= num max)))

(defsyntax (def-sym-enum stx)
  (syntax-case stx ()
    ((_ name members)
     #'(def name (cut memq <> members)))))

(begin-syntax
  (def (entry->api-arg sym-convertp)
    (lambda (entry)
      (let (entry (syntax->datum entry))
        (match entry
          ((? symbol?)
           `(list ',entry ,(sym-convertp entry)))
          ([sym proc]
           `(list ',sym ,proc)))))))

(begin-syntax
  (def (camel->kebab/case exp)
    (def (index-of-first-cap str)
      (let lp ((i 0)
               (len (string-length str)))
        (cond ((= i len) #f)
              ((char-upper-case? (string-ref str i)) i)
              (else (lp (1+ i) len)))))
    (let* ((str (symbol->string exp))
           (i (index-of-first-cap (symbol->string exp))))
      (if i
        (make-symbol (substring str 0 i)
                     "-"
                     (string (char-downcase (string-ref str i)))
                     (substring str (1+ i) (string-length str))
                     '?)
        (make-symbol exp '?)))))

(defsyntax (build-api-arg-list stx)
  (syntax-case stx ()
    ((_ (sym-convertp: sym-convertp) . lst)
     (let (sym-convertp (eval-syntax #'sym-convertp))
       (displayln sym-convertp)
       ;; (when (not (procedure? sym-convertp))
       ;;   (error "build-api-arg-list expects procedure but got: " sym-convertp))
       (with-syntax ((magic
                      `(list ,@(stx-map (entry->api-arg sym-convertp)
                                        #'lst))))
         #'magic)))))

(build-api-arg-list (sym-convertp: camel->kebab/case)
                    symbol
                    string
                    (custom-key string?))

(def order? (cut memq <> '(date rating relevance title videoCount viewCount)))
(def page-token? string?)
(def date-time? (cut pregexp-match +date-time-regexp+ <>))
(def safe-search? (cut memq <> '(moderate none strict)))

(def region-code?
  (let (+region-code-regexp+ (pregexp "[A-Z]{2,3}|\\d{3}"))
    (cut pregexp-match +region-code-regexp+ <>)))

(def type? (cut memq <> '(channel playlist video)))
(def video-caption? (cut memq <> '(any closedCaption none)))
(def video-definition? (cut memq <> '(any high standard)))
(def video-dimension? (cut memq <> '(2d 3d any)))
(def video-Duration? (cut memq <> '(any long medium short)))
(def video-embeddable? (cut memq <> '(any true)))
(def video-type? (cut memq <> '(any episode movie)))
;; `

(with-api-arg-list (sym-convertp: camel->kebab/case)
                   (channelId string?)
                   (maxResult (cut between? <> 0 50))
                   order
                   pageToken
                   (publishedAfter ,date-time?))

(valid-args? args (with-api-arg-list (sym-convertp: camel->kebab/case)
                                     (channelId string?)
                                     channelType
                                     eventType
                                     location
                                     locationRadius
                                     (maxResult (cut between? <> 0 50))
                                     ;; (onBehalfOfContentOwner )
                                     order
                                     pageToken
                                     (publishedAfter ,date-time?)
                                     (publishedBefore ,date-time?)
                                     (q ,string?) ;; query
                                     (regionCode ,string?)
                                     (relevanceLanguage ,string?)
                                     safeSearch
                                     (topicId ,string?) ;; https://developers.google.com/youtube/v3/docs/search/list?apix_params=%7B%22part%22%3A%5B%22minecraft%22%5D%7D#
                                     type      ;; search-type
                                     videoCaption
                                     (videoCategoryId ,string?)
                                     videoDefinition
                                     videoDimension
                                     videoDuration
                                     videoEmbeddable
                                     ;; (videoLicense)
                                     videoType))

(defmethod {search youtube-api}
  (lambda (self part search-type order-by)
    (def (search-query-success? req)
      (= (request-status req) 200))
    (def req
      (http-get
       (format "https://www.googleapis.com/youtube/v3/search?part=~a&type=~a&maxResults=50&order=~a&key=~a" part search-type order-by (@ self key))))
    (if (search-query-success? req)
      (request-json req)
      #f)))

(def (save-json json to: file)
  (call-with-input-file file
    (cut write-string <> (json-object->string json))))

;; (def (str-string-join strs str)
;;   (let lp ((strs strs) (acc ""))
;;     (match strs
;;       ([s . strs] (lp strs (string-append acc s str)))
;;       ([s] (string-append acc s))
;;       ([] acc))))

(def (url-parameterize args)
  (string-join
   (map (cut string-join <> #\=)
        args)
   #\&))

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
       ((search) {search (make-youtube-api +key+)})))
   (catch (getopt-error? exn)
     (getopt-display-help exn "parse-org" (current-error-port))
     (exit 1))
   (catch (e)
     (display-exception e (current-error-port))
     (exit 2))))