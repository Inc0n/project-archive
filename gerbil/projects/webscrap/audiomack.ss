
(import :std/net/request)
(import :gerbil/gambit/ports) ;; write-u8vector
(import :std/srfi/13)

(def (get-json url)
  (request-json (http-get url)))

;; (def (download-url! url file-path)
;;   (let ((u8 (request-content (http-get url))))
;;     (call-with-output-file
;;         file-path
;;       (cut write-u8vector u8 <>))))
zaniyahdamon1611
(let* ((respond (get-json "https://api.audiomack.com/v1/artist/zaniyahdamon1611/uploads/page/1?limit=20&oauth_consumer_key=audiomack-js&oauth_nonce=l4iOqtlBjc97z7WjZ2dP1oDkrzpG6y9L&oauth_signature_method=HMAC-SHA1&oauth_timestamp=1603216880&oauth_version=1.0&oauth_signature=Bj03BE3cdeG16+QA3Ce3tCSUtzE="))
	   (result (hash-key respond 'result)))
  )

(def (get-api api . more-args)
  (let (url (string-append
             "https://api.imjad.cn/cloudmusic/?type="
             api
             "&id="))
    (lambda (id)
      (get-json
       (string-append url id)))))

;; url param

(def (url-make-query url)
  (if (string-index-right url #\?)
    url
    (string-append url "?")))

(def (url-append-param url arg val)
   (let (url (url-make-query url))
     (url-just-append-param url arg val)))

(def (url-append-params url arg-val-pairs)
  (match arg-val-pairs
    ([[arg . val] . pairs]
     (url-append-params (url-append-param url arg val)
                        pairs))
    ([arg . val]
     (url-append-param url arg val))
    ([] url)
    (x (error "url-append-param: expected a pair or pairs but got" x))))

(def (url-just-append-param url arg val)
  (if val
    (string-append (if (string-suffix? url "&")
                     ""
                     "&")
                   arg "=" val)
    url))

;; api

(def (get-api-w-args api id more-args)
  (let (url (string-append
             "https://api.imjad.cn/cloudmusic/?type="
             api
             "&id="
             id))
    (get-json
     (url-append-params url more-args))))

(def (get-song id (br #f))
  (get-api-w-args "song" id [['br . br]]))

(def get-lyric (get-api "lyric"))

(def get-comments (get-api "comments"))

;; detail can be for
;; song
;; artist
;; album cover
(def get-detail (get-api "detail"))
(def get-playlist (get-api "playlist"))

;; search type const
(def +search/song+ 1)
(def +search/album+ 10)
(def +search/artist+ 100)
(def +search/playlist+ 1000)
(def +search/user+ 1002)
(def +search/mv+   1004)
(def +search/lyric+ 1006)
(def +search/podcast+ 1009)

(def +search-type+ [["song" . +search/song+]
                    ["album" . +search/album+]
                    ["artist" . +search/artist+]
                    ["playlist" . +search/playlist+]
                    ["user" . +search/user+]
                    ["mv"   . +search/mv+]
                    ["lyric" . +search/lyric+]
                    ["podcast" . +search/podcast+]])

;; TODO - broken search
(def (search type query)
  (let (url (string-append
             "https://api.imjad.cn/cloudmusic/?type=search&s="
             query))
    (get-json url)))

