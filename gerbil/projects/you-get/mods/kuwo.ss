
(import :std/format)
(import :std/pregexp)
(import :std/net/request)

(import :you-get/utils)
(import :you-get/you-get)

(def (download url (output-dir "."))
     (let ((rid (match-grouped1 "play_detail/(\\d+)" url)))
       (if rid
           (kuwo-download-by-rid! rid output-dir)
           (displayln "unsupported url resource for " url))))

(def (kuwo-song-detail-url rid)
     "this url will contain song details"
     (string-append
      "http://player.kuwo.cn/webmusic/st/getNewMuiseByRid?rid=MUSIC_" rid))

(def (kuwo-song-url-url rid)
     "this url will generate a string that points to the mp3"
     ;; (declare (type string rid))
     (format "http://antiserver.kuwo.cn/anti.s?format=mp3&rid=MUSIC_~a&type=convert_url&response=url" rid))


(def (kuwo-download-by-rid! rid (output-dir "."))
     (let ((html (get-content (kuwo-song-detail-url rid)))
           (url (get-content (kuwo-song-url-url rid))))
       (when (string=? "res not found" url)
         (error url "for rid: " rid))
       (log-url-media-info url)
       (let ((title (get-elm-by-tagname html "name"))
             (artist (get-elm-by-tagname html "artist"))
             (item-name (url-last-path-component url)))
         (let ((file-name
                (string-append title "-" artist "." item-name)))
           (displayln (format "downloading ~s -> ~s ..." url file-name))
           (download-url! url file-name)
           (displayln "downloaded")
           html))))

(def-you-get "kuwo.cn" download)

;; download-playlist=kuwo-playlist-download