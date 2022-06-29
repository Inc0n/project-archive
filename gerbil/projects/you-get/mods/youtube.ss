
(import :std/format)
(import :std/pregexp)
(import :std/net/request)
(import :std/net/uri) ;; form-url-decode
(import :std/text/json) ;; json
(import :std/ref) ;; generic ref
(import :std/sugar) ;;

(import :you-get/utils)
(import :you-get/you-get)

(def (download url (output-dir "."))
  (let ((v-id (or (match-grouped1 "youtu\.be/([^?/]+)" url)
                  (match-grouped1 "youtube\.com/embed/([^/?]+)")
                  (match-grouped1 "youtube\.com/v/([^/?]+)")
                  (match-grouped1 "youtube\.com/watch/([^/?]+)")
                  (parse-query-param url "v")
                  (parse-query-param (parse-query-param url "u") "v"))))
    (if v-id
      (kuwo-download-by-rid! v-id output-dir)
      (displayln "unsupported url resource for " url))))


(def (youtube-video-info-url vid)
     "this url will contain song details"
     (format "https://www.youtube.com/get_video_info?video_id={}&eurl=https%3A%2F%2Fy" vid))

(def (kuwo-song-url-url rid)
     "this url will generate a string that points to the mp3"
     ;; (declare (type string rid))
     (format "http://antiserver.kuwo.cn/anti.s?format=mp3&rid=MUSIC_~a&type=convert_url&response=url" rid))

(def (youtube-video-url vid)
  (string-append "https://www.youtube.com/watch?v=" vid))


(def (kuwo-download-by-rid! rid (output-dir "."))
  (def (get-vid-info info key)
    (assoc key info string=?))
  (def (get-html-play-url ytplayer-config)
    (string-append "https://www.youtube.com"
                   (ref ytplayer-config 'assets 'js)))
  (def (get-config-js video-page)
    (string->json-object
     (match-grouped1 "ytplayer.config\s*=\s*([^\n]+?});"
                     video-page)))
  (def (get-stream-list ytplayer-config)
    (chain (ref ytplayer_config 'args 'url_encoded_fmt_stream_map)
      (string-split <> #\,)))
  (def (get-player-response-js ytplayer-config)
    (string->json-object
     (ref ytplayer_config 'args 'player_response)))
  (let ((info (form-url-decode (get-content (youtube-video-info-url rid))))
        (url (get-content (kuwo-song-url-url rid))))
    (match (get-vid-info info "status")
      ("ok"
       (match (get-vid-info info "use_cipher_signature")
         ("False"
          (let ((title (parse.unquote_plus
                        (ref
                         (string->json-object
                          (get-vid-info info "player_response"))
                         'videoDetails 'title))))
            (let* ((video-page (get-content (youtube-video-url rid)))
                   (ytplayer-config (get-config-js video-page))
                   (html5player (get-html-play-url ytplayer-config))
                   (args (hash-get ytplayer-config 'args)))
              (match (hash-get args 'url_encoded_fmt_stream_map)
                (#f (get-stream-list ytplayer-config))
                (_ (ref (get-player-response-js ytplayer-config)
                        'streamingData 'formats))))))
         (_ ;; Parse video page instead
          (let* ((video-page (get-content (youtube-video-url rid)))
                 (ytplayer-config (get-config-js video-page))
                 (title (ref (get-player-response-js ytplayer-config)
                             'videoDetails 'title))
                 (html5player (get-html-play-url ytplayer-config)))
            (get-stream-list ytplayer-config)))))
      ("fail"
       (match (get-vid-info info "errorcode")
         ("150"
          (let* ((video-page (get-content (youtube-video-url rid)))
                 (ytplayer-config (get-config-js video-page))
                 (title (ref ytplayer-config 'args 'titles))
                 (js-name (ref ytplayer-config 'assets 'js)))
            (get-stream-list ytplayer-config)))
         ("100" (error "[failed] This video doesn't exist, video-id: " rid))
         (_ (error "[failed] Reason: " (get-vid-info info "reason")))))
      (_ (error "[failed] Unknown status. Mission failed, we'll get them next time. video-id:" rid))))

  (for (stream stream-list)
    (if (string? stream)
      (let* ((metadata (form-url-decode stream))
             (stream-itag (assoc metadata 'itag)))
        (push ['mime . (string-splt (assoc metadata 'type) #\;)]
              (hash-get streams stream-itag))
        (push ['container . (mime->container (string-splt (assoc metadata 'type) #\;))]
              (hash-get streams stream-itag)))
      (let ((stream-itag (ref metadata 'itag)))
        (hash-set! streams
                   (hash ('itag (string (ref stream 'itag)))
                         ('url (ref stream 'url))
                         ('quality (ref stream 'quality))
                         ('type (ref stream 'mimeType))
                         ('mime (car
                                 (string-split (ref stream 'mimeType) #\;)))
                         ('container (mime->container
                                      (car (string-splt (assoc metadata 'type) #\;))))))
        (push ['mime . (string-splt (assoc metadata 'type) #\;)]
              (hash-get streams stream-itag))
        (push ['container . (mime->container (string-splt (assoc metadata 'type) #\;))]
              (hash-get streams stream-itag))))))


(def-you-get "youtube.com" download)

;; download-playlist=kuwo-playlist-download