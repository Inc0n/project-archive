
(import :std/format
        :std/getopt
        :std/text/json
        :std/net/request
        :std/pregexp)
(import :std/misc/string)

;;
(def +watch-url+ "https://www.youtube.com/watch?v=")
(def (get-youtube-url video-id)
  (string-append +watch-url+ video-id))

(def (split-string str match)
  (let (len (string-length match))
    (let loop ((str str) (acc []))
      (let (i (string-contains str match))
        (if i
          (loop (substring str
                           (+ i len)
                           (string-length str))
                (cons (substring str 0 i) acc))
          (reverse (cons str acc)))))))

(def (get-yt-vid-html video-id)
  (request-text
   (http-get
    ;; '\\u0026', '&'
    ;; trim \)
    (get-youtube-url video-id))))

(def (extract-caption-json html video-id)
  (def splitted-html (split-string html "\"captions:\""))
  (alet* ((x (split-string (cadr splitted-html)
                           ",\"videoDetails"))
          (x (car x))
          (x (string-subst x "\n" "")))
    (string->json-object x)))