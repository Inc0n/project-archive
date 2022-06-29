(import :std/text/base64)
(import :std/text/utf8)
(import :std/text/json)
(import :std/ref) ;; generic ref

(def (download url info-only output-dir)
  (cond ((string-find url "5sing")
         (let* ((html (get-content url))
                (ticket (match-grouped1 "\"ticker\":\s*\"(.*)\"" html))
                (j (string->json-object
                    (string->utf8 (base64-decode ticket)))))
           (let ((url (ref j 'file))
                 (title (ref j 'songName)))
             (log-url-media-info url)
             (when ((is #f) info-only)
               (download-url! url file-name)))))
        ((string-find url "hash")
         (kugou-download-by-hash url output-dir info-only))
        (else ;; for the www.kugou.com/
         (kugou-download-playlist url output-dir info-only)
         ;; raise NotImplementedError(url)
         )))

(def (kugou-get-url hash-val album-id)
  (format "http://www.kugou.com/yy/index.php?r=play/getdata&hash={}&album_id={}&mid=123" hash-val album-id))

(def (kugou-download-by-hash url output-dir)
  ;; url-sample:http://www.kugou.com/song/#hash=93F7D2FC6E95424739448218B591AEAF&album_id=9019462
  (let* ((hash-val (match-grouped1 "hash=(\w+)" url))
         (album-id (or (match-grouped1 "album_id=(\d+)")
                       "123")))
    (let* ((html (get-content (kugou-get-url hash-val album-id)))
           (j (string->json-object html))
           (url (ref j 'data 'play_url))
           (title (ref j 'data 'audio_name)))
      (when (string-empty? url)
        (error "song is copyrighted " url))
      (log-url-media-info url)
      (when ((is #f) info-only)
        (download-url! url file-name)))))

;; def kugou_download_playlist(url, output_dir='.', merge=True, info_only=False, **kwargs):
;; urls = []

;; # download music leaderboard
;;   # sample: http://www.kugou.com/yy/html/rank.html
;;     if url.lower().find('rank') != -1:
;;     html = get_html(url)
;;     pattern = re.compile('<a href="(http://.*?)" data-active=')
;;     res = pattern.findall(html)
;;     for song in res:
;;     res = get_html(song)
;;     pattern_url = re.compile('"hash":"(\w+)".*"album_id":(\d)+')
;;     hash_val, album_id = res = pattern_url.findall(res)[0]
;;     if not album_id:
;;     album_id = 123
;;     urls.append('http://www.kugou.com/song/#hash=%s&album_id=%s' % (hash_val, album_id))

;;     # download album
;;       # album sample:   http://www.kugou.com/yy/album/single/1645030.html
;;         elif url.lower().find('album') != -1:
;;         html = get_html(url)
;;         pattern = re.compile('var data=(\[.*?\]);')
;;                                   res = pattern.findall(html)[0]
;;                                   for v in json.loads(res):
;;                                   urls.append('http://www.kugou.com/song/#hash=%s&album_id=%s' % (v['hash'], v['album_id']))

;;                                   # download the playlist        
;;                                     # playlist sample:http://www.kugou.com/yy/special/single/487279.html
;;                                       else:
;;                                       html = get_html(url)
;;                                       pattern = re.compile('data="(\w+)\|(\d+)"')
;;                                       for v in pattern.findall(html):
;;                                       urls.append('http://www.kugou.com/song/#hash=%s&album_id=%s' % (v[0], v[1]))
;;                                       print('http://www.kugou.com/song/#hash=%s&album_id=%s' % (v[0], v[1]))

;;                                       # download the list by hash
;;                                         for url in urls:
;;                                         kugou_download_by_hash(url, output_dir, merge, info_only)


(def-you-get "kugou.com" download)