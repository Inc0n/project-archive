(import :std/sugar ;; chain, try
        :std/iter
        :std/getopt
        :std/format
        :std/xml
        :std/net/httpd
        :std/net/request
        :std/net/address
        :std/text/json
        :std/misc/list ;; unique
        :gerbil/gambit/threads
        :gerbil/gambit/ports)

(export main)

(def (tagged? x y)
  (match x
    ([(eq? y) . _] #t)
    (else #f)))

;; utils

(def (compose2-keyed key-fn fn)
  (lambda (x y)
    (fn (key-fn x) (key-fn y))))

(def (url->xml url)
  (read-xml (request-text (http-get url))))

(def (sxml-find-elm sxml tag)
  (sxml-select sxml (lambdA (n) (eq? (sxml-e n) tag))))

;; speedtest

(def (get-servers)
  (def urls ["://www.speedtest.net/speedtest-servers-static.php"
             "http://c.speedtest.net/speedtest-servers-static.php"
             "://www.speedtest.net/speedtest-servers.php"
             "http://c.speedtest.net/speedtest-servers.php"])
  (chain (lambda (url)
           (sxml-find-elm (url->xml url)
                         'server))
    (map <> urls)
    (foldr append [] <>)
    (unique <>)))

(def (get-best-server servers)
  (def (test-server server)
    (let* ((times (for/collect (i (in-range 0 3))
                    (let* ((start (current-time))
                           (request (http-get latency-url))
                           (total (/
                                   (time-nanosecond
                                    (time-different start (current-time)))
                                   1000)))
                      (if (and (= (request-status request) 200)
                               (string=? (request-text request) "test=test"))
                        total
                        3600))))
           (sum (foldr + 0 times)))
      (cons server
            (* (/ sum 6) 1000.0))))
  (let ((milli-stamp (* 1000 (time->seconds (current-time))))
        (latency-url (string-append url "/latency.txt?x=" stamp)))
    (chain (map test-server servers)
      (sort <> (compose2-keyed cdr <))
      (car <>))))

;;

(def (get-config)
  (def +size+ '(upload: [32768 65536 131072 262144 524288 1048576 7340032]
                download: [350 500 750 1000 1500 2000 2500 3000 3500 4000]))
  (def +count+ [upload: (/ upload_max size_count)])
  (let (sxml (url->xml "://www.speedtest.net/speedtest-config.php"))
    (def (get-xml-elm-attri tag)
      (chain (sxml-find-elm sxml tag)
        (sxml-attributes <>)))
    (let ((server-config (get-xml-elm-attri 'server-config))
          (download (get-xml-elm-attri 'download))
          (upload (get-xml-elm-attri 'upload))
          (times (get-xml-elm-attri 'times))
          (client (get-xml-elm-attri 'client)))
      (map string->number
           (chain (ref server_config 'ignoreids)
             (string-split <> #\,)))
      (string->number (hash-get upload 'ratio)))))

(def (test-download server)
  "test download speed against speedtest.net"
  )

(def (main . args)
  (def gopt
    (getopt (option 'address "-a" "--address"
                    help: "server address"
                    default: "127.0.0.1:8080")
            (command 'get help: "you-get the url resource"
                   (argument 'url help: "url to you-get")
                   (flag 'info "-i" "--info" help: "get the info for the url")
                   (option 'dir "-d" "--dir" help: "set output directory"))))

  (displayln "running at 127.0.0.1:8080")
  (try
   (let (opt (getopt-parse gopt args))
     (run (hash-get opt 'address)))
   (catch (getopt-error? exn)
     (getopt-display-help exn "hellod" (current-error-port))
     (exit 1))))