
(import :std/net/request)
(import :you-get/utils)
(import :gerbil/expander)

(export def-you-get you-get
        make-module default-log-fn)

(def *module-table* (make-hash-table test: string=?))

(def (media-type-ext type)
  (def +mapping+
    '(("video/3gpp" . "3gp")
      ("video/f4v" . "flv")
      ("video/mp4" . "mp4")
      ("video/MP2T" . "ts")
      ("video/quicktime" . "mov")
      ("video/webm" . "webm")
      ("video/x-flv" . "flv")
      ("video/x-ms-asf" . "asf")
      ("audio/mp4" . "mp4")
      ("audio/mpeg" . "mp3")
      ("audio/wav" . "wav")
      ("audio/x-wav" . "wav")
      ("audio/wave" . "wav")
      ("image/jpeg" . "jpg")
      ("image/png" . "png")
      ("image/gif" . "gif")
      ("application/pdf" . "pdf")))
  (alet ((type-ext (assoc type +mapping+ string=?)))
    (cdr type-ext)))

(def (default-log-fn url)
  (def (is-header-filed? headers key val)
    (alet ((field (assoc key headers)))
      (string=? (cdr field) val)))
  (displayln "url-info: ~s" url)
  (let* ((r (http-head url))
         (headers (request-headers r))
         (type (assoc "content-type" headers)))
    ;; (if (string= type "image/jpg")
    ;;     (setf type "audio/mpeg"))
    (if type
      (values
       type
       (media-type-ext (cdr type))
       ;; size
       (and (is-header-filed? headers "transfer-encoding" "chunked")
            (and-let* ((len (assoc "content-length" headers)))
                      (string->number len))))
      
      (error "unable to get url-info for ~a" url))))

(defstruct module
  (getter
   logger))

(def +empty-module+
  (let ((fn (lambda x
              (displayln "no modules matched, pass args: " x)
              #f)))
    (make-module fn fn)))
;; TODO: mod-path is relative atm, change it to relative to ~/
;;       perhaps ~/.you-get/mods
(def +default-mod-path+ "mods/")

(def (def-you-get key down-fn (log-fn default-log-fn))
  (hash-put! *module-table* key
             (make-module down-fn log-fn)))


;; (define-syntax def-you-get
;;   (syntax-rules ()
;;     ((def-you-get ?key ?down-fn)
;;      (hash-put! *module-table* ?key
;;                 (make-module ?down-fn default-log-fn)))
;;     ((def-you-get ?key ?down-fn ?log-fn)
;;      (hash-put! *module-table* ?key
;;                 (make-module ?down-fn ?log-fn)))))

(def +load-path+ (or (getenv "GERBIL_PATH" #f)
                     "~/.gerbil"))

;; considering using .ss scripts as modules
(def (get-module! domain)
  (def (domain-key->mod-name domain)
    (substring domain 0 (string-index domain #\.)))
  (let ((name (domain-key->mod-name domain)))
    (display (string-append "loading module for " +default-mod-path+ name))
    (load-module "")
    (eval-module (string-append "./" +default-mod-path+ name ".ss"))
    (displayln " done")
    (hash-ref *module-table* domain +empty-module+)))

(def (you-get url info-only (output-dir "."))
  (let* ((domain (url-domain url))
         (module (get-module! domain)))
    (displayln "matched " url " to " domain " " (eq? module +empty-module+))
    ;; (hash-for-each (lambda (k v)
    ;;                  (displayln k ", " v))
    ;;                *module-table*)
    (if info-only
      ((module-logger module) url)
      ((module-getter module) url output-dir))))
