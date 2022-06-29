
#!/usr/bin/env gxi

(import :org-parser/utils)

(import :std/pregexp ;; regex
        :std/format ;; format
        :std/sugar)
(import :std/misc/func) ;; compose1
(import :std/misc/ports) ;; read-line
;; (import :std/srfi/13) ;; string-contains

(export lexify)

;; string utils

;; (def (string-slice str1 str2 (start 0))
;;   (let (i (string-contains str1 str2 start))
;;     (if i
;;       (values (substring str1 0 i)
;;               (substring str1
;;                          (+ i (string-length str2))
;;                          (string-length str1)))
;;       (values str1 ""))))

(def (string-slice str1 char (start 0))
  (let (i (string-index str1 char start))
    (if i
      (values (substring str1 0 i)
              (substring str1
                         (fx1+ i)
                         (string-length str1)))
      (values str1 ""))))

;;

(def (compose-pregex . pat)
  (def (substr string start (end (string-length string)))
    (substring string start end))
  (let (pat (pregexp (apply string-append pat)))
    (lambda (line)
      (match (pregexp-match-positions pat line)
        ([[_ . match-end] rest-pos]     ; matched X group positions
         (values (map (match <>
                        ([x . y] (substring line x y)))
                      rest-pos)
                 (substr line match-end)))
        ([[start . end]]                ; single match only
         (values (substring line start end)
                 (substr line end)))
        (#f #f)
        (x (error "pregexp-match unexpected pat for result" pat x))))))

(def status-regexp "(TODO|DONE|WAITING|PROJECT|STARTED|SOMEDAY|CANCELLED)?[ ]*")
(def text-regexp ".*\\n")

;; heading
(def starts-with-*-more? (compose-pregex "^\\*+[ ]*"
                                         status-regexp
                                         text-regexp))
(def (heading-text line)
  (substring line
             (1+ (or (string-index line #\space)
                     -1))
             (string-length line)))
(def (heading-level line)
  (substring line 0 (string-index line #\space)))

;; header
(def header? (pregexp-match-compose "^#\\+"))

;; comment
(def line-comment? (pregexp-match-compose "^[ ]*#[^ ]"))

;;
(def (text-attri-pred-compose delim)
  (let* ((pat (format "~s.*~s" delim delim))
         (re-pat (pregexp pat)))
    (lambda (line)
      ;; TODO - check for space after the match
      (pregexp-match re-pat line (string-index line #\space)))))

(def text-attri? (text-attri-pred-compose "[\\*\\+_]"))

;; text attri
(def text-attri-*? (text-attri-pred-compose "\\*"))
(def text-attri-+? (text-attri-pred-compose "\\+"))
(def text-attri-_? (text-attri-pred-compose "_"))
(def (attri-text line delim)
  (substring line
             1
             (string-index line delim 1)))

;; lexify

(def (lex-text-w-attri name line delim)
  (let ((values text line) (string-slice line delim))
    (debugln "text (" delim "): " line)
    (cons (make-elmt name text)
          (lexify-line-body line))))

(def (lex-text-line name line delim)
  (let ((values text line) (string-slice line delim))
    (debugln "line (" delim "): " line)
    (make-elmt name text)))

;; (def char->symbol (compose1 make-symbol string))
(def text-attri-delim->tag-name
  (match <>
    (#\* 'bold)
    (#\+ 'strike-through)
    (#\_ 'underline)))

(def (lexify-line-body line)
  (cond ((text-attri? line) =>
         (match <>
           ([delim] (lex-text-w-attri (text-attri-delim->tag-name delim)
                                      line delim))
           (else (error "unexpected text-attri match: " line))))
        (else (make-elmt 'string line))))

(def (lexify-line line)
  (match line
    ((? starts-with-*-more?)
     ;; (debugln "lexify heading: " line)
     (make-elmt 'heading
                `((level . ,(heading-level line))
                  (title . ,(heading-text line)))
                #f))
    ;; ((? header?) (make-elmt ))
    ((? line-comment?) (lex-text-line 'comment line #\#))
    ((? string-empty?) ;; (or "\n")
     (make-elmt 'newline #\newline))
    (else (lexify-line-body line))))

;;

(def (lexify-lines lines)
  (match lines
    ([] [])
    ([line . lines]
     (cons (lexify-line line)
           (lexify-lines lines)))))
;; (def (lexify-lines lines (acc []))
;;   (match lines
;;     ([] (reverse acc))
;;     ([line . lines]
;;      (lexify-lines lines
;;                    (cons (lexify-line line) acc)))))

(def (lexify-port port)
  (let (line (read-line port))
    (match line
      (#!eof [])
      (else (cons (lexify-line line)
                  (lexify-port port))))))

;; (def (lexify-port port (acc []))
;;   (let (line (read-line port))
;;     (match line
;;       (#!eof (reverse acc))
;;       (else (lexify-port port
;;                          (cons (lexify-line line) acc))))))

(def (lexify x)
  (match x
    ((? list?) (lexify-lines x))
    ((? input-port?) (lexify-port x))
    ((? string?)
     (if (file-exists? x)
       (if (eq? (file-type x) 'regular)
         (lexify-lines (read-file-lines x))
         (error "path does not point to a regular file " x))
       (lexify-lines (string-split x #\newline))))))