

(def (list-line-gen lines)
  (lambda ()
    (let lp ((lines lines))
      (when (not (null? lines))
        (yield (car lines))
        (lp (cdr lines))))))

(def (port-line-gen port)
  (lambda ()
    (let lp ()
      (let (line (read-line port))
        (when (not (eq? line #!eof))
          (yield line)
          (lp))))))

(def (generator x)
  (match x
    ((? list?) (list-line-gen x))
    ((? input-port?) (port-line-gen x))
    ((? string?)
     (if (file-exists? x)
       (if (eq? (file-type x) 'regular)
         (list-line-gen (read-file-lines x))
         (error "path does not point to a regular file " x))
       (list-line-gen (string-split x #\newline))))))

;; (import :std/iter)
;; (def (lexify x)
;;   (for/collect (line (generator x))
;;     (lexify-line line)))