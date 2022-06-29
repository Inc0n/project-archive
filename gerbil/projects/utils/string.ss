
(import :std/srfi/13) ;; string-contains

;; (def (string-slice str1 str2)
;;   (let (i (string-contains str1 str2))
;;     (values (substring str1 0 i)
;;             (substring str1
;;                        (+ i (string-length str2))
;;                        (string-length str1)))))

(def (string-slice str1 str2 (start 0))
  (let (i (string-contains str1 str2 start))
    (if i
      (values (substring str1 0 i)
              (substring str1
                         (+ i (string-length str2))
                         (string-length str1)))
      (values str1 ""))))

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