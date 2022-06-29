
;; (import :std/generic)
(import :std/sugar)
(import :std/misc/number) ;; increment! decrement!

(export
  str-stream str-stream?
  make-str-stream)
  ;; peek-chr
  

(defclass str-stream
  (index;; index slot for stream
   len  ;; slot cached string(sequence) len
   seq) ;; slot holder for sequence, string only right now
  constructor: :init!
  final: #t)

;; (def (new-str-stream str)
;;   (match str
;;     ((? string?) 
;;      (str-stream index: 0 len: (string-length str) seq: str))
;;     (else (error "make-str-stream: str seq passed in must be of type string"))))

(defmethod {:init! str-stream}
  (lambda (self str)
    (match str
      ((? string?) 
       (@-set! self index 0)
       (@-set! self len (string-length str))
       (@-set! self seq str))
      (else (error "make-str-stream: str seq passed in must be of type string")))))

(defmethod {empty? str-stream}
  (lambda (self)
    (with ((str-stream index: index len: len seq: seq) self)
      (>= index len))))

(defmethod {read-until str-stream}
  (lambda (self delim consume)
    "reads the stream until delim, optionally `consumes' the `delim'"
    (let aux ((start (@ self index)))
      (let (x {next-chr self})
        (match x
          (#f {sub-seq self start})
          ((? (is delim test: char=?))
           (unless consume
             {back-chr self delim})
           {sub-seq self start})
          (else
           (aux start)))))))

(defmethod {back-chr str-stream}
  (lambda (self _chr)
    (decrement! (@ self index))))


(defmethod {current-chr str-stream}
  (lambda (self)
    (with ((str-stream index: index len: len seq: seq) self)
      (string-ref seq index))))

(defmethod {next-chr str-stream}
  (lambda (self)
    (if {empty? self}
      #f
      (let (x {current-chr self})
        (increment! (@ self index))
        x))))

(defmethod {peek-chr str-stream}
  (lambda (self)
    (and (not {empty? self})
         {current-chr self})))

(defmethod {sub-seq str-stream}
  (lambda (self start (end #f))
    (substring (@ self seq)
               start
               (or end
                   (@ self index)))))

(defmethod {debug str-stream}
  (lambda (self)
    (with ((str-stream index: index len: len seq: seq) self)
      (displayln "stats: " index " len: " len)
      (displayln "seq: " seq))))

(def (test)
  (let (stm (str-stream "test 1tring"))
    (displayln {next-chr stm})
    (displayln {read-until stm #\space})
    (displayln {next-chr stm})))

(def (main . args)
  (displayln args)
  (test))

;; (defmethod (is-empty (self <stream>))
;;   (null (peek-char nil self nil nil)))