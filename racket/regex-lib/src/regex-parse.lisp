
(in-package :cl-user)

(defpackage regex.parse
  (:use :cl)
  (:export
   :parse))
(in-package :regex.parse)


(defun char? (c)
  (eq (type-of c) 'standard-char))
(defun null? (x) (null x))

(defun make-stream (string)
  (let ((index 0)
        (length (length string))
        (stack '()))
    (lambda (m)
      (case m
        (read-char
         (lambda ()
           (let ((char (char string index)))
             (if (= index length)
                 nil
                 (setq index (+ index 1)))
             char)))
        (push-char
         (lambda (c)
           (if (char? c)
               (setq stack (cons c stack))
               (error nil "argument c must be of type character: PUSH-CHAR" c))))
        (stack
         (if (null? stack)
             nil
             (coerce (reverse stack) 'string)))
        (t (error nil "unknown message: STREAM" m))))))

(defun test-allowed (item allowed-list)
  (if (consp item)
      (if (not (position (car item) allowed-list :test #'eq))
          (error "Unallowed procedure encounter ~a" item))
      (error "item: ~a passed into TEST-ALLOWED must be a list" item)))

(defmacro def-regex (name args rest-op regex-op acc-op)
  (if (not (intersection '(rest acc) args))
      (error nil (format nil "need a 'parser arg in 2nd arg list when DEF-REGEX ~a" name)))
  (test-allowed acc-op '(re/skip re/cdr))
  (test-allowed rest-op '(re/skip re/read-next re/read-pair))
  `(defun ,name ,args
     (list (,@rest-op rest)
           (cons ',regex-op
                 (,@acc-op acc)))))

(defun re/cons (rest-lst regex acc)
  (list rest-lst (cons regex acc)))

(defun re/car (lst) (car lst))
(defun re/cdr (lst) (cdr lst))
(defun re/skip (x) x)
(defun re/read-next (lst) (parse-1 lst '()))
(defun re/read-pair (start-char end-char lst) lst)

(def-regex parse/dot (rest acc)
  ;; (re/cons (re/skip rest)
  ;;          (re/dot)
  ;;          (re/skip acc))
  (re/skip)
  (re/dot)
  (re/skip))
(def-regex parse/char (rest acc c)
  ;; (re/cons (re/skip rest)
  ;;           (re/char c)
  ;;           (re/skip acc))
  (re/skip)
  (re/char c)
  (re/skip))

;; repeat
(def-regex parse/star (rest acc)
  ;; (re/cons (re/skip rest)
  ;;           (re/star (re/car acc))
  ;;           (re/cdr acc))
  (re/skip)
  (re/star (re/car acc))
  (re/cdr))

(def-regex parse/plus (rest acc)
  (re/skip)
  (re/plus (re/car acc))
  (re/cdr))

(def-regex parse/question (rest acc)
  (re/skip)
  (re/question (re/car acc))
  (re/cdr))

;; repeat & block
(def-regex parse/curly-brak (rest acc)
  (re/read-pair rest #\{ #\})
  (re/curly-brak pair (re/car acc))
  (re/cdr))

;; block
(def-regex parse/bracket (rest acc)
  ;; (re/cons)
  (re/read-pair #\[ #\])
  (re/bracket ans)
  (re/skip))
(def-regex parse/group (rest acc)
  ;; (re/cons)
  (re/read-pair #\( #\))
  (re/bracket ans)
  (re/skip))

(def-regex parse/or (rest acc)
  (re/read-next)
  (re/or ans (re/car acc))
  (re/cdr))

(defun parse/next (rest acc)
  (destructuring-bind (rest c) (re/read-next rest)
    (parse/char rest acc c)))

(def-regex parse/tail (rest acc)
  (re/skip)
  (re/tail)
  (re/skip))

(def-regex parse/head (rest acc)
  (re/skip)
  (re/head)
  (re/skip))

;; parse helper function


(defun parse (str)
  (labels
      ((aux (lst)
         (cond ((not (consp lst))
                (error nil "lst must be a list for PARSE"))
               ((null (car lst))
                (cadr lst))
               (t
                (let ((lst (car lst))
                      (acc (cadr lst)))
                  (aux (parse-1 lst acc)))))))
    (aux (list (coerce str 'list)
               '()))
    ;; (parse-1 (coerce str 'list) '())
    ))