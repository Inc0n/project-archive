
(in-package :cl-user)

(defpackage regex.core
  (:use :cl)
  (:export
   :re/character
   :re/dot
   :re/question
   :re/star
   :re/group
   :re/head
   :re/tail
   :re/plus
   :re/brackets
   :re/ok
   :re/repeat
   :re/ref
   :re/or
   :match
   :match-all))
(in-package :regex.core)


(declaim (inline sar))
(defun sar (data)
  (declare (type data data))
  (when (< (data-index data) (data-length data))
    (let ((c (char (data-string data) (data-index data))))
      (if (char/= #\Newline c)
        c))))

(declaim (inline sdr))
(defun sdr (data)
  (declare (type data data))
  (incf (data-index data)))

(defmacro push-curnt-str (data &optional start)
  (labels
      ((aux (expr)
         `(push (subseq (data-string ,data)
                        ,expr (data-index ,data))
                (data-register ,data))))
    (if start
        (aux start)
        (aux `(data-start ,data)))))

(defmacro defmatcher (name args body &optional &key (progress nil))
  `(defun ,name ,args
     (lambda (data)
       (declare (type data data))
       ,(if progress
            `(let ((result ,body))
               (if result
                   (sdr data))
               result)
            `,body))))


(defmatcher re/character (c)
  (eql c (sar data))
  :progress t)

(defmatcher re/dot ()
  (sar data)
  :progress t)

(defmatcher re/repeat (n m r)
  (let ((start (data-index data)))
    (if (and (= n 0) (= m 1))
        (funcall r data)
        (loop
           :while (and
                   (< (data-index data) (length (data-string data)))
                   (funcall r data))))
    (if (>= (- (data-index data) start) n)
        t)))

(defun re/question (r)
  (re/repeat 0 1 r))

(defun re/star (r)
  (re/repeat 0 -1 r))

(defun re/plus (r)
  (re/repeat 1 -1 r))

(defmatcher re/or (r1 r2)
  (some (lambda (regs)
          (every (lambda (x) (funcall x data)) regs))
        (list r1 r2)))

(defmatcher re/head ()
  (let ((ret-val (=  (data-index data) 0)))
    (if (not ret-val)
        (throw 'match
          nil)
        ret-val)))

(defmatcher re/tail ()
  (= (data-index data) (data-length data)))

(defmatcher re/ref (n)
  (let ((str
         (nth (1- n) (reverse (data-register data)))))
    (every (lambda (x) (funcall (re/character x) data)) str)))

(defmatcher re/group (r)
  (let ((start (data-index data))
        (ret-val (every (lambda (x) (funcall x data)) r)))
    (if ret-val
        (push-curnt-str
         data
         start))
    ret-val))

(defmatcher re/brackets (regex &optional (exclude nil))
  (let ((match (position (sar data) regex)))
    (if exclude
        (not match)
        match))
  :progress t)