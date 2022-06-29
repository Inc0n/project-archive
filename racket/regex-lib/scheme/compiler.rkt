
(define (install-re-lexi)
  (define (lexi-self stream c) (re/tag c c))
  (define (lexi-repeat stream c) c)
  ;;
  (put 'compile #\$
       (lambda (_)
         (lambda (data)
           (eq? #f ((data 'next))))))
  (put 'compile #\^
       (lambda (_)
         (lambda (data)
           (= ((data 'index)) 0))))
  (put 'compile #\.
       (lambda (_)
         (lambda (data)
           ((data 'next)))))
  ;;
  (put 'compile #\* lexi-repeat)
  (put 'compile #\+ lexi-repeat)
  (put 'compile #\? lexi-repeat)
  ;;
  (put 'compile #\|
       (lambda (stream c) (re/tag c (compile stream (stream-next stream)))))
  (put 'compile 'char
       (lambda (c)
         (lambda (data)
           (char=? c ((data 'next))))))
  (put 'compile #\[
       (lambda (lst)
         (lambda (data)
           (member ((data 'next)) lst))))
  (put 'compile #\{
       (lambda (stream c) (re/tag c ((stream 'read-until) #\}))))
  (put 'compile #\(
       (lambda (stream c) (re/tag c ((stream 'read-until) #\)))))
  (put 'compile #\\
       (lambda (stream c) (re/tag c (stream-next stream))))
  ;;
  'done)