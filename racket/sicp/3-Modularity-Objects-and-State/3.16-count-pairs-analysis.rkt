;; Exercise 3.16 count-pairs

;; incorrect version
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

;; this is what i thought initially
(count-pairs '(a b c))                  ; 3
(count-pairs '(a b (c)))                ; 4
(count-pairs '((a (b (c ())))))         ; 7
;; but compare to the solution below, its clearly not
;; understanding the core concept of mutability

(define str1 )
 (count-pairs '(foo bar baz)) ; 3
 ; x -> ( . ) -> ( . ) -> ( .\)
 ;       |        |        |
 ;       v        v        v
 ;      'foo     'bar     'baz

 (define x '(foo))
 (define y (cons x x))
 (define str2 (list y))
 (count-pairs str2) ; 4
 ; x -> ( .\)
 ;       |
 ;       v
 ;      ( . )
 ;       | |
 ;       v v
 ;      ( .\)
 ;       |
 ;       v
 ;      'foo

 (define x '(foo))
 (define y (cons x x))
 (define str3 (cons y y))
 (count-pairs str3) ; 7
 ; str3 -> ( . )
 ;          | |
 ;          v v
 ;         ( . )
 ;          | |
 ;          v v
 ;         ( . ) -> null
 ;          |
 ;          v
 ;         'foo

 (define str4 '(foo bar baz))
 (set-cdr! (cddr str4) str4)
 (count-pairs str4) ; maximum recursion depth exceeded
 ;          ,-------------------,
 ;          |                   |
 ;          v                   |
 ; str4 -> ( . ) -> ( . ) -> ( . )
 ;          |        |        |
 ;          v        v        v
 ;         'foo     'bar     'baz