;; Exercise 2.55 double-quote-mystery
;; Eva Lu Ator types to the interpreter the expression

(car ''abracadabra)

;; To her surprise, the interpreter prints back quote . Explain

;; Answer:
;; So ' is a short-hand for (quote ...) then
;; ''abcdefg => '(quote abcdefg)
;; therefore, (car ''abcdefg) => (car '(quote abcdefg)) => 'quote

;; 2019-05-10 00:06