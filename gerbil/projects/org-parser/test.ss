(import :std/test :std/getopt ;; unix opt
        :std/misc/repr ;; print-representation
        :gerbil/gambit/exceptions) ;; display-exception
(import :org-parser/lexer
        :org-parser/parser
        :org-parser/to-html)
        

(export main)

(def +test-string+ #<<EOF
* meat
** add salt + say sauce + white pepper + 料酒 + mix well
** egg white + mix well
** corn flour + mix well + oil + mix well

* 水超一下食蔬

* 煎土豆
  冷水
  大火烧开, 转小火煮五分钟
EOF
)


(def (lexer-test)
  (check-equal? (lexify +test-string+)
                '((heading "meat" "*" #f)
                  (heading "add salt + say sauce + white pepper + 料酒 + mix well" "**" #f)
                  (heading "egg white + mix well" "**" #f)
                  (heading "corn flour + mix well + oil + mix well" "**" #f)
                  (newline)
                  (heading "水超一下食蔬" "*" #f)
                  (newline)
                  (heading "煎土豆" "*" #f)
                  (text "  冷水")
                  (text "  大火烧开, 转小火煮五分钟"))))


(def (parse-test)
  (check-equal? (parse-org +test-string+)
                '((heading (@ (level "*") (text "meat"))
                           (heading (@ (level "**")
                                       (text "add salt + say sauce + white pepper + 料酒 + mix well")))
                           (heading (@ (level "**") (text "egg white + mix well")))
                           (heading (@ (level "**") (text "corn flour + mix well + oil + mix well"))
                                    (newline #\newline)))
                  (heading (@ (level "*") (text "水超一下食蔬"))
                           (newline))
                  (heading (@ (level "*") (text "煎土豆"))
                           (string "  冷水")
                           (string "  大火烧开, 转小火煮五分钟")))))

(def (sxml-test)
  (check-equal? (org->sxml (parse-org +test-string+))
                '((div (a (@ (href "#heading1") (data-toggle "collapse")) (h1 "* meat"))
                       (div (@ (id "heading1") (class "collapse"))
                            (a (h1 "** add salt + say sauce + white pepper + 料酒 + mix well"))
                            (a (h1 "** egg white + mix well"))
                            (div (a (@ (href "#heading0") (data-toggle "collapse"))
                                    (h1 "** corn flour + mix well + oil + mix well"))
                                 (div (@ (id "heading0") (class "collapse"))
                                      (br)))))
                  (div (a (@ (href "#heading2") (data-toggle "collapse"))
                          (h1 "* 水超一下食蔬"))
                       (div (@ (id "heading2") (class "collapse"))
                            (br)))
                  (div (a (@ (href "#heading3") (data-toggle "collapse"))
                          (h1 "* 煎土豆"))
                       (div (@ (id "heading3") (class "collapse"))
                            (p "  冷水")
                            (p "  大火烧开, 转小火煮五分钟"))))))

(def (main)
  (print-representation
   (org->sxml 
    (parse-org "~/sources/notes/cooking/bakery.org" ;; +test-string+
               )))
  (display #\newline)
  ;; (print-representation (time (lexify +test-string+)))
  ;; (display #\newline)
  (lexer-test)
  (parse-test)
  (sxml-test))

(displayln (org->sxml (parse-org +test-string+)))
;; (main)
