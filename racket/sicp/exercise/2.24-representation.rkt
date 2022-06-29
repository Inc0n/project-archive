#| visual presentation of '(1 (2 (3 4)))

   +---+---+  +---+---+
   | * | *-+->| * | / |
   +-+-+---+  +-+-+---+
     |          |
     V          V
   +---+      +---+---+  +---+---+
   | 1 |      | * | *-+->| * | / |
   +---+      +-+-+---+  +---+---+
                |          |
                V          V
              +---+      +---+---+  +---+---+
              | 2 |      | * | *-+->| * | / |
              +---+      +-+-+---+  +-+-+---+
                           |          |
                           V          V
                         +---+      +---+
                         | 3 |      | 4 |
                         +---+      +---+
|#

(define (box-plot x)
  (define (map-times f n initial-val)
    (define (iter n acc)
      (if (< n 1)
          acc
          (iter (- n 1) (f acc))))
    (iter n initial-val))
  (define str (format "~a" x))
  (define len (string-length str))
  (define delim (map-times (lambda (acc) (string-append acc "-"))
                           len ""))
  (list
   "+---+---+"
   "| * | "
   "+-|-+---+"
   "  v      "
   (format "+-~a-+" delim)
   (format "| ~a |" str)
   (format "+-~a-+" delim)))

(define (present lst)
  (define (map-1 op lst)
    (if (null? (car lst))
        '()
        (cons (op (map car lst))
              (map-1 op
                     (map cdr lst)))))
 (define arrow
   (list
    "  " "* -->" "  " "  " "  " "  " "  "))
 (define end
   (list
    "\n" "\\ |\n" "\n" "\n" "\n""\n""\n"))
 (define (join-box x end acc)
   (cons end
         (cons (box-plot x) acc)))
 (define (iter lst acc)
   (cond ((null? (cdr lst))
          (reverse (join-box (car lst) end acc)))
         (else
          (iter (cdr lst)
                  (join-box (car lst)
                            arrow acc)))))
  (define (join x)
    (foldr (lambda (x acc) (string-append x acc))
           ""
           x))
  (printf "~a~%" (join (map-1 join (iter lst '())))))
