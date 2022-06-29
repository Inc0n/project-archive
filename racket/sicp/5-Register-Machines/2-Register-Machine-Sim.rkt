
;; design
(make-machine ⟨register-names⟩ ⟨operations⟩ ⟨controller⟩)

(set-register-contents! ⟨machine-model⟩
                        ⟨register-name⟩
                        ⟨value⟩)

(get-register-contents ⟨machine-model⟩
                       ⟨register-name⟩)

(start ⟨machine-model⟩)

(define gcd-machine
  (make-machine
   '(a b t)
   (list (list 'rem remainder) (list '= =))
   '(test-b (test (op =) (reg b) (const 0))
            (branch (label gcd-done))
            (assign t (op rem) (reg a) (reg b))
            (assign a (reg b))
            (assign b (reg t))
            (goto (label test-b))
            gcd-done)))

(define (make-machine register-names operations controller)
  (define registers
    (map (lambda (x) (cons x 'nil)) register-names))
  (define (set-register-contents! register-name value)
    )
  (define (get-register-contents register-name)
    (let ((x (assoc register-name registers)))
      (if x
          (cdr x)
          (begin
            (printf "undefined register ~a~%" register-name)
            x))))
  (define (dispatch msg)
    (case msg
      ('set-register-contents! set-register-contents!)
      ('get-register-contents get-register-contents)
      (else (error "Undefined operator" msg))))
  dispatch)

(define (set-register-contents! machine register-name value)
  ((machine 'set-register-contents!) register-name value))

(define (get-register-contents machine register-name)
  ((machine 'get-register-contents) register-name))