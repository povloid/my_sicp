(car '(a b c))

(cdr '(a b c))

'()


; Упражнение 2.53

(list 'a 'b 'c)
(list '(a) '(b) '(c))

(list (list 'george))

(cdr '((x1 x2) (y1 y2)))

(cadr '((x1 x2) (y1 y2)))

(pair? (car '(a short list)))

(memq 'red '((red shoes) (blue socks)))

(memq 'red '(red shoes blue socks))


; Упражнение 2.54
(newline)
(equal? '(this is a list) '(this is a list))
(equal? '(this is a list) '(this (is a) list))
(equal? '(this (is a) list) '(this (is a) list))


;(eq? 'ab 'ab)
;(car (cdr '(this (is a) list)))

(define (equal?-2 a b)
  (if (and (pair? a) (pair? b)) 
      (and (equal?-2 (car a) (car b))
           (equal?-2 (cdr a) (cdr b)))
      (eq? a b)))

(newline)

(equal?-2 '(this is a list) '(this is a list))
(equal?-2 '(this is a list) '(this (is a) list))
(equal?-2 '(this (is a) list) '(this (is a) list))

(newline)

; Упражнение 2.55.

(car ''abracadabra)
; потому что равносильно
(car (quote (quote abracadabra)))
; потому что равносильно
(car '(quote abracadabra))