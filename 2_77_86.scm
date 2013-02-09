(define (error message x)
  (newline)(display "Error: ")
  (display message)(display ": ") 
  (display x)) 


;;;(error "23432424" 1)

; Для того чтобы выполнить задание забежим вперед в 3 главу 
; и возьмем от туда построение таблици

;Итак общие функции работы с двумерными таблицами

(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              #f))
        #f)))

(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table)))))
  'ok)


; Теперь специфика под задачу
(define table-1 (list '*table*))

(define (get op type)
  (lookup op type table-1))

(define (put op type element)
  (insert! op type element table-1))



(define (attach-tag type-tag contents)
  (cons type-tag contents))



;Обобщенные арифметические процедуры определяются следующим образом:

(define (apply-generic op x y) 
  ((get op (list (car x) (car y))) 
   (cdr x) (cdr y)))







(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (exp2 x y) (apply-generic 'exp x y))

(define (equ? x y) (apply-generic 'equ? x y))


(define (=zero? x) 
  ;(display (list (car x)))
  ((get '=zero? (list (car x))) (cdr x)))



; Пакет установки пакета для работы с обычными числами
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  
  ; Упражнение 2.79
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  
  
  ; Упражнение 2.80
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  
  
  (define (exp x y) (apply-generic 'exp x y))
  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) (tag (expt x y))))
  
  
  
  
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done2)

(install-scheme-number-package)


(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(add
 (make-scheme-number 4)
 (make-scheme-number 5))

(equ?
 (make-scheme-number 4)
 (make-scheme-number 5))

(equ?
 (make-scheme-number 5)
 (make-scheme-number 5))

(=zero? (make-scheme-number 5))
(=zero? (make-scheme-number 0))

;  пакет, который реализует арифметику рациональных чисел. 
(define (install-rational-package)
  ;; внутренние процедуры
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  
  ; Упражнение 2.79
  (define (equ?-rat x y)
    (and (= (denom x) (denom y))
         (= (numer x) (numer y))))
  
  ; Упражнение 2.80
  (define (=zero?-rat x)
    (= (numer x) 0))
  
  
  ;; интерфейс к остальной системе
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  
  ; Упражнение 2.79
  (put 'equ? '(rational rational)
       (lambda (x y) (equ?-rat x y)))
  
  
  ; Упражнение 2.80
  (put '=zero? '(rational)
       (lambda (x) (=zero?-rat x)))
  
  
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(install-rational-package)

(define (make-rational n d)
  ((get 'make 'rational) n d))


(add (make-rational 1 2)
     (make-rational 5 10))


(sub (make-rational 1 2)
     (make-rational 5 10))

(equ? (make-rational 1 2)
      (make-rational 5 10))

(equ? (make-rational 5 10)
      (make-rational 5 10))

(equ? (make-rational 5 10)
      (make-rational 5 11))

(=zero? (make-rational 5 10))
(=zero? (make-rational 0 10))

;; пакет и для комплексных чисел


(define (install-rectangular-package)
  ;; внутренние процедуры
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; интерфейс к остальной системе
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(install-rectangular-package)

(define (install-polar-package)
  ;; внутренние процедуры
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; интерфейс к остальной системе
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(install-polar-package)



(define (install-complex-package)
  
  ;; Еще надо добавить селекторы
  (define (real-part z) (cadr z))
  (define (imag-part z) (cddr z))
  
  (define (magnitude z)  (cadr z))
  (define (angle z) (cddr z))
  
  (define (tag x)
    (attach-tag 'complex x))
  ;; интерфейс к остальной системе
  ;(define (tag z) 
  ;  (attach-tag 'complex z))
  
  ;; процедуры, импортируемые из декартова
  ;; и полярного пакетов
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; внутренние процедуры
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  
  ; Упражнение 2.79
  (define (equ?-complex z1 z2)
    (and (= (magnitude z1) (magnitude z2))
         (= (angle z1) (angle z2))))
  
  ; Упражнение 2.80
  (define (=zero?-complex z1)
    (and (= (magnitude z1) 0)
         (= (angle z1) 0)))
  
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  
  ; Упражнение 2.79
  (put 'equ? '(complex complex)
       (lambda (z1 z2) (equ?-complex z1 z2)))
  
  
  ; Упражнение 2.80
  (put '=zero? '(complex)
       (lambda (z1) (=zero?-complex z1)))
  
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  
  
  
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  
  
  
  'done3)

(install-complex-package)


(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))


(add (make-complex-from-real-imag 2 1) 
     (make-complex-from-real-imag 3 0.5))

(add (make-complex-from-mag-ang 2 1)
     (make-complex-from-mag-ang 3 0.5))


(equ? (make-complex-from-mag-ang 2 1)
      (make-complex-from-mag-ang 3 0.5))

(equ? (make-complex-from-mag-ang 2 1)
      (make-complex-from-mag-ang 2 1))

(=zero? (make-complex-from-mag-ang 2 1))
(=zero? (make-complex-from-mag-ang 0 0))

;; Упражнение 2.77 (Както непонятно, сделал свой вызов) - посути должно производиться двойное диспетчирвание
(define (magnitude2 z)
  ((get 'magnitude (list (car z))) (cdr z))
  )

(magnitude2 (make-complex-from-mag-ang 2 1))


; Упражнение 2.78

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else (error "Некорректные помеченные данные -- TYPE-TAG" datum))))


(define (attach-tag type-tag contents)
  (if  (number? contents) contents
       (cons type-tag contents)))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Некорректные помеченные данные -- CONTENTS" datum))))







; ******************************************************************************
; Приведение типов
; Теперь специфика под задачу
(define table-coercion (list '*table-coercion*))

(define (get-coercion type-1 type-2)
  (lookup type-1 type-2 table-coercion))

(define (put-coercion type-1 type-2 element)
  (insert! type-1 type-2 element table-coercion))


(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))


(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Bad tagged datum -- CONTENTS" datum))))

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))


(put-coercion 'scheme-number 'complex scheme-number->complex)

;------------------------------------------------
(define (scheme-number->scheme-number n) n)

(define (complex->complex z) z)

(put-coercion 'scheme-number 'scheme-number
              scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)
;-------------------------------------------------




(define (apply-generic op . args)  
  (display "*->")
  ;; в)
  (define (apply-generic-old op x y) 
    ((get op (list (car x) (car y))) 
     (cdr x) (cdr y)))
  
  (define (is-all-type-tags-eq? args)
    (if (null? args) #t
        (let ((a (eq? (car args) (cadr args))))
          (if a 
              (and a (is-all-type-tags-eq? (cddr args))) #f))))
  
  
  (let ((type-tags (map type-tag args)))
    (display args)
    ;; в)
    (if (is-all-type-tags-eq? type-tags) 
        ;; Если все аргументы в одинаковы то ничего приводить ненадо
        (apply (get op type-tags) (map contents args))
        ;; Иначе пробуем привести
        (let ((proc (get op type-tags)))
          (if proc 
              (apply proc (map contents args))
              (if (= (length args) 2)
                  (let ((type1 (car type-tags))
                        (type2 (cadr type-tags))
                        (a1 (car args))
                        (a2 (cadr args)))
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))                  
                      (cond (t1->t2
                             (apply-generic op (t1->t2 a1) a2))
                            (t2->t1
                             (apply-generic op a1 (t2->t1 a2)))
                            (else
                             (error "No method for these types"
                                    (list op type-tags))))))
                  (error "No method for these types"
                         (list op type-tags))))))))





(add
 (make-scheme-number 4)
 (make-scheme-number 5))


(add
 (make-complex-from-real-imag 2 1)
 (make-scheme-number 5))


; Упражнение 2.81

(newline)
(add
 (make-scheme-number 4)
 (make-scheme-number 5))

(newline)
(add
 (make-complex-from-real-imag 2 1)
 (make-complex-from-real-imag 2 1))


(exp2
 (make-scheme-number 4)
 (make-scheme-number 5))


;(exp2
; (make-complex-from-real-imag 2 1)
; (make-complex-from-real-imag 2 1))

; a)
; Работает если операции определены в таблице типов
; если не определены то происходит зацикливание

; б) Особого смысла в доработке нет


; в) Процедура доработана выше

; Упражнение 2.82


(newline)

(define (apply-generic op args)
  ;(display "*->")  
  ;(display args)
  
  ; Новая обобщенная процедура, принимает на вход список аргументов
  (define (make-op op2 args)
    ;(display "/>")  
    ;(display args)
    (let ((x (car args)) (y (cadr args)))
      (let ((opp (get op2 (map type-tag (list x y))))) 
        (if (null? (cddr args)) 
            (opp (contents x) (contents y))
            (make-op op2 (cons (opp (contents x) (contents y)) (cddr args)))))))               
  
  ; Процедура проверяющая все ли типы в списке одинаковы
  (define (is-all-type-tags-eq? args2)
    (if (null? (cdr args2)) #t
        (let ((a (eq? (car args2) (cadr args2))))
          (if a 
              (and a (is-all-type-tags-eq? (cdr args2))) 
              #f))))
  
  ; Процедура приведение первых двух аргументов
  (define (type1->type2 a1 a2)
    (let ((type1 (type-tag a1))
          (type2 (type-tag a2)))
    (let ((t2->t1 (get-coercion type2 type1)))   
      (cond (t2->t1
             (t2->t1 a2))
            (else
             a2)))))
  
  
  (let ((type-tags (map type-tag args)))
    ;; в)
    (if (is-all-type-tags-eq? type-tags) 
        ;; Если все аргументы в одинаковы то ничего приводить ненадо
        (make-op op args)
        
        ;; Иначе пробуем привести по первому элементу, ставим его в конец и вызываем снова
        (apply-generic op
                       (append (map (lambda (x) (type1->type2 (car args) x)) (cdr args)) 
                               (list (car args))))
        )))



(define (add . args) 
  (apply-generic 'add args))


(add
 (make-complex-from-real-imag 2 1)
 (make-complex-from-real-imag 2 1)
 (make-complex-from-real-imag 2 1))

(add
 
 (make-scheme-number 5)
 (make-scheme-number 5)
 (make-scheme-number 5)
 (make-scheme-number 5)
 (make-scheme-number 5)
 (make-scheme-number 5)
 (make-scheme-number 5)
 (make-scheme-number 5))


(add
 
 (make-scheme-number 5)
 (make-scheme-number 5)
 (make-scheme-number 5)
 (make-scheme-number 5)
 (make-complex-from-real-imag 2 1)
 (make-scheme-number 5)
 (make-scheme-number 5)
 (make-scheme-number 5)
 (make-scheme-number 5))










