; Переменные — это символы. Они распознаются элементарным предикатом symbol?

(symbol? 1)
(symbol? 'a)

(define (variable? x) (symbol? x))

;Две переменные одинаковы, если для представляющих их символов выполняется eq?
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))


;Суммы и произведения конструируются как списки:
;(define (make-sum a1 a2) (list '+ a1 a2))

(define (=number? exp num)
  (and (number? exp) (= exp num)))


(define (make-sum a1 a2)
  
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))


;(define (make-product m1 m2) (list '* m1 m2))


(define (make-product m1 m2)
  (display m1)(display " -- ")(display m2)(newline)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))


;Сумма — это список, первый элемент которого символ + --------------------------------
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))


;Первое слагаемое — это второй элемент списка, представляющего сумму
(define (addend s) (cadr s))

;Второе слагаемое — это третий элемент списка, представляющего сумму
;(define (augend s) (caddr s))

; Упражнение 2.57.
(define (augend s) 
  ;(display (cdddr p))(display "---")
  (if (null? (cdr (cdr (cdr s)))) ; Если третий элемент пустой?
    (caddr s)
    (make-product (multiplier (cdr s)) (multiplicand (cdr s)))))


;-------------------------------------------------------------------------------------
;Произведение — это список, первый элемент которого символ * -------------------------
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

;Первый множитель — это второй элемент списка, представляющего произведение
(define (multiplier p) (cadr p))

;Второй множитель — это третий элемент списка, представляющего произведение
;;(define (multiplicand p) (caddr p))

; Упражнение 2.57.
(define (multiplicand p)
  ;(display (cdddr p))(display "---")
  (if (null? (cdr (cdr (cdr p))))  ; Если третий элемент пустой?
    (caddr p)
    (make-product (multiplier (cdr p)) (multiplicand (cdr p)))))

;-------------------------------------------------------------------------------------

; Упражнение 2.56.
; Возведение в степень ---------------------------------------------------------------
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

; Основание 
(define (base p)
  (cadr p))

; Степень
(define (exponent p) 
  (caddr p))

(define (make-exponentiation m1 m2)
  (cond ((=number? m2 0) 1)
        ((=number? m2 1) m1)
        ((number? m2) (make-product m2 (list '** m1 (- m2 1))))
        (else (make-product m2 (list '** m1 (list '- m2 1))))))

; Процедура дифференцирования
(define (deriv exp var)
  (cond ((number? exp) 0) 
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ;Упражнение 2.56. ----------------------v
        ((exponentiation? exp) 
         (make-exponentiation (base exp)
                              (exponent exp)))
        ;Упражнение 2.56. ----------------------^
        (else
         (error "неизвестный тип выражения -- DERIV" exp))))



(deriv '(+ x 3) 'x)
(newline)
(deriv '(* x y) 'x)
(newline)
(deriv '(* (* x y) (+ x 3)) 'x)

;Упражнение 2.56.
(newline)
(deriv '(** x 10) 'x)
(deriv '(+ (** x y) (* x 5)) 'x)

;Упражнение 2.57.
(newline)
(deriv '(* (* x y) (+ x 3)) 'x)
(deriv '(* x y (+ x 3)) 'x)





;Упражнение 2.58.

(deriv '(+ (* 3 x) (* y x)) 'x)












