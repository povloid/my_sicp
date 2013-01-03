; а)
; Переменные — это символы. Они распознаются элементарным предикатом symbol?

(define (variable? x) (symbol? x))


;Две переменные одинаковы, если для представляющих их символов выполняется eq?
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))



(define (=number? exp num)
  (and (number? exp) (= exp num)))

;Суммы и произведения конструируются как списки:
(define (make-sum a1 a2) (list a1 '+ a2))


(define (make-product m1 m2) (list m1 '* m2))



;Сумма — это список, первый элемент которого символ + --------------------------------
(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

;Первое слагаемое — это второй элемент списка, представляющего сумму
(define (addend s) (car s))

;Второе слагаемое — это третий элемент списка, представляющего сумму
(define (augend s) (caddr s))


;-------------------------------------------------------------------------------------
;Произведение — это список, первый элемент которого символ * -------------------------
(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

;Первый множитель — это второй элемент списка, представляющего произведение
(define (multiplier p) (car p))

;Второй множитель — это третий элемент списка, представляющего произведение
(define (multiplicand p) (caddr p))


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
        (else
         (error "неизвестный тип выражения -- DERIV" exp))))


(deriv '(x + (3 * x) + (y * x)) 'x)

; б) Для данного варианта необходимо вводить разбиени выражений на термы и приритет операций. 

