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

; проверка 
(put 1 1 1)
(put 2 2 2)

(get 1 1)
(get 2 2)

; Итак первый вариант функции дифферинцирования

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
(define (deriv-1 exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv-1 (addend exp) var)
                   (deriv-1 (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv-1 (multiplicand exp) var))
          (make-product (deriv-1 (multiplier exp) var)
                        (multiplicand exp))))
        ;;Здесь можно добавить еще правила
        (else (error "неизвестный тип выражения -- DERIV" exp))))

(deriv-1 '(+ (* 2 x) (+ x 1)) 'x)



; Теперь проработка основного варианта по заданию
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) exp                                        
                                           var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

; a) Потому что операции предикаты number? и variable?? они работают с унар
; ными операциями.



; б) 
(put 'deriv '+
     (lambda (exp var)
       (make-sum (deriv (addend exp) var)
                 (deriv (augend exp) var))))


(put 'deriv '* 
     (lambda (exp var)
       (make-sum
        (make-product (multiplier exp)
                      (deriv (multiplicand exp) var))
        (make-product (deriv (multiplier exp) var)
                      (multiplicand exp)))))


(get 'deriv '+)
(deriv '(* 2 x) 'x)
(deriv '(+ (* 2 x) (+ x 1)) 'x)


; в)
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


(put 'deriv '** 
     (lambda (exp var) 
       (make-exponentiation (base exp)
                            (exponent exp))))


(newline)
(deriv '(** x 10) 'x)
(deriv '(+ (** x y) (* x 5)) 'x)


;г. В этой простой алгебраической системе тип выражения — это алгебраическая операция верх-
;него уровня. Допустим, однако, что мы индексируем процедуры противоположным образом, так
;что строка диспетчеризации в deriv выглядит как
;((get (operator exp) ’deriv) (operands exp) var)
;Какие изменения потребуются в системе дифференцирования?

; Надо поменять местами операнды 'deriv (operator exp) 
; (else ((get (operator exp) 'deriv) exp                                        
;                                           var))))












































