;; Ошибка
(define (error message x)
  (newline)(display "Error: ")
  (display message)(display ": ") 
  (display x)) 

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



;; ----------------------------------------------------------
; Пустой список термов
(define
  (the-empty-termlist) '())

; Вытащить первы терм
(define
  (first-term term-list) (car term-list))

; Вытащить все остальное кроме первого терма
(define
  (rest-terms term-list) (cdr term-list))

; Предикат - пустой ли список термов
(define
  (empty-termlist? term-list) (null? term-list))

; Создать терм
(define (make-term order coeff) (list order coeff))

; Вытащить степень
(define (order term) (car term))

; Вытащить коэффициент
(define (coeff term) (cadr term))

; Создать многочлен
(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms)) 


; Упражнение 2.87
(define (=zero? x)
  (if (pair? x)
      (=zero? (coeff x))
      (= x 0)))
  
;; Проверка
(=zero? 0)
(=zero? 5)
(=zero? (make-term 1 5))
(=zero? (make-term 1 0))
(=zero? (make-term 1 (make-term 1 0)))

; Добавить к списку многочленов новый многочлен
(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      ; Если коэффициент многочлена нуль то он ненужен
      term-list
      ; Иначе добаляем его в голову списка
      (cons term term-list)))

 

; (+) Сложение термов
(define (add-terms L1 L2)
  (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1)
        (else
         (let ((t1 (first-term L1)) (t2 (first-term L2)))
           (cond ((> (order t1) (order t2))
                  (adjoin-term
                   t1 (add-terms (rest-terms L1) L2)))
                 ((< (order t1) (order t2))
                  (adjoin-term
                   t2 (add-terms L1 (rest-terms L2))))
                 (else
                  (adjoin-term
                   (make-term (order t1)
                              (add (coeff t1) (coeff t2)))
                   (add-terms (rest-terms L1)
                              (rest-terms L2)))))))))


; (*) Умножение термов
(define (mul-terms L1 L2)
  (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))

(define (mul-term-by-all-terms t1 L)
  (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
         (make-term (+ (order t1) (order t2))
                    (mul (coeff t1) (coeff t2)))
         (mul-term-by-all-terms t1 (rest-terms L))))))

         
        
(define (install-polynomial-package)
  ;; внутренние процедуры
  ;; представление poly
  (define (make-poly variable term-list)
    (cons variable term-list))

  ;; Получить переменную
  (define (variable p) (car p))
  ;; Получить список термов
  (define (term-list p) (cdr p))

  ;; представление термов и списков термов
  ;; (+) Сложение многочленов
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Многочлены от разных переменных -- ADD-POLY"
               (list p1 p2))))
  ;; (*) Умножение многочленов
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Многочлены от разных переменных -- MUL-POLY"
               (list p1 p2))))
  
  ;; интерфейс к остальной системе
  ;; Определяем тег
  (define (tag p) (attach-tag 'polynomial p))
  
  ;; Прописываем в системе определенные функции
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (args) (tag (make-poly (car args) (cdr args)))))
  'done)



;; Обобщенные функции 
(define (apply-generic op x y) 
  ((get op (list (car x) (car y))) 
   (cdr x) (cdr y)))

(define (add x y) (apply-generic 'add x y))
;(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
;(define (div x y) (apply-generic 'div x y))

; Обобщенное создание
(define (make args)
  ((get 'make (car args))
   (cadr args)))
  
  ;; Теперь инсталируем пакет в систему
(install-polynomial-package)

(make (list 'polynomial (list 'x (list 10 2))))






























