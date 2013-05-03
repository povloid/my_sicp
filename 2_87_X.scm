;; Печать n раз
               
(define (print-c-n c n)
  (newline)
  (define (cr-list ct nt)
    (display c)
    (if (= nt 1)
        '()
        (cons ct (cr-list ct (- nt 1)))))
  (cr-list c n)
  (newline)(newline))

(define (pline) (print-c-n '- 20))

(pline)

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


; Функция определяет максимальный элемент в списке
(define (max-order-element element tlist)
    (if (null? tlist)
        element
        (max-order-element
         (if (> element (car tlist))
             element (car tlist))
         (cdr tlist))))

(max-order-element 10 (list 10 20 40 20 400 8000 20))
(newline)



;; сортировка пузырьковая типовая обобщенная
(define (sort-1 list1 f fmax fmin)
  (define (max x y)
    (if (fmax x y) x y))
  (define (min x y)
    (if (fmin x y) x y))
  (define (test x y)
    (f x y))
  (define (split-list tst flist e elist)
    (cond ((null? elist) (list tst (append flist (list e))))
          (else (split-list (and tst (test e (car elist)))
                            (append flist (list (max e (car elist))))
                            (min e (car elist))
                            (cdr elist)))))
  (let ((a (split-list #t '() (car list1) (cdr list1))))
    (if (car a)
        (cadr a)
        (sort-1 (cadr a) f fmax fmin))))
             
          
      
      




(display '<)
(sort-1 (list 10 20 30 10 40 60 10 20) 
        (lambda (x y)(>= x y)) > <)

;(sort-1 (list 10 20 40 20 400 8000 20))
(display '>)





(pline)
(define terms1 (list (make-term 200 5) (make-term 4 5) (make-term 10 3) (make-term 100 5)))

(display terms1)


;; Упражнение 2.89
(define (sort-terms terms) 
  (sort-1 terms 
          (lambda (x y) (>= (order x) (order y)))
          (lambda (x y) (> (order x) (order y)))
          (lambda (x y) (< (order x) (order y)))))


(define terms2 (sort-terms terms1))



(newline)

(display terms2)



(pline)

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
                    (* (coeff t1) (coeff t2)))
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
  
  
  (define (same-variable? v1 v2)
    (eq? v1 v2))

  ;; представление термов и списков термов
  ;; (+) Сложение многочленов
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Многочлены от разных переменных -- ADD-POLY"
               (list p1 p2))))
  
  
  ;; (-) Вычитание многочленов (Задание 2.ы88)
  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms 
                    ;; По заданию 2.88 меняем знак коэффициентов в одном из многочленов
                    (map (lambda (x) 
                           (make-term (order x) (- (coeff x))))
                     (term-list p1))
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
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (args) (tag (make-poly (car args) (cdr args)))))
  'done)



;; Обобщенные функции 
(define (apply-generic op x y) 
  ;(display (cdr x))
  ((get op (list (car x) (car y))) 
   (cdr x) (cdr y)))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
;(define (div x y) (apply-generic 'div x y))

; Обобщенное создание
(define (make args)
  ((get 'make (car args))
   (cadr args)))
  
  ;; Теперь инсталируем пакет в систему
(install-polynomial-package)

(define p1 (make (list 'polynomial (list 'x (list 10 2)))))

(define p2 (make (list 'polynomial (list 'x (list 5 4)))))

(pline)

(add p1 p2)

(pline)

(sub p1 p2)

(pline)

(mul p1 p2)


(pline)


;; Цикл FOR
(define (for-i-n i n state f)
  (if (> i n) state
      (for-i-n (+ i 1) n (f i state) f)))
              
(for-i-n 0 1000 1
         (lambda (i state)
                (display i)
                (newline)
                state))
        
(pline)

;; Цикл while
(define (while ftest state f)
  (if (ftest state)
      (while ftest (f state) f)
      state))


(while (lambda (a) (< a 1000))
       0
       (lambda (a)
         (display a)
         (newline)
         (+ a 5))
 )
   
; Цикл do while
(define (do-while ftest state f)
      (while ftest (f state) f))



(do-while (lambda (a) (< a 2))
       0
       (lambda (a)
         (display a)
         (newline)
         (+ a 5))
 )
   
; FOR LIST

(define (for-list iflist state f)
  (define (for-iter blist elist state)
    (if (null? elist) (cons blist state)
        (let ((e (f (car elist) state)))
        (for-iter (cons (car e) blist) (cdr elist) (cdr e)))))
  (for-iter '() iflist state)) 
  



(for-list (list 0 1 2 3 4 5 5 6 7 8 9 10 5 10)
          0
          (lambda (e state)
            (cons (* e 2) (+ state (if (= e 5) 1 0))))
          )
          

;; Вытащить элемент по ключу и применить к нему функцию, 
;; затем применить функцию к значению и записать на место значения 
;; результат функции
(define (app-f-for-key maplist key f)
  (map (lambda (e)
         (if (eq? (car e) key)
             (cons key (f (cdr e)))
             e))
       maplist))


;; Применить список функций по ключу
(define (app-for-map maplist applist)
  (if (null? applist)
      maplist
      (let ((fx (car applist)))
        (app-for-map
         (app-f-for-key maplist (car fx) (cdr fx))
         (cdr applist)))))

(pline)


;; test
(app-for-map
 (list (cons 'orders-sum 0)
       (cons 'max-order 0)
       (cons 'min-order 0))
 
 (list 
  (cons 'orders-sum 
        (lambda (val)
          (+ val 1000))))
 )
(pline)





(for-list terms2 ;; Список термов
          ;; Теперь опишим состояние
          (list (cons 'orders-sum 0)
                (cons 'max-order 0)
                (cons 'min-order 100000))
          (lambda (e state)
            (cons e 
                  (app-for-map state 
                               (list 
                                (cons 'orders-sum (lambda (x)(+ x (order e))))
                                (cons 'max-order (lambda (x)(if (< x (order e)) (order e) x)))
                                (cons 'min-order (lambda (x)(if (> x (order e)) (order e) x)))
                                )
                               )
                  )
            )
          )


(define (div-terms L1 L2)
  (if (empty-termlist? L1) ;; Если числитель равен нулю тогда и знаминатель тоже равен нулю
      ;; Возвращаем пустую дробь
      (list (the-empty-termlist) (the-empty-termlist))
      ;; Иначе производим вычисления
      (let ((t1 (first-term L1)) ;; Вытаскиваем 
            (t2 (first-term L2)))
        (if (> (order t2) (order t1))
            (list (the-empty-termlist) L1)
            (let ((new-c (div (coeff t1) (coeff t2)))
                  (new-o (- (order t1) (order t2))))
              (let ((rest-of-result
                     рекурсивно вычислить оставшуюся
                     часть результата
                     ))
                сформировать окончательный результат
                ))))))

























