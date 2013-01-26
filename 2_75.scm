(define (square x) (* x x))



(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
           (error "Неизвестная оп. -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)


(define (apply-generic op arg) (arg op))


(define a (make-from-real-imag 0.5 0.6))



(apply-generic 'real-part a)
(apply-generic 'imag-part a)
(apply-generic 'magnitude a)
(apply-generic 'angle a)


(define (make-from-magnitude-angle magnitude angle)
  (define (dispatch op)
    (cond ((eq? op 'real-part) 
           (* magnitude (cos angle)))
          ((eq? op 'imag-part) 
           (* magnitude (sin angle)))
          ((eq? op 'magnitude) magnitude)
          ((eq? op 'angle) angle)
          (else
           (error "Неизвестная оп. -- make-from-magnitude-angle" op))))
  dispatch)

(define b (make-from-magnitude-angle 0.7810249675906654 0.8760580505981934))

(newline)
(apply-generic 'real-part b)
(apply-generic 'imag-part b)
(apply-generic 'magnitude b)
(apply-generic 'angle b)


;Упражнение 2.76.
;Когда большая система с обобщенными операциями развивается, могут потребоваться новые типы
;объектов данных или новые операции. Для каждой из трех стратегий — обобщенные операции с
;явной диспетчеризацией, стиль, управляемый данными, и передача сообщений, – опишите, какие
;изменения нужно произвести в системе, чтобы добавить новый тип или новую операцию. Какая
;организация лучше подходит для системы, в которую часто добавляются новые типы? Какая для
;системы, где часто появляются новые операции?


;1. Обобщенные операции с явной диспетчеризацией.
; Нужно перерабатывать обобщенные процедуры

;2. Cтиль, управляемый данными
; Сделать только новый пакет с процедурами для конкретного типа и доьавить новые данные в таблицу операций

;3. И передача сообщений
; Добавление нового типа как такового. А придобавлении новой операции модифицировывать все типы ( конструкторы)


; Более мение удобными будут последние 2 типа реализации хотя они посути равнозначны, мне лично больше наравится управление данными









