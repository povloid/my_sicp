;Упражнение 2.60


(define set1 (list 2 3 2 1 3 2 2))
(define set2 (list 3 4 5 6 7 8 9 10 1))


; Оставляем прежний
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))



(element-of-set? 11 set1)
(element-of-set? 1 set1)
(element-of-set? 2 set1)


; Здесь убираем проверку
(define (adjoin-set x set)
  (cons x set))


(adjoin-set 12 set1)

(adjoin-set 2 set1)


; В первом случае можно оставить так как есть
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))


; Хотел попробовать через мап. Идея не плохая. только надо акумулировать и фильтровать
; как в прошлых упражнениях
(define (intersection-set-2 set1 set2)
  (let ((all-list (append set1 set2)))
    (map 
     (lambda (x) (if (and (element-of-set? x set1) (element-of-set? x set2))
                     x 'del))
     all-list)))

; Более правильный вариант как мне кажется
(define (intersection-set-2 set1 set2)
  (let ((all-list (append set1 set2)))
    (define (intersection-set-t list)
      (if (null? list) 
          '()
          (let ((fe (car list)))
            (if (and (element-of-set? fe set1) (element-of-set? fe set2))
                (cons fe (intersection-set-t (cdr list)))
                (intersection-set-t (cdr list))))))
    (intersection-set-t all-list)))




(cons 1 (cons 1 (cons 1 '())))

(newline)
(intersection-set set1 set2)
(intersection-set-2 set1 set2)

; Старый вариант но плох тем что удаляет дубликаты
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (union-set (cdr set1) (cons (car set1) set2)))))

; Тут все просто, просто разрешаем дуюликаты
(define (union-set-2 set1 set2)
  (append set1 set2))



(union-set set1 set2)         
(union-set '() '())    

(union-set-2 set1 set2) 





