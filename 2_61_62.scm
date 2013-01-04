
(define set1 (list 1 2 3 4 5 6 7))
(define set2 (list 4 5 6 7 8 9 10))



(define (с x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))


(element-of-set? 11 set1)
(element-of-set? 1 set1)
(element-of-set? 2 set1)




(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))



(intersection-set set1 set2)

; Сделал с поддержкой уникальности и упорядоченностиы
(define (adjoin-set x set)
    (cond ((null? set) (cons x set))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))


(adjoin-set 0 set1)
(adjoin-set 4 set1)
(adjoin-set 8 set1)
        

(define set3 (list 1 3 5 7 9 11 14))

(adjoin-set 4 set3)


; упражнение 2.61 
; Так как механизм работы посути такой же как element-of-set?
; то справедливо сказать что:
; среднее
; число требуемых шагов будет примерно n/2. Это все еще рост порядка Θ(n), но это эко-
; номит нам в среднем половину числа шагов по сравнению с предыдущей реализацией.






