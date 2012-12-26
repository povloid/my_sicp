;Задание 2.21
(define (square x) (* x x))


(define (square-list items)
  (if (null? items)
      (list)
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list-2 items)
  (map square items ))


(square-list (list 1 2 3 4))
(square-list-2 (list 1 2 3 4))


; Задание 2.22

(define (square-list-3 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items (list)))

; В рекурсивном варианте оболдуй Хьюго строит результат
; добавляя очередной вычесленный элемент в голову списка
; таким образои он строит результат с конца

(define (square-list-4 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items (list)))


; Во втором варианте он ставит тело списка в голову а результат в хвост
; тем самым накопление производится в голове спискаы


; Задание 2.23
(newline)

(define (for-each-2 f l)
  (f (car l))
  (if (null? (cdr l)) #t
      (for-each f (cdr l))))


(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))

(for-each-2 (lambda (x) (newline) (display x))
            (list 57 321 88))
