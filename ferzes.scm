(define (accumulate op initial sequence1)
  (if (null? sequence1)
      initial
      (op (car sequence1)
          (accumulate op initial (cdr sequence1)))))


(define (filter predicate sequence)
  (cond ((null? sequence) (list))
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append (list) (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      (list)
      (cons low (enumerate-interval (+ low 1) high))))


; Функция перебора всех вариантов
(define (make-perebor n psize)
  (let ((seq (enumerate-interval 1 n)))
    (define (permutations-t s size)
      (if (= size 0) ; пустое множество?
          (list (list)); последовательность,
          ; содержащая пустое множество
          (flatmap (lambda (x)
                     (map (lambda (p) (cons x p))
                          (permutations-t seq (- size 1))))
                   s)))
    (permutations-t seq psize)))

(define empty-board (make-perebor 8 2))
(display empty-board)


; (1 2 3 4 ... n) k-вертикаль ((1 1) (1 2) (1 3) ... (n n))
(define (adjoin-position new-row k rest-of-queens)
   (map (lambda (y) 
    
   rest-of-queens)
   
   
(define (queens board-size)
  (define (queen-cols k) ;Начинем процедуру
    (if (= k 0) ; Если k кончилось то возвращаем 
        ;(list empty-board) ; Пустое множеств позиций
        (make-perebor board-size 2) ; генерируем 64 ячейки если передаем к примеру board-size = 8
        (filter 
         (lambda (positions) (safe? k positions)) ; выфельтровываем те что можно сохранить
         (flatmap 
          (lambda (rest-of-queens) ; есть способ размещения k-1 ферзя на первых k-1 вертикалях - обрабатывает список ((1 1) (1 2) (1 3) ... (n n))
            (map (lambda (new-row) ; обрабатывает список  (1 2 3 4 ... n) 
                   (adjoin-position new-row k rest-of-queens)) ; добавляет нового ферзя на определенных горизонтали и вертикали к заданному множеству позиций
                 (enumerate-interval 1 board-size))) ; создаем числовой список (1 2 3 4 ... n)
          (queen-cols (- k 1)))))) ; рекурсивно вызываем основную функцию
  (queen-cols board-size))


;;(queens 8)