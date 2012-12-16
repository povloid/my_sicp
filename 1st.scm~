; Первый пробный вариант
(define a 10)
(define b 5)

(+ a b)

; Упражнения 1

(if (< a b) b a)

(define (max2 a b) 
  (if (< a b) b a))
   

(max2 2 3)

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))


(define (cond_1  a b)
(cond 
       ((= a 10) 100)
       ((= b 20) 200)
       (else 0)))


; Срабатявает первое условие
(cond_1 3 2)
(cond_1 10 2)
(cond_1 2 20)
(cond_1 10 20)

; Упражнение 1.2

(/
 (+ 5 4 (- 2(- 3(+ 6 (/ 4 5)))))
 (* 3 (- 6 2) (- 2 7)))


; Упражнение 1.3


(define (sumqfrom2max a b c)
  (define (qsum x y) 
    (+ (* x x) (* y y)))
  
  (cond ((and (> c a) (> c b))
         (qsum c (max a b)))
        ((and (> b a) (> b c))
         (qsum b (max a c)))
        ((and (> a b) (> a c))
         (qsum a (max b c))))
  )  
  
(sumqfrom2max 1 4 3)










