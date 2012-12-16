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
        (else
         (qsum a (max b c))))
  )  
  
(sumqfrom2max 8 3 4)




(define (p) (p))
(define (test x y)
(if (= x 0)
0
y))

; интерпритатор апликативный
;(test 0 (p)) 


; Квадратные корни

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? oldguess guess x)
  (> (abs (- (* guess guess) x)) oldguess))

(define (sqrt-iter oldguess guess x)
  (if (good-enough? oldguess guess x)
      oldguess
      (sqrt-iter guess (improve guess x)
                 x)))

(define (sqrt1 x)
  (sqrt-iter 1.0 1.0 x))


(sqrt1 25)

; New if

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter2 guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter2 (improve guess x)
                     x)))


(define (sqrt2 x)
  (sqrt-iter2 1.0 x))


;;(sqrt2 9)







