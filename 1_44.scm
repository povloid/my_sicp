(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))


(define dx 0.00001)

(define (cube x) (* x x x))

;((deriv cube) 5)


(define (square x) (* x x))

;((deriv square) 5)


(define tolerance 0.0000001)

(define (double-f f)
  (lambda (x) (f (f x) )))

(define (average x y) (/ (+ x y) 2))


(define (repeated-r fn n)
    (if (< n 2)
        fn
        (lambda (x) (fn ((repeated-r fn (- n 1)) x)))))
                    


(define (repeated fn n)
  (define (repeated-it f n)
    (if (= n 1)
        f
        (repeated-it (lambda (x) (fn (f x))) (- n 1))))
  (repeated-it fn n))


;pg - достаточно ли хороше значение
;mg - метод улучшения значения
; * Функция должна возвращать процедуру, которая принимает начальное значение 
;  аргумента и улучшает его, пока оно не станет достаточно хорошим
(define (iterative-improve pg? mg first-guess)  
    (define (try guess)
      (let ((next (mg guess)))
        (if (pg? guess next)
          next
          (try next))))
    (try first-guess))

(define (close-enough? v1 v2)
      (< (abs (- v1 v2)) tolerance))

; Нахождение неподвихной точки функции 
(define (fixed-point f first-guess)
  (iterative-improve close-enough? f first-guess))


(define (sqrt2 x)
  (fixed-point (lambda (y) (average y (/ x y)))
             1.0))


(newline)
(sqrt2 16)
(sqrt2 9)


(define (sqrt2-2 x)
  (define (improve guess)
    (average guess (/ x guess)))
  (define (good-enough? guess k)
    (< (abs (- (square guess) x)) 0.0001))
  (iterative-improve good-enough? 
                     improve
                     1.0))

(newline)
(sqrt2-2 16)
(sqrt2-2 9)


;(define (sqrt x)
;(sqrt-iter 1.0 x))





