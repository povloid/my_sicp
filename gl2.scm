
; Представление рациональных чисел
;(define (make-rat n d) (cons n d)) ;Первый вариант, самый простой

(define (make-rat n d) ;вариант конструктора с применением к аименьшему знаминателю
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))


(define (numer x) (car x))

(define (denom x) (cdr x))





; Определим операции над рациональными числами
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

#|(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))|#

(define (mul-rat x y)
  (let ((h (* (numer x) (numer y))) 
        (z (* (denom x) (denom y))))
    (define k (if (or (< h 0) (< z 0)) (- 1) 1))
    (make-rat (* k(abs h)) (abs z))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))


; Печать 


(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))



(define one-half (make-rat 1 2))


(print-rat one-half)


(define one-third (make-rat 1 3))

(print-rat (add-rat one-half one-third))

(print-rat (mul-rat one-half one-third))

(print-rat (add-rat one-third one-third))

(newline)
(define a (make-rat 1 2))
(define b (make-rat (- 1) 5))

(print-rat a)
(print-rat b)

(print-rat (mul-rat a b))
