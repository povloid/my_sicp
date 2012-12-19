(define (cube x) (* x x x))


(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (sum-cubes a b)
  (sum cube a inc b))

(sum-cubes 1 3)


(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))


(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b) result
        (iter (next a) (* result (term a)))))
    (iter a 1))


(define (product-cubes1 a b)
  (product cube a inc b))



(define (product-cubes2 a b)
  (product-iter cube a inc b))



(product-cubes1 1 1)
(product-cubes1 1 2)
(product-cubes1 1 3)

(product-cubes2 1 1)
(product-cubes2 1 2)
(product-cubes2 1 3)


; Определили факториал
(define (factorial n)
  (define (val a) a)
  (product-iter val 1 inc n))

(factorial 5)


; Вычисление pi

(* 4.0 (/ (*  2 4 4 6 6 8 8 10 10 12) (* 3 3 5 5 7 7 9 9 11 11)))

(define (my-pi n)
  (define (h-term x) 
    (define h (+ x 1))
    (if (even? h) h (+ h 1)))
  (define (z-term x) 
    (define z (+ x 1))
    (if (even? z) (+ z 1) z))
  (* 4.0 (/ (product-iter h-term 1.0 inc n)
          (product-iter z-term 1.0 inc n))))
  

(my-pi 150.0)

;; accumulate 
(define (accumulate-i combiner null-value term a next b)
  (define (iter a result)
    (if (> a b) result
        (iter (next a) (combiner result (term a)))))
    (iter a null-value))


(define (sum-a term a next b)
  (define (combiner result ta)
    (+ result ta))
  (accumulate-i combiner 0 term a next b))

(define (sum-cubes-a a b)
  (sum-a cube a inc b))

(sum-cubes-a 1 3)


(define (product-a term a next b)
  (define (combiner result ta)
    (* result ta))
  (accumulate-i combiner 1 term a next b))

(define (product-cubes-a a b)
  (product-a cube a inc b))


(product-cubes1 1 1)
(product-cubes1 1 2)
(product-cubes1 1 3)

(product-cubes-a 1 1)
(product-cubes-a 1 2)
(product-cubes-a 1 3)

; рекурсивный вариант акумулятора
(define (accumulate-r combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
         (accumulate-r combiner null-value term (next a) next b))))


(define (product-a term a next b)
  (define (combiner result ta)
    (* result ta))
  (accumulate-r combiner 1 term a next b))

(define (product-cubes-a-r a b)
  (product-a cube a inc b))


(product-cubes1 1 1)
(product-cubes1 1 2)
(product-cubes1 1 3)

(product-cubes-a-r 1 1)
(product-cubes-a-r 1 2)
(product-cubes-a-r 1 3)



;акумулятор с фильтром

(define (filtered-accumulate-r filter-a filter-term-rezults combiner null-value term a next b)
  (define term-a (term a))
  (display term-a) (display " ")
  (if (> a b)
      null-value
      (combiner (if (and (filter-a a) (filter-term-rezults term-a)) term-a null-value) 
         (filtered-accumulate-r filter-a filter-term-rezults combiner null-value term (next a) next b))))


(define (product-accumulate-r-filtred filter-a filter-term-rezults term a next b)
  (define (combiner result ta)
    (* result ta))
  (filtered-accumulate-r filter-a filter-term-rezults combiner 1 term a next b))

(define (product-filtred-for-even?-cubes-a-r a b)
  (define (filter-a a) #t)
  (product-accumulate-r-filtred filter-a even? cube a inc b))


(product-cubes1 1 1)
(product-cubes1 1 2)
(product-cubes1 1 3)

(product-filtred-for-even?-cubes-a-r 1 1)
(product-filtred-for-even?-cubes-a-r 1 2)
(product-filtred-for-even?-cubes-a-r 1 3)
(product-filtred-for-even?-cubes-a-r 1 4)


; Тест ферма
(define (square x)
  (* x x))

(define (next n)
  (if (= n 2) 3 (+ n 2)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor))))) ;; Упражнение 1.23

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(prime? 5)


; Подзандание а, сумма квадратов простых чисел от a до b
(define (squre-prime-sum a b )
  (define (term a) a)
  (define (combiner result ta)
    (+ result ta))
  (define (filter-term-rezults a) #t)
   (filtered-accumulate-r prime? filter-term-rezults combiner 0 square a inc b))


; 1 + 4 + 9 + 25 = 39
(squre-prime-sum 1 6)  
  
;(define (product-all-nod n)
  

; НОД
(define (gcd2 a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))


(define (product-all-gcd n)
  (define (term a) a)
  (define (combiner result ta)
    (* result ta))
  (define (filter-term-rezults a) #t)
  (define (filter a)
    (and (< a n) (= 1 (gcd2 a n))))
   (filtered-accumulate-r filter filter-term-rezults combiner 1 term 1 inc n))
  
(gcd2 10 1)
(gcd2 10 2)
(gcd2 10 3)
(gcd2 10 4)
(gcd2 10 5)
(gcd2 10 6)
(gcd2 10 7)
(gcd2 10 8)
(gcd2 10 9)


(product-all-gcd 10)



















