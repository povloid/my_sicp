(define (cube x) (* x x x))

; Обобщенная сумма
(define (sum term a next b)
  (if (> a b) 0
      (+ (term a) (sum term (next a) next b))))


(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2)) add-dx b) 
     dx))

(integral cube 0 1 0.01)
(integral cube 0 1 0.001)

; h = (b - a)/n - для какогото четного целого числа. 
; yk = f(a + kh)


(define (integral-2 f a b n)
  (define h (/ (- b a) n)) ; Нельзя делать как (h) надо просто h, очень важно!
  (define (inc x) (+ x 1))
  (define (yk k)
    (define m
      (if (or (= k 0) (= k n)) 1
          (if (even? k) 2 4)))
    (* m (f (+ a (* k h)))))
  (/ (* h (sum yk a inc n) ) 3))


(integral-2 cube 0 1 100)
(integral-2 cube 0 1 1000)








