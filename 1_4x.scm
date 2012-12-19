(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))


(define dx 0.00001)

(define (cube x) (* x x x))

((deriv cube) 5)


(define (square x) (* x x))

((deriv square) 5)


(define tolerance 0.00001)

; Нахождение неподвихной точки функции 
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))


(define (sqrt2 x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))

(sqrt2 4)





; Задание 1.40 нахождение кубических уравнений
(define (cubic-x x a b c)
  (+ (* x x x) (* a x x) (* b x) c))

(define (cubic a b c)
  (newtons-method (lambda (x) (+ (* x x x) (* a x x) (* b x) c))
                  1.0))

(cubic 5 6 7)
(cubic-x -3.9258 5 6 7)
  

; Задание 1.41

(define (inc x) (+ x 1))

(define (double-f f)
  (lambda (x) (f (f x) )))

(inc 1)

((double-f inc) 1)

(((double-f (double-f double-f)) inc) 5)




; Задание 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

((compose square inc) 6)


; Задание 1.42

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
                    



((repeated-r inc 5) 0)
((repeated inc 5) 0)


((repeated-r square 2) 5)
((repeated square 2) 5)


; 1.44 Сглаживаниие

(define (smoothing f dx)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))


(sin 1)
((smoothing sin 0.01) 1)
((repeated (smoothing sin 0.01) 5) 1)













