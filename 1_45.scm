(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))


(define dx 0.00001)

(define (cube x) (* x x x))

((deriv cube) 5)


(define (square x) (* x x))

((deriv square) 5)


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



(define (sqrt2 x)
  (fixed-point (lambda (y) (average y (/ x y)))
             1.0))

(sqrt2 16)


(define (sqrt3 x)
  (fixed-point (lambda (y) (average y (/ x (* y y))))
             1.0))


(sqrt3 27)
(sqrt3 64)

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

(cube-root 27)


(define (x-y x y)
  ((repeated (lambda (z) (* z x)) (- y 1)) x))

(x-y 3 3)
(x-y 2 5)

(define (quad-root x)
  (fixed-point (repeated (average-damp (lambda (y) (/ x (x-y y 3)))) 2)
               1.0))

(quad-root 256)

(define (root-x-y x y)
  (fixed-point (repeated (average-damp (lambda (z) (/ x (x-y z (- y 1))))) (- y 2))
               1.0))



(root-x-y 27 3)
(root-x-y 64 3)
(root-x-y 256 4)
;(root-x-y 1024 5)
;(root-x-y 32 5)

