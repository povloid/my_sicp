; Квадратные корни

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? oldguess guess x)
  (< (abs (- (* guess guess) x)) oldguess))

(define (sqrt-iter oldguess guess x)
  (if (good-enough? oldguess guess x)
      oldguess
      (sqrt-iter guess (improve guess x)
                 x)))

(define (sqrt1 x)
  (sqrt-iter 1.0 1.0 x))


(sqrt1 25)