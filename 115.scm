(define (cube x) (* x x x))

(define (p x) 
  (display 1)
  (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

(sine 12.15)
(sine 10000.15)
(sine 1000000000000.15)




