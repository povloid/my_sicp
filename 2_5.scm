(define (power x y)
  (if (= y 0)
      1
      (* x (power x (- y 1)))))

(define a (power 2 5))
(define b (power 3 8))

(display a)
(newline)
(display b)
(newline)
(define c (* a b))
(newline)
(display c)

(remainder (/ (/ (/ c 2) 2) 2) 2)
(remainder (/ (/ (/ (/ c 3) 3) 3) 3) 3)


(define (rem-c b v)
  (define (rem-c-it v c)
  (if (= (remainder v b) 0) (rem-c-it (/ v b) (+ c 1)) c))
  (rem-c-it v 0))

(rem-c 2 c)
(rem-c 3 c)





(define (cons-h a b)
  (* (power 2 a) (power 3 b)))

(define (get-a c)
  (rem-c 2 c))

(define (get-b c)
  (rem-c 3 c))

(define c2 (cons-h 7 9))

(get-a c2)
(get-b c2)


