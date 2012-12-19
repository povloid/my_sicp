(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (let ((gg (abs (- v1 v2))))
    (display v1)(display "-")(display v2)(newline)  
    (< gg tolerance)))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


(fixed-point cos 1.0)

(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)


(fixed-point (lambda (x) (+ 1 (/ 1 x)))  1.0)



(fixed-point (lambda (x) (/ (log 1000) (log x)))  2.0)


(define (cont-frac n d k)
   (define (cont-frac-iter rez k)
     (if (< k 1)
         rez
         (cont-frac-iter (/ (n k) (+ (d k) rez)) (- k 1))))
   (cont-frac-iter 0 k))

(define (cont-frac-r n d k)
  (define (cont-frac-rr i)
    (let ((nk (n i)) (dk (d i)))
      (if (> i k) (/ nk dk)
          (/ nk (+ dk (cont-frac-rr (+ i 1)))))))
  (cont-frac-rr 1))
      

(fixed-point (lambda (x) (+ 1 (/ 1 x)))  1.0)

(/ 1 (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 1000))
(/ 1 (cont-frac-r (lambda (i) 1.0) (lambda (i) 1.0) 1000))

(newline)

(/ 1.0 (+ 1.0 (/ 1.0 (+ 1.0 (/ 1.0 (+ 1.0 (/ 1.0 (+ 1.0 (/ 1.0 (+ 1.0 (/ 1.0 1.0)))))))))))
(/ 1.0 (/ 1.0 (+ 1.0 (/ 1.0 (+ 1.0 (/ 1.0 (+ 1.0 (/ 1.0 (+ 1.0 (/ 1.0 (+ 1.0 (/ 1.0 1.0))))))))))))

; 1.38
; 1,2,3,4,5,6,7,8,9,
; 1,2,1,1,4,1,1,6,1,
(define (le x)
  (define (le-it last2 i k) 
    (let ((r (if (> i 2) (+ last2 2) 1))
          (in (if (> i 2) 0 i)))
    (if (= k x)
        r
        (le-it (if (> r 1) (+ 2 last2) last2) (+ in 1) (+ k 1)))))
   (le-it 0 2 1))

(le 1)
(le 2)
(le 3)
(le 4)
(le 5)
(le 6)
(le 7)
(le 8)
(le 9)
(le 10)
(le 11)


(+ 2 (cont-frac (lambda (i) 1.0) 
                  le
                  1000))

;1,2,3,4
;1,3,5,7

(define (ncr x) (+ (- x 1) x))

(ncr 1)
(ncr 2)
(ncr 3)
(ncr 4)
(ncr 5)





(define (tg x)
  (cont-frac-r (lambda (i) (if (= i 1) x (- (* x x))))
             (lambda (i) (+ i (- i 1.0))) 
             10000))

(tg 1.0)
(tg 0.5)
(tg 0.3)
(tg 0)







