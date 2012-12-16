(define (even1? n)
  (= (remainder n 2) 0))

(define (square x)
  (* x x))


(define (fast-expt b n)
  (fast-expt-iter 1 b (+ n 1)))

(define (fast-expt-iter a b n)
  (if (= n 1) a
      (if (even1? n)
         (fast-expt-iter (* a b)(square  b) (/ n 2)) 
         (fast-expt-iter (* a b) b (- n 1)))))
                 

(fast-expt 4 1)
(fast-expt 4 2)
(fast-expt 4 3)
(fast-expt 4 4)
(fast-expt 4 5)
(fast-expt 4 6)
(fast-expt 4 7)
(fast-expt 4 8)
          
;  1 * 4^3 =  64
; 4 * 4^2 =  64
; 16 * 4^1 =  64


