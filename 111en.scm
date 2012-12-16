(define (f1 n)
  (if (< n 3) n
      (+ (f1 (- n 1)) (* 2 (f1 (- n 2))) (* 3 (f1 (- n 3))))))

(f1 20)

; 0 1 2 
(define (f2 n) (f-it 2 1 0 n))

(define (f-it a b c n)
  (if (= n 0) c
      (f-it (+ a (* 2 b) (* 3 c)) a b  (- n 1))))


(f2 20)