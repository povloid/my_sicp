; 1.11

; Рекурсивное определение
(define (fr n)
  (if
   (< n 3) n
    (+ (fr (- n 1)) (fr (- n 2)) (fr (- n 3)))
    )
  )

(fr 5)

; Итеративный процесс
; a <- a + b + c
; b <- a
; c <- b



(define (f n)
  (f-iter 2 1 0 n))
(define (f-iter a b c count)
  (if (= count 0)
      c
      (f-iter (+ a b c) a b (- count 1))))

(f 40 )











