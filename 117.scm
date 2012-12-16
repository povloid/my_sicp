(define (m a b)
  (display 1)
  (if (= b 0)
      0
      (+ a (m a (- b 1)))))

(m 3 6 )


(define (double x)
  (* x 2))


(define (halve x)
  (if (even? x)
      (/ x 2) 
      0))

;; Рекурсивный вариант
(define (*-fast a b)
  (if (= b 0)
      0
      (if (even? b)
          (double (*-fast a (halve b)))
          (+ a (*-fast a (- b 1))))))
     

(*-fast 10 1)
(*-fast 10 2)
(*-fast 10 3)
(*-fast 10 4)
(*-fast 10 5)
(*-fast 10 6)
(*-fast 10 7)
(*-fast 10 8)
(*-fast 10 9)
(*-fast 10 10)
(*-fast 10 12)
(*-fast 10 13)
(*-fast 10 14)
(*-fast 10 15)
(*-fast 10 16)
(*-fast 10 17)
(*-fast 10 18)
(*-fast 10 19)
(*-fast 10 20)







; Итеративный вариант логарифмическимй

(define (*-fast-log a b)
  (*-fast-log-iter 0 a b))

(define (*-fast-log-iter r a b)
  (if (or (= b 0) (= a 0))
      r
      (if (even? b)
          (*-fast-log-iter r (double a) (halve b))
          (*-fast-log-iter (+ r a) a (- b 1)))))
   


; 0 +  2 * 40 = 80
; 40 + 2 * 20 = 80
; 60 + 2 * 10 = 80
; 70 + 2 * 5 = 80
; 72 + 2 * 4 = 80
; 74 + 2 * 2 = 80
; 76 + 2 * 2 = 

(*-fast-log 2 111)
     