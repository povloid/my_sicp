(define (square x)
  (* x x))

(define (expmod base exp m)
  (cond 
    ((= exp 0) 1)
    ((even? exp)
     (remainder (square (expmod base (/ exp 2) m))
                m))
    (else
     (remainder (* base (expmod base (- exp 1) m))
                m))))

; Тест ферма
(define (fermat-test n)
  (define (try-it a)
    (not (= (expmod a n n) 0))
    (= (expmod a n n) a))
  (try-it (+ 1 (- n 12))))





(fermat-test 1009)
(fermat-test 1010)


