(define (square x)
  (* x x))


(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor))))) ;; Упражнение 1.23

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

;; Упражнение 1.23
(define (next n)
  (if (= n 2) 3 (+ n 2)))


;; rutime в данной реализации нет

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))


;; буду делать без runtime
(prime? 2)

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

; Тест ферма
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (- n 12))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))




(define (search-for-primes s e n)
  ;; Задание 1.24
  ;;(define (p?) (and (not (even? s)) (fast-prime? s 3)))
  (define (p?) (and (not (even? s)) (prime? s)))
  (if (p?)
      (display s)          
      (display "."))
  (if (or (> (+ s 1) e) (= n 0))
      (display " END")
      (search-for-primes (+ s 1) e (if (p?) (- n 1) n))))


(search-for-primes 1000 1100 3)
(newline)
(search-for-primes 10000 10100 3)
(newline)
(search-for-primes 100000 100100 3)
(newline)
(search-for-primes 1000000 1000100 3)


;Задание 1.27
(prime? 561)
(fast-prime? 561 3)
(prime? 6601)
(fast-prime? 6601 3)






