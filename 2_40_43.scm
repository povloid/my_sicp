(define (square x) (* x x))

(define (divides? a b)
  (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))


(define (smallest-divisor n)
  (find-divisor n 2))



(define (prime? n)
  (= n (smallest-divisor n)))




(define (enumerate-interval low high)
  (if (> low high)
      (list)
      (cons low (enumerate-interval (+ low 1) high))))



(define (accumulate op initial sequence1)
  (if (null? sequence1)
      initial
      (op (car sequence1)
          (accumulate op initial (cdr sequence1)))))


(define (filter predicate sequence)
  (cond ((null? sequence) (list))
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))




(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))






(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))



(define (flatmap proc seq)
  (accumulate append (list) (map proc seq)))


(define (permutations s)
  (if (null? s) ; пустое множество?
      (list (list)); последовательность,
      ; содержащая пустое множество
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))



(remove 2 (list 1 2 3))

(permutations (list 1 2 3))

(enumerate-interval 1 7)



(accumulate append
            (list)
            (map (lambda (i)
                   (map (lambda (j) (list i j))
                        (enumerate-interval 1 (- i 1))))
                 (enumerate-interval 1 7)))


(newline)

;Упражнение 2.40.
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1)))) 
           (enumerate-interval 1 n)))


(unique-pairs 7)



(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))


(prime-sum-pairs 10)


(define (prime-sum-pairs-2 n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

(prime-sum-pairs-2 10)


;Упражнение 2.41.


;(define (make-3e n psize)
;  (let ((seq (enumerate-interval 1 n)))
;    (define (perebor r size)
;      (if (= size 0)
;          r
;          (map (lambda (x) (perebor (append r (list x))  (- size 1)))
;               seq)))
;    (perebor (list) psize)
;    ))


; Функция перебора всех вариантов
(define (make-perebor n psize)
  (let ((seq (enumerate-interval 1 n)))
    (define (permutations-t s size)
      (if (= size 0) ; пустое множество?
          (list (list)); последовательность,
          ; содержащая пустое множество
          (flatmap (lambda (x)
                     (map (lambda (p) (cons x p))
                          (permutations-t seq (- size 1))))
                   s)))
    (permutations-t seq psize)))

(make-perebor 3 4)

(define (find-3 n s)  
  (filter 
   (lambda (x) (= (accumulate + 0 x) s)) 
   (make-perebor n 3)))

(find-3 4 10)



















