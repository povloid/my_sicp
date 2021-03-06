(define l (list 1 2 3 4 5 6 7))

(car l)

; Задание 2.17
(define (last-pair l)
  (if (null? (cdr l))
      (car l)
      (last-pair (cdr l))))


(last-pair l)


; Задание 2.18
(define (reverce l)
  (define (reverce-it rl l)
    (if (null? (cdr l))
        (cons (car l) rl)
        (reverce-it (cons (car l) rl) (cdr l))))
  (reverce-it (list) l))

(reverce l)



; Задание 2.19


#|(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))|#


(define (first-denomination coin-values)
  (car coin-values)) 

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (no-more? coin-values)
  (null? coin-values))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))


(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(cc 100 us-coins)




;Задание 2.20

(define (same-parity . l)
  (define add?
    (if (even? (car l))
        (lambda (x) (even? x))
        (lambda (x) (not (even? x)))))
  (define (same-parity-r lt)
    (let ((next (if (null? (cdr lt))
                    (list)
                    (same-parity-r (cdr lt)))))
      (if (add? (car lt))
          (cons (car lt) next) next)))
  (cons (car l) (same-parity-r (cdr l))))


(same-parity 1 2 3 4 5 6 7)

(same-parity 2 3 4 5 6 7)


















