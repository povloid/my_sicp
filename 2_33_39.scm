(define (square x) (* x x))

(define (accumulate op initial sequence1)
  (if (null? sequence1)
      initial
      (op (car sequence1)
          (accumulate op initial (cdr sequence1)))))


; Упражненение 2.33
(define (map2 p sequence1)
  (accumulate (lambda (x y)
                (cons (p x) y)) (list) sequence1))

(map2 square (list 1 2 3 4 5 6 7))
(map square (list 1 2 3 4 5 6 7))



(define (append2 seq1 seq2)
  (accumulate cons seq2 seq1))


(append2 (list 1 2 3 4 5) (list 6 7 8 9 10))


(define (length2 sequence1)
  (accumulate (lambda (x y)  (+ 1 y))
              0 sequence1))


(length2 (list 1 2 3 4 5 6))



; Упражненение 2.34


(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) 
                (+ this-coeff (* x  higher-terms)))
              0
              coefficient-sequence))

; Например, чтобы вычислить 1 + 3x + 5x^3 + x^5 в точке x = 2, нужно ввести


(horner-eval 2 (list 1 3 0 5 0 1))
(+ 1 (* 3 2) (* 5 2 2 2) (* 2 2 2 2 2))



; Упражненение 2.35
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))


(count-leaves (list (list 1 2) (list 3 4)))



(define (count-leaves2 t)
  (accumulate + 0
              (map (lambda (x) 
                     (if (pair? x) (count-leaves2 x) 1))
                   t)))

(count-leaves2 (list (list 1 2 (list 1 2)) 1 2 3 4 5 6 7 (list 1 2)))













