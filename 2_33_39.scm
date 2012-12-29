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


; Упражненение 2.36

(define l1 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))


(map (lambda (x) (car x)) l1)

(map (lambda (x) (cdr x)) l1)


(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      (list)
      (cons (accumulate op init (map (lambda (x) (car x)) seqs))
            (accumulate-n op init (map (lambda (x) (cdr x)) seqs)))))

(accumulate-n + 0 l1)

; Упражненение 2.37

(define v1 (list 2 2 2 2))
(define v2 (list 4 4 4 4))

(define m1 (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))


(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(map * v1 v2)
(dot-product v1 v2)

(define (matrix-*-vector m v)
  (map (lambda (x) (map * v x))
       m))

(display "-------------------------------------------")
(newline)
(display v1)
(NEWLINE)
(display m1)
(NEWLINE)
(matrix-*-vector m1 v1)

(define (transpose mat)
  (accumulate-n cons
                (list)
                mat))

(display m1)

(newline)
(transpose m1)


(define m2 (list (list 1 2) (list 3 4)))
(define m3 (list (list 5 6) (list 7 8)))

(transpose m2)

(car (list 5 6))
(car (cdr m2))

(define (matrix-*-matrix m n)
  ;(newline)(display "|")(display m)(display "-")(display n)(display "->")
  (let ((cols (transpose n))) ;((1 3) 
    ;                           (2 4))
    ;(display cols)(display " -> ")
    (map 
     (lambda (x) ;(5 6)
       (accumulate-n + 0 (matrix-*-vector cols x)))
     
     m))) ;((5 6) (7 8))



(matrix-*-matrix m2 m3)



; Упражненение 2.38

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)

; 1/6 ошибся - будет 1*1/2
(fold-right / 1 (list 1 2 3))
; 1/6
(fold-left / 1 (list 1 2 3))


; (1 (2 (3)))
(fold-right list (list) (list 1 2 3))

; (list () 1) (2 3) .....-> ((() 1) 2) 3)
(fold-left list (list) (list 1 2 3))

; не джолжно быть головы и хвоста. 

; Упражненение 2.39

(define (reverse1 sequence)
  (fold-right (lambda (x y) (append y (list x)) ) (list) sequence))

(reverse1 (list 1 2 3 4 5 6 7))

(define (reverse2 sequence)
  (fold-left (lambda (result carx) (cons carx result) ) (list) sequence))

(reverse2 (list 1 2 3 4 5 6 7))
