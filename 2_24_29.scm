(define a (cons (list 1 2) (list 3 4)))

(cdr a)

(list 1 (list 2 (list 3 4)))



; Упражнение 2.24
(define a (list 1 3 (list 5 7) 9))

(car (cdr (car (cdr (cdr a)))))


(define b (list (list 7)))
(car (car b))

(define c (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr c))))))))))))

; Упражнение 2.25

(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y)
(cons x y)
(list x y)


; Упражнение 2.27
(newline)

(define x1 (list (list 1 2) (list 3 4)))

(define (reverce l)
  (define (reverce-it rl l)
    (if (null? (cdr l))
        (cons (car l) rl)
        (reverce-it (cons (car l) rl) (cdr l))))
  (reverce-it (list) l))

(define (deep-reverce l)
  (define (reverce-it rl l)
    (let ((carl (if (pair? (car l)) (deep-reverce (car l)) (car l)))
          (cdrl (cdr l)))
      (if (null? cdrl)
          (cons carl rl)
          (reverce-it (cons carl rl) cdrl))))
  (reverce-it (list) l))

(reverce x1)
(deep-reverce x1)


;Упражнение 2.28
(newline)

(define (fringe tree)
  (append 
   (cond ((null? tree) (list))
         ((pair? tree) (fringe (car tree)))
         (else (list tree)))
   (cond ((null? tree) (list))
         ((pair? tree) (fringe (cdr tree)))
         (else (list)))))

(define x2 (list (list 1 2) (list 3 4)))

(fringe a)
(fringe x1)
(fringe x2)

(fringe (list x2 x2))


;Упражнение 2.29
(newline)


(define (make-mobile left right)
  ;(list left right))
  (cons left right)) ; d)



(define (make-branch length structure)
  ;(list length structure))
  (cons length structure)) ; d)

; a)
(define (left-branch m)
  (car m))

(define (right-branch m)
 ; (car (cdr m)))
  (cdr m)) ; d)

(define (branch-length b)
  (car b))

(define (branch-structure b)
  ;(car (cdr b)))
  (cdr b)) ; d)

; b)
(define (total-weight m)
  (let ((lbs (branch-structure (left-branch m)))
        (rbs (branch-structure (right-branch m))))
  (+ 
   (if (pair? lbs) (total-weight lbs) lbs) 
   (if (pair? rbs) (total-weight rbs) rbs)))) 


; c)

(define (is-balanced? m)
  (let ((lb (left-branch m))
        (rb (right-branch m)))
    (define lbs (branch-structure lb))
    (define rbs (branch-structure rb))
  (= (* (if (pair? rbs) (total-weight rbs) rbs)
        (branch-length lb))
     (* (if (pair? lbs) (total-weight lbs) lbs) 
        (branch-length rb)))))


(define m1 (make-mobile
           (make-branch 10 50)
           (make-branch 10 50)
           ))

(define m2 (make-mobile
           (make-branch 10 50)
           (make-branch 10 50)
           ))


(define m3 (make-mobile
           (make-branch 10 m1)
           (make-branch 10 m2)
           ))

(define m4 (make-mobile
           (make-branch 10 200)
           (make-branch 10 m3)
           ))
  

(total-weight m1)
(total-weight m2)
(total-weight m3)
(total-weight m4)

(is-balanced? m4)
