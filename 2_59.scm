(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))



(define set1 (list 1 2 3 4 5 6 7))
(define set2 (list 4 5 6 7 8 9 10))



(element-of-set? 11 set1)
(element-of-set? 1 set1)
(element-of-set? 2 set1)



(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))


(adjoin-set 1 set1)
(adjoin-set 12 set1)


(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))


(intersection-set set1 set2)


;Упражнение 2.59.
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (union-set (cdr set1) (cons (car set1) set2)))))



(union-set set1 set2)         
(union-set '() '())        

