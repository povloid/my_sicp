(define (square x) (* x x))


; Упражнение 2.30

(define (square-tree-1 tree)
  (cond ((null? tree) (list))
        ((pair? tree) (cons  
                       (square-tree-1 (car tree))
                       (square-tree-1 (cdr tree))))
        (else (square tree))))


(define xlist (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(square-tree-1 xlist)


(define (square-tree-2 tree)
  (map 
   (lambda (stree)
     (if (pair? stree)
         (square-tree-2 stree)
         (square stree)))
   tree))

(square-tree-2 xlist)



; Упражнение 2.31

(define (tree-map f tree)
  (map 
   (lambda (stree)
     (if (pair? stree)
         (tree-map f stree)
         (f stree)))
   tree))

(define (square-tree-3 tree) (tree-map square tree))

(square-tree-3 xlist)



; Упражнение 2.32
; 1 2 3
; rest = subsets 2 3 = subsets 3 = () + 


(map (lambda (x)
       (display x)
       (cons 1 x))
     (list 1 2 3))

(define (subsets s)
  (if (null? s)
      (cons s s) 
      (let ((rest (subsets (cdr s))))
        (append rest (map 
                      (lambda (x)
                        (cons (car s) x))
                      rest)))))

(subsets (list 1 2 3 4 5))




















