; Селекторы
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (value tree) (cadddr tree))


(define (make-tree entry left right value)
  (list entry left right value)) ; В основе всего список из 3 элементов


;Упражнение 2.66.
(define (get key set)
  (cond ((null? set) #f)
        ((= key (entry set)) (value set))
        ((< key (entry set))
         (get key (left-branch set)))
        ((> key (entry set))
         (get key (right-branch set)))))




; Определим простое дерево
(define set1 (list 5
                   (list 4 '() '() '444)
                   (list 6 '() '() '666) 
                   '555)
  )


(get 6 set1)



