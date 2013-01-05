; Представления деревьев


; Селекторы
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right)) ; В основе всего список из 3 элементов



(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))




; Определим простое дерево
(define set1 (list 5
                   (list 4 '() '())
                   (list 6 '() '())))



(element-of-set? 6 set1)



(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))


(adjoin-set 2 set1)

;Упражнение 2.63.



(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))


(newline)
(tree->list-1 set1)
(tree->list-2 set1)
; Обе функции работают почти одинаково и выдают один и тот же порядок в результатах



;Упражнение 2.64.
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))





(list->tree (tree->list-1 set1))
(list->tree (list 1 3 5 7 9 11))




;Упражнение 2.65.


(define set2 (list->tree (list 1 2 3 4 5 6 7)))
(define set3 (list->tree (list 4 5 6 7 8 9 10)))

; Проще всего использовать старые наработки для упорядоченных множеств
; делать промежуточный упорядоченный список потом обрабатывать 
; его и приобразовывать обратно в дерево


(define (intersection-set-s set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set-s (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set-s (cdr set1) set2))
              ((< x2 x1)
               (intersection-set-s set1 (cdr set2)))))))


(define (intersection-set set1 set2)
  (list->tree 
   (intersection-set-s (tree->list-1 set1) (tree->list-1 set2))))

(newline)
(intersection-set set2 set3)


(define (adjoin-set-s x set)
    (cond ((null? set) (cons x set))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set-s x (cdr set))))))


(define (union-set-s set1 set2)
  (if (null? set1) set2
      (union-set-s (cdr set1) (adjoin-set-s (car set1) set2))))

(define (union-set set1 set2)
  (list->tree 
   (union-set-s (tree->list-1 set1) (tree->list-1 set2))))
(newline)
(union-set set2 set3)

























