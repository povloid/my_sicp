; Представление листьев
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))




; Представление дерева
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))



;Процедура декодирования
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "плохой бит -- CHOOSE-BRANCH" bit))))



(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))


(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))



(make-leaf-set (list  (list 'A 4)
                      (list 'B 2)
                      (list 'C 1)
                      (list 'D 1)))


(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))



(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))


;Упражнение 2.67.
;(a d a b b c a)
(define message (decode sample-message sample-tree))
(display message)

(newline)
(display sample-tree)
(newline)

(define (have? c l)
  (cond ((null? l) #f)
        ((eq? c (car l)) #t)
        (else (have? c (cdr l)))))

(have? 6 (list 1 2 3))


(define (encode-symbol c tree)  
  (let ((lb (left-branch tree))
        (rb (right-branch tree)))
    (cond ((and (leaf? lb) (eq? c (symbol-leaf lb))) (list '0))
          ((and (leaf? rb) (eq? c (symbol-leaf rb))) (list '1))
          ((have? c (symbols lb)) (cons '0 (encode-symbol c lb)))
          (else (cons '1 (encode-symbol c rb))))))



(encode-symbol 'c sample-tree)



;Упражнение 2.68.
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))



(encode message sample-tree)



;Упражнение 2.69.
(define tt1 (list  (list 'A 4)
                   (list 'B 2)
                   (list 'C 1)
                   (list 'D 1)
                   (list 'E 1)
                   (list 'F 1)))

(define t1 (make-leaf-set tt1))



(define (merge-0 tree m)
    ;(newline)
    ;(display tree)(display " +++ ")(display m)(newline)
    ;(display  (cadr tree))(display " - ")(display (car tree))(newline)
    (cond ((not (pair? tree)) tree)
          ((or (< m (weight (car tree))) (< m (weight (cadr tree)))) tree)
          (else (let ((ntree (make-code-tree  (cadr tree) (car tree))))
                  (cons ntree (merge-0 (cddr tree) (weight ntree)))))))


;(< 2 (weight (car t1)))

;(or (< 2 (weight (car t1))) (< 2 (weight (cadr t1))))

(newline)
(merge-0 (merge-0 (merge-0 t1 6) 6) 6)
(newline)

(define (successive-merge tree)
  (define (merge-1 tree m)
    ;(newline)
    ;(display tree)(display " +++ ")(display m)(newline)
    ;(display  (cadr tree))(display " - ")(display (car tree))(newline)
    (cond ((not (pair? tree)) tree)
          ((or (< m (weight (car tree))) (< m (weight (cadr tree)))) tree)
          (else (let ((ntree (make-code-tree  (cadr tree) (car tree))))
                  (cons ntree (merge-1 (cddr tree) (weight ntree)))))))
  (if (null? (cdr tree))
      tree
      (successive-merge (merge-1 tree 1000))))


(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(generate-huffman-tree tt1)











;(make-code-tree (make-leaf 'A 4) (make-leaf 'B 2))













