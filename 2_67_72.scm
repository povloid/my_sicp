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
                   (list 'F 1)
                   (list 'J 1)))

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
    (cond ((or (not (pair? tree)) (null? (cdr tree))) tree)
          ((or (< m (weight (car tree))) (< m (weight (cadr tree)))) tree)
          (else (let ((ntree (make-code-tree  (cadr tree) (car tree))))
                  (cons ntree (merge-1 (cddr tree) (weight ntree)))))))
  (if (null? (cdr tree))
      (car tree)
      (successive-merge (merge-1 tree 1000))))


(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(generate-huffman-tree tt1)




;Упражнение 2.70
(define tt2 (list  (list 'A 2)
                   (list 'BOOM 1)
                   (list 'GET 2)
                   (list 'JOB 2)
                   (list 'NA 16)
                   (list 'SHA 3)
                   (list 'YIP 9)
                   (list 'WAH 1)
                   ))


(define tree-2.70 (generate-huffman-tree tt2))



;(make-code-tree (make-leaf 'A 4) (make-leaf 'B 2))

(define song '(Get a job 
               Sha na na na na na na na na 
               Get a job 
               Sha na na na na na na na na 
               Wah yip yip yip yip yip yip yip yip yip 
               Sha boom))


(define song1 '(a))

; Кодируем
(define message-2.70 (encode song tree-2.70))

(newline)
(display message-2.70)
(newline)

; Кодируем и декодируем
(decode message-2.70 tree-2.70)


(define (list-len l)
  (define (list-len-it list count)
    (if (null? list) count
        (list-len-it (cdr list) (+ count 1))))
  (list-len-it l 0))


(list-len message-2.70)
(/ (list-len message-2.70) 8.0)
; Для того чтобы закодировать по дереву хафмана понадобилосыь 10 байт
; иначе если кодировать по байту на символ то надо 36 байт
; а если ASCII кодирование то надо приблизительно 123 байта 



(define tn5 (list  (list '1 1)
                   (list '2 2)
                   (list '3 4)
                   (list '4 8)
                   (list '5 16)
                   ))


(define tn10 (list  (list '1 1)
                   (list '2 2)
                   (list '3 4)
                   (list '4 8)
                   (list '5 16)
                   (list '6 32)
                   (list '7 64)
                   (list '8 128)
                   (list '9 256)
                   (list '10 512)
                   ))



(generate-huffman-tree tn5)
(newline)

(generate-huffman-tree tn10)


;Упражнение 2.71.
;Сколько битов в таком дереве (для произвольного n) требуется, чтобы закодировать самый частый символ? 
; Требуется 1 бит

;Самый редкий символ?
; n бит

;Упражнение 2.72.
;рост алгоритма Q(n)


