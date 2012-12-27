(define (accumulate op initial sequence1)
  (if (null? sequence1)
      initial
      (op (car sequence1)
          (accumulate op initial (cdr sequence1)))))



;(define (map p sequence1)
;  (accumulate (lambda (x y) ?? ) nil sequence1))

(define (append2 seq1 seq2)
  (accumulate cons seq2 seq1))


(append2 (list 1 2 3 4 5) (list 6 7 8 9 10))


(define (length2 sequence1)
  (accumulate (lambda (x y)  (+ 1 y))
                0 sequence1))


(length2 (list 1))

