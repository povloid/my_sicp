(define (cons2 x y)
  (lambda (m) (m x y)))

(define (car2 z) 
  (z (lambda (p q) p))) ; Создает соответствующую процедуру для возвращения

(define (cdr2 z)
  (z (lambda (p q) q)))

(car2 (cons2 1 2))
(cdr2 (cons2 1 2))
