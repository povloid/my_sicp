(define (make-interval a b) (cons a b))

(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

; Сложение интервалов
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))



; Разность интервалов
(define (razn-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

; Произведение двух интервалов
(define (mul-interval-old x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

;(define (mul-interval x y)
;  (let ((lx (lower-bound x)) 
;        (ux (upper-bound x))
;        (ly (lower-bound y)) 
;        (uy (upper-bound y)))
;    (cond (
        
                             

;Деление интервалов
(define (div-interval x y)
  (if (< (lower-bound y) 1) 
      (error "Делить нельзя, интревал пересекает 0")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

;Радиус интервала
(define (interval-radius x)
  (abs (/ (- (upper-bound x) (lower-bound x)) 2)))


;Печать интервала
(define (print-interval x)
  (newline)
  (display "Интервал: от ")
  (display (lower-bound x))
  (display " до ")
  (display (upper-bound x))
  (display " радиус: ")
  (display (interval-radius x)))


(define a (make-interval 5 6))
(define b (make-interval 8 9))

(print-interval a)
(print-interval b)

(define c (add-interval a b))
(print-interval c)

(define d (mul-interval a b))
(define e (div-interval c b))

(print-interval c)
(print-interval d)
(print-interval e)


;;(define f (make-interval (- 1) 4))
;;(define e (div-interval c f))







