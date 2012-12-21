; Точка
(define (make-point x-point y-point)
  (cons x-point y-point))
; Координата x
(define (x-point point)
  (car point))

; координата y
(define (y-point point)
  (cdr point))

; Печать
(define (print-point p)
  (newline)
  (display "точка (x,y): (")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
  
; процедуры работы с точеками

(define (midpoint-segment p1 p2)
  (make-point (/ (+ (x-point p1) (x-point p2)) 2)
              (/ (+ (y-point p1) (y-point p2)) 2)))

(define a (make-point (- 5) (- 5)))
(define b (make-point 5 5))

(print-point a)
(print-point b)

(print-point (midpoint-segment a b))



; Прямоугольник
(define (make-rectangle p1 p2)
  (cons p1 p2))

(define (point-1 r)
  (car r))

(define (point-2 r)
  (cdr r))

(define (print-rectangle r)
  (newline)
  (display "прямоугольник из точек:")
  (print-point (point-1 r))
  (print-point (point-2 r))
  (newline))
 

;Операции



;Вычисление площади прямоугольника
(define (area r)
  (abs (*
   (- (x-point (point-1 r)) (x-point (point-2 r)))
   (- (y-point (point-1 r)) (y-point (point-2 r))))))

(define (perimetr r)
  (* 2 (+ 
   (abs (- (x-point (point-1 r)) (x-point (point-2 r))))
   (abs (- (y-point (point-1 r)) (y-point (point-2 r)))))))


(define r (make-rectangle a b))

(print-rectangle r)

(area r)
(perimetr r)





  
  