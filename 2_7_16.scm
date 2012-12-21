(define (make-interval a b) (cons a b))

(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))


;Ему нужна программа, которая работала бы с числами,
;представленными в виде срединного значения и аддитивной погрешности; например, ему
;хочется работать с интервалами вида 3.5 0.15, а не [3.35, 3.65].±

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

; Задача 2.12
; Конструктор со средним значением и процентом
(define (make-center-percent c p)
  (let ((w (* (/ c 100) p)))
    (make-center-width c w)))

; селектор процент
(define (percent i)
  (/ (width i) (/ (center i) 100)))



(make-center-percent 50 10)

(percent (make-center-percent 50 10))

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









; ++ ++
; -- ++
; ++ --
; -- -+
; -+ -+
; -+ --
; -+ ++
; -- -+
; -- --


; Пока взял готовый вариант. Для анализа. Изза непонимания интервальной математикиы
(define (mul-interval x y)
  (let ((xl (lower-bound x))
        (xu (upper-bound x))
        (yl (lower-bound y))
        (yu (upper-bound y)))
    (cond ((>= xl 0)
           (cond ((>= yl 0)
                  (make-interval (* xl yl) (* xu yu)))
                 ((<= yu 0)
                  (make-interval (* xu yl) (* xl yu)))
                 (else
                  (make-interval (* xu yl) (* xu yu)))))
          ((<= xu 0)
           (cond ((>= yl 0)
                  (make-interval (* xl yu) (* xu yl)))
                 ((<= yu 0)
                  (make-interval (* xu yu) (* xl yl)))
                 (else
                  (make-interval (* xl yu) (* xl yl)))))
          (else
           (cond ((>= yl 0)
                  (make-interval (* xl yu) (* xu yu)))
                 ((<= yu 0)
                  (make-interval (* xu yl) (* xl yl)))
                 (else
                  (make-interval (min (* xl yu) (* xu yl))
                                 (max (* xl yl) (* xu yu)))))))))


;Деление интервалов
(define (div-interval x y)
  (if (<= (lower-bound y) 0) ; Меньше или равны 0
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


(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))
(par1 a b)
(par2 a b)







