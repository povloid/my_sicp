#lang racket
(require (lib "racket/draw"))
(require racket/class)

(define target (make-bitmap 500 500))
(define dc (new bitmap-dc% [bitmap target]))



; Упражнение 2.45 будет решено позже...






; Упражнение 2.46 
; Конструктор -------------------------------------------------
(define (make-vect x y)
  (cons x y))

; Селекторы
(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define tv1 (make-vect 2.0 1.0))
(define tv2 (make-vect 1.0 0.5))

(xcor-vect tv1)
(ycor-vect tv1)


; Операции над векторами --------------------------------------

; Сложение векторов
(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

; Вычитание векторов
(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))


; Умножение вектора на скаляр
(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

(add-vect tv1 tv2)
(sub-vect tv1 tv2)
(scale-vect 1.1 tv1)

; Упражнение 2.47
; Вариант с list
;(define (make-frame origin edge1 edge2)
;  (list origin edge1 edge2))

;(define (origin f)
;  (car f))

;(define (edge1 f)
;  (car (cdr f)))

;(define (edge2 f)
;  (car (cdr (cdr f))))

; Вариант с cons
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame f)
  (car f))

(define (edge1-frame f)
  (car (cdr f)))

(define (edge2-frame f)
  (cdr (cdr f))) ; Отличие от метода с list только тут


(define tf1 (make-frame 1.0 2.0 3.0))

(origin-frame tf1)
(edge1-frame tf1)
(edge2-frame tf1)


; Упражнеие 2.48
(define (make-segment v1 v2)
  (list v1 v2))

(define (start-segment v)
  (car v))

(define (end-segment v)
  (car (cdr v)))


; Рисовалки


(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))


(define (draw-line-2 v1 v2)
  (send dc draw-line
        (xcor-vect v1) (ycor-vect v1)
        (xcor-vect v2) (ycor-vect v2)))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line-2
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

; Упражнеие 2.49

(define line1 (segments->painter (list 
                                  (make-segment 
                                   (make-vect 0 1)
                                   (make-vect 1 0)))))

(define a-variant (segments->painter (list 
                                      (make-segment 
                                       (make-vect 0 0)
                                       (make-vect 1 0))
                                      (make-segment 
                                       (make-vect 1 0)
                                       (make-vect 1 1))
                                      (make-segment 
                                       (make-vect 1 1)
                                       (make-vect 0 1))
                                      (make-segment 
                                       (make-vect 0 1)
                                       (make-vect 0 0))
                                      )))

(define b-variant-x (segments->painter (list 
                                        (make-segment 
                                         (make-vect 0 0)
                                         (make-vect 1 1))
                                        (make-segment 
                                         (make-vect 1 0)
                                         (make-vect 0 1))
                                        )))


(define pwave (segments->painter (list 
                                  (make-segment 
                                   (make-vect 0.1 0)
                                   (make-vect 0.5 1))
                                  (make-segment 
                                   (make-vect 0.5 1)
                                   (make-vect 0.9 0))
                                  (make-segment 
                                   (make-vect 1 0.1)
                                   (make-vect 0 0.5))
                                  (make-segment 
                                   (make-vect 0 0.5)
                                   (make-vect 1 0.9))
                                  ;(make-segment 
                                  ; (make-vect 0.1 1)
                                  ; (make-vect 0.5 0))
                                  ;(make-segment 
                                  ; (make-vect 0.5 0)
                                  ; (make-vect 0.9 1))
                                  ;(make-segment 
                                  ; (make-vect 0 0.1)
                                  ; (make-vect 1 0.5))
                                  ;(make-segment 
                                  ; (make-vect 1 0.5)
                                  ; (make-vect 0 0.9))
                                  )))




(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))


(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)   ; новая исходная точка
                     (make-vect 1.0 1.0)   ; новый конец edge1
                     (make-vect 0.0 0.0))) ; новый конец edge2

(define frame-0 (make-frame (make-vect 0 0) (make-vect 10 450) (make-vect 400 0)))

(define pwave-1 (flip-vert pwave))


(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))


(define pwave-2 (shrink-to-upper-right pwave))


(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define pwave-3 (rotate90 pwave))

(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))


(define pwave-4 (squash-inwards pwave))


(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0
                                         0.0)
                              split-point
                              (make-vect 0.0
                                         1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0
                                         0.0)
                              (make-vect 0.5
                                         1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))




; Упражнение 2.50

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 0.0 0.25)
                     (make-vect 1.0 0.25)
                     (make-vect 0.0 0.75)))

(define pwave-5 (flip-horiz pwave))

(define (rotate-180 painter)
  (rotate90 (rotate90 painter)))

(define (rotate-270 painter)
  (rotate-180 (rotate90 painter)))

(define pwave-6 (rotate-180 pwave))


(define pwave-7 (rotate-270 pwave))



; Упражнение 2.51


(define (below-1 painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0
                                         0.0)
                              split-point                              
                              (make-vect 1.0
                                         0.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 0
                                         1)
                              (make-vect 1.0
                                         0.5))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))


(define pwave-8 (below-1 pwave pwave-7))

(define (below-2 painter1 painter2)
  (rotate90 (beside painter1 painter2)))


(define pwave-9 (below-2 pwave pwave-7))



(define pwave-00 (segments->painter (list                                   
                                     (make-segment 
                                      (make-vect 0.1 1)
                                      (make-vect 0.5 0))
                                     (make-segment 
                                      (make-vect 0.5 0)
                                      (make-vect 0.9 1))
                                     (make-segment 
                                      (make-vect 0 0.1)
                                      (make-vect 1 0.5))
                                     (make-segment 
                                      (make-vect 1 0.5)
                                      (make-vect 0 0.9))
                                     )))

; Упражнение 2.52
; a)
(define (merge painter1 painter2)
  (let ((split-point (make-vect 0.0 0.0))
        (v1-point (make-vect 0.0 1))
        (v2-point (make-vect 1 0.0)))
    (let ((paint-1
           (transform-painter painter1                              
                              split-point v1-point v2-point))
          (paint-2
           (transform-painter painter2                              
                              split-point v1-point v2-point)))
      (lambda (frame)
        (paint-1 frame)
        (paint-2 frame)))))


(define pwave-10 (merge pwave pwave-00))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below-1 smaller smaller)))))

(define pwave-11 (right-split pwave-10 5))


(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below-1 painter (beside smaller smaller)))))


(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below-1 right right))
              (corner (corner-split painter (- n 1))))
          (beside (below-1 painter top-left)
                  (below-1 bottom-right corner))))))


(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below-1 (flip-vert half) half))))



(define pwave-12 (beside pwave-10 pwave-10))

(define pwave-13 (below-1 pwave-12 pwave-12))


(send dc clear)
(pwave-13 frame-0)
(send target save-file "111.png" 'png)




