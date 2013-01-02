#lang racket
(require (lib "racket/draw"))
(require racket/class)

(define target (make-bitmap 500 500))
(define dc (new bitmap-dc% [bitmap target]))


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

; Упражнение 2.46
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


(define pvawe (segments->painter (list 
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








(send dc clear)
(pvawe (make-frame (make-vect 0 0) (make-vect 10 450) (make-vect 400 0)))
(send target save-file "111.png" 'png)




