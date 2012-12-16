(define (f-xy x y)
  (if (or (= x 1) (= y 1)) 1
      (+ (f-xy (- x 1) y) (f-xy x (- y 1)))))

(define (f row num)
  (f-xy num (+ row (- num) 1)))

(f 5 2)

;(+ 4 (- 4) 1)