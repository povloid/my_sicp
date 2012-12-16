(define (average y x)
  (/
   (+
    (/ x (* y y))
    (* 2 y)
    )
   3)
  )


(define (loop? og g)
  (> og g)
  )


(define (kube-iter og g x)
  (if (loop? og g)
      (kube-iter g (average g x) x)
      g
      )
  )


(define (kube x) 
  (kube-iter (+ x 1) x x))
(kube 666.0)
;;(average 8.8 8.8)
;;(average 18.012345679012345 27)
(* 8.732891741295967 (* 8.732891741295967 (* 8.732891741295967)))



;;(average 4.5 27)
;;(average 3.4444444444444446 27)
;;(average 3.054881103788492 27)
;;(average 3.0009800378224583 27)
;;(average 3.0000003200186494 27)
;;(average 3.000000000000034 27)
;;(average 3.0 27)