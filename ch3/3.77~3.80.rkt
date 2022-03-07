#lang sicp

(#%require "stream.rkt")
(#%require "infinite-stream.rkt")
(#%require "delay-sign-stream.rkt")

; 3.77, 2022/03/07, 修改这个过程
; 在用于带循环的系统时，这个过程有着与开始 integral 版本一样的问题
; 请修改这个过程，使它将 integrand 看作延时参数，以便能用于上述的 solve 过程

; 成功了可还行                

#|
(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
              (let ((integrand (force (force delayed-integrand))))
                (if (stream-null? integrand)
                    the-empty-stream
                    (integral (delay (delay (stream-cdr integrand)))
                              (+ (* dt (stream-car integrand))
                                 initial-value)
                              dt)))))

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (delay (stream-map f y)))
  y)

(stream-ref (solve (lambda (y) y) 1 0.001) 1000)

|#

