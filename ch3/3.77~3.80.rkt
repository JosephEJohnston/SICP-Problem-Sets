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

; 3.78, 2022/03/07，现在考虑设计一个信号处理系统，研究齐次二阶线性微分方程
(define (solve-2nd a b dt y0 dy0 dy-divide-dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (delay (integral (delay ddy) dy0 dy-divide-dt)))
  (define ddy (delay (add-streams (scale-stream dy a) (scale-stream y b))))
  y)

#|
(define test (solve-2nd 1 1 0.001 1 1 0.001))
(stream-ref test 1)
|#




