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
  (define ddy (delay (add-streams (scale-stream (force dy) a) (scale-stream y b))))
  y)

; https://zhuanlan.zhihu.com/p/35278737
#|

|#
; b = -1, a = 2 时,解为 C* e^x
#|
(define test (solve-2nd 2 -1 0.001 1 1 0.001))
(stream-ref test 0)
(stream-ref test 1)
(stream-ref test 2)
(stream-ref test 3)
(stream-ref test 4)
(stream-ref test 5)
(stream-ref test 1000)
(stream-ref test 2000)
|#

; 3.79, 2022/03/08，推广 solve-2nd
; 使之能求解一般的二次微分方程 d^2y/dt^2=f(dy/dt,y)
; https://www.zhihu.com/question/54563858
(define (solve-2nd-common f1 f2 dt y0 dy0 dy-divide-dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (delay (integral (delay ddy) dy0 dy-divide-dt)))
  (define ddy (delay (add-streams (stream-map f1 (force dy)) (stream-map f2 y))))
  y)

#|
; 我不知道自己写得是啥，就这样吧
(define test (solve-2nd-common (lambda (y) y) (lambda (y) y) 0.001 1 1 0.001))
(stream-ref test 0)
(stream-ref test 1)
(stream-ref test 2)
(stream-ref test 3)
(stream-ref test 4)
(stream-ref test 5)
(stream-ref test 1000)
(stream-ref test 2000)
|#

; 3.80, 2022/03/08, 实现串联 RLC 电路
; 那些方程用算算就行，肯定是成立的
(define (RLC R L C dt)
  (define (RLC-pairs vC0 iL0)
    (define vC (integral (delay dvC) vC0 dt))
    (define iL (integral (delay diL) iL0 dt))
    (define dvC (delay (scale-stream iL (- 0 (/ 1 C)))))
    (define diL (delay (add-streams
                        (scale-stream iL (- 0 (/ R L)))
                        (scale-stream vC (/ 1 L)))))
    (cons vC iL))
  RLC-pairs)

#|
; 测试通过
(define test-RLC (RLC 1 1 0.2 0.1))

(define test-RLC-pairs (test-RLC 10 0))

(define left-RLC-pairs (car test-RLC-pairs))

(define right-RLC-pairs (cdr test-RLC-pairs))

(stream-ref left-RLC-pairs 0)
(stream-ref left-RLC-pairs 1)
(stream-ref left-RLC-pairs 2)
(stream-ref left-RLC-pairs 3)
(stream-ref left-RLC-pairs 4)
(stream-ref right-RLC-pairs 0)
(stream-ref right-RLC-pairs 1)
(stream-ref right-RLC-pairs 2)
(stream-ref right-RLC-pairs 3)
(stream-ref right-RLC-pairs 4)
|#



