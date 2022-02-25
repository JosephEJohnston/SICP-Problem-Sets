#lang sicp

(#%require "stream.rkt")
(#%require "infinite-stream.rkt")
(#%require "pairs-infinite-stream.rkt")

; 3.66, 2022/02/25, 检查流（pairs integers integers），描述序对的数量
(define integers-pairs (pairs integers integers))
; 大概是 2^n-1，(100, 100) 前是 2^100-1。（1，100）就不知道了

; 3.67，2022/02/25，修改过程 pairs
; 使（pairs integers integers） 能生成所有整数序对（i，j）的流（不考虑 i<=j，即包括 i>j 的部分）
; 还是挺难的
(define (pairs-all s t)
  (let ((row (stream-map (lambda (x) (list (stream-car s) x))
                         (stream-cdr t)))
        (col (stream-map (lambda (x) (list x (stream-car t)))
                         (stream-cdr s))))
    (cons-stream
     (list (stream-car s) (stream-car t))
     (interleave row (interleave col
                                 (pairs-all (stream-cdr s) (stream-cdr t)))))))


(define integers-pairs-all (pairs-all integers integers))

; 3.68, 2022/02/25，他的方法能行吗
#|

|#

(define (pairs-wrong s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x))
               t)
   (pairs-wrong (stream-cdr s) (stream-cdr t))))
; 变成（1，1）、（2，2）？ 直接卡炸了
; (define integers-pairs-wrong (pairs-wrong integers integers))
; 估计是因为 pairs-wrong 没有进行延时求值，看看答案（没答案了）

; 3.69，2022/02/25
; a)实现过程 triples
;; 以三个无穷流 S、T、U 为参数，生成三元组（Si, Tj, Uk) 的流
;; 其中 i<=j<=k
; b) 生成所有正的毕达哥拉斯三元组的流
;; 也就是说，生成所有的三元组（i，j，k），其中 i<=j，而且有 i^2+j^2=k^2

