#lang sicp

(#%require "stream.rkt")

; 3.50, 2022/02/14, 完成定义
; 这个过程是 stream-map 的推广，它允许过程带有多个参数
#|
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map car argstreams))
       (apply stream-map
              (cons proc (map cdr argstreams))))))

|#

; 3.51，2022/02/14，解释器对于顺序地求值下面各个表达式的响应是什么？
#|
(define (show x)
  (display-line x)
  x)

(define x (stream-map show (stream-enumerate-interval 0 10)))

; 打印 0 到 5
(stream-ref x 5)

; 打印 0 到 7
; 实际上只打印了 6,7,因为有记忆化
(stream-ref x 7)
|#



; 3.52, 2022/02/14，考虑下面的表达式序列
; 在上面每个表达式求值之后 sum 的值是什么？
;; 不容易算，表示要执行和实际执行的过程不同。y 中只执行一部分，z 中也只执行一部分
;; (stream-ref y 7) 后为 136
;; (display-stream z) 后为 210

; 求值其中的 stream-ref 和 display-stream 表达式将打印出什么响应？
;; stream-ref 返回 136； display-stream 打印了以 5 为倍数的值

; 如果我们简单地将 (delay <exp>) 实现为 (lambda () <exp>)
; 而不使用 memo-proc 所提供的优化，这些响应会有什么不同吗？
;; 会有不同，一些记忆化过的值会再次求值


#|
(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))

(define y (stream-filter even? seq))

(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))

(stream-ref y 7)

(display-stream z)
|#





