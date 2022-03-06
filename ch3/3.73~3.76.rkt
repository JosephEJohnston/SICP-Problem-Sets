#lang sicp

(#%require "stream.rkt")
(#%require "infinite-stream.rkt")
(#%require "sign-stream.rkt")

; 3.73, 2022/03/06
; 模拟 RC 电路
; 按着图写就行
(define (RC R C dt)
  (define (v v-initial i)
    (add-streams 
       (scale-stream (integral i v-initial dt) (/ 1 C))
       (scale-stream i R)))
  v)

(define RC1 (RC 5 1 0.5))

#|
(define test-rc (RC1 1 integers))

(stream-ref test-rc 0)
(stream-ref test-rc 1)
(stream-ref test-rc 2)
(stream-ref test-rc 3)
(stream-ref test-rc 4)
|#

; 3.74, 2022/03/06, 描述输入信号过零点的信号
; 在输入信号从负值变成正值时这个结果信号应该是 +1
; 而当输入信号由正变负时它应该是 -1
; 其他时刻值位 0（0 输入的符号也假定为正）
(define (sign-change-detector before after)
  (cond ((and (> before 0) (< after 0)) -1)
        ((and (< before 0) (> after 0)) 1)
        (else 0)))

#|
(define (make-zero-crossings input-stream last-value)
  (cons-stream
   (sign-change-detector (stream-car input-stream) last-value)
   (make-zero-crossings (stream-cdr input-stream)
                        (stream-car input-stream))))
|#


(define sense-data ones)

; (define zero-crossings (make-zero-crossings sense-data 0))

; 填充这里缺少的 <expression>，完成这一程序
; stream-map 是练习 3.50 的推广版本
#|
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

|#

#|
; 解:
(define zero-crossings
  (stream-map sign-change-detector sense-data (stream-cdr sense-data)))
|#


; 3.75, 2022/03/06, 对信号做平滑，在提去过零点之前过滤掉噪声
; 先做每个感应值与前一感应值的平均值，而后在这样构造出的信号里提去过零点
; 请找出 Louis 留在其中的错误，改正它，但不要改变程序的结构
; 提示：你将需要增加 make-zero-crossings 的参数的个数

; 解：
#|
(define (make-zero-crossings input-stream last-value last-avpt)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    (cons-stream (sign-change-detector avpt last-avpt)
                 (make-zero-crossings (stream-cdr input-stream)
                                      (stream-car input-stream)
                                      avpt))))
|#

; 3.76, 2022/03/06, 实现 smooth
(define (smooth s)
  (let ((average (/ (+ (stream-car s) (stream-car (stream-cdr s))) 2)))
    (cons-stream average
                 (smooth (stream-cdr s)))))

(define (make-zero-crossings input-stream last-value)
  (cons-stream
   (sign-change-detector (stream-car input-stream) last-value)
   (make-zero-crossings (stream-cdr input-stream)
                        (stream-car input-stream))))

(define zero-crossings
  (make-zero-crossings (smooth sense-data) 0))

