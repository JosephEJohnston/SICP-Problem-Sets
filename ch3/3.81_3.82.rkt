#lang sicp

(#%require "stream.rkt")
(#%require "infinite-stream.rkt")
(#%require "monte-carlo-stream.rkt")

; 3.81, 2022/03/11, 实现随机数生成器的流模型
; 解中不要使用赋值

; 题目看了老半天，不知道要干啥
(define (rand-generator init-value)
  (define (generate-rand)
    (cons-stream (rand-update init-value)
                 (stream-map rand-update (generate-rand))))

  (define (reset-rand i)
    (rand-generator i))
  
  (define (dispatch m)
    (cond ((eq? m 'generate-rand) generate-rand)
          ((eq? m 'reset-rand) reset-rand)
          (else (error "Unknown Operation Error -- rand-generator"))))
  dispatch)

#|
(define test (rand-generator 1))
(stream-ref ((test 'generate-rand)) 0)
(stream-ref ((test 'generate-rand)) 1)
(stream-ref ((test 'generate-rand)) 2)

(define test-2 ((test 'reset-rand) 2))
(stream-ref ((test-2 'generate-rand)) 0)
(stream-ref ((test-2 'generate-rand)) 1)
(stream-ref ((test-2 'generate-rand)) 2)
|#


; 3.82, 2022/03/14, 以流的方式重新做练习 3.5 里的蒙特卡罗积分
; estimate-integral 的流版本将不需要参数告知执行试验的次数
; 相反，它将生成一个表示越来越多试验次数的估值流

