#lang sicp

(#%require "stream.rkt")
(#%require "infinite-stream.rkt")
(#%require "monte-carlo-stream.rkt")

; 3.81, 2022/03/11, 实现随机数生成器的流模型
; 解中不要使用赋值

(define (rand-generator stream)
  (define (generate-rand s)
    (define (next)
      (cons-stream (rand-update (stream-car s))
                   (generate-rand (stream-cdr s))))
    (next))

  (define (reset-rand value)
    (value))
  
  (define (dispatch m)
    (cond ((eq? m 'generate-rand) (generate-rand stream))
          ((eq? m 'reset-rand) reset-rand)
          (else (error "Unknown Operation Error -- rand-generator"))))
  dispatch)

(define test (rand-generator integers))
((test 'generate-rand))


           

