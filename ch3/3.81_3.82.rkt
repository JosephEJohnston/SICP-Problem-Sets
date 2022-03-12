#lang sicp

(#%require "monte-carlo-stream.rkt")

; 3.81, 2022/03/11, 实现随机数生成器的流模型
; 解中不要使用赋值

(define (rand-generator stream)

  (define random-numbers
    (cons-stream (rand-update (stream-car stream))
                 (stream-map rand-update (stream-cdr stream))))

  (define (generate-rand)
    (stream-car random-numbers
    
  
  (define (dispatch m)
    (cond ((eq? m 'generate-rand) generate-rand)
          ((eq? m 'reset-rand) reset-rand)
          (else (error "Unknown Operation Error -- rand-generator")))))


           

