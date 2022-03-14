#lang sicp

(#%require "stream.rkt")
(#%require "infinite-stream.rkt")
(#%require "monte-carlo-stream.rkt")

; 3.81, 2022/03/11, 实现随机数生成器的流模型
; 解中不要使用赋值

; 题目看了老半天，不知道要干啥
(define (rand-generator stream)
  (define (generate-rand)
    (cons-stream (rand-update (stream-car stream))
                 (stream-map rand-update (stream-cdr stream))))

  (define (reset-rand s)
    (rand-generator s))
  
  (define (dispatch m)
    (cond ((eq? m 'generate-rand) generate-rand)
          ((eq? m 'reset-rand) reset-rand)
          (else (error "Unknown Operation Error -- rand-generator"))))
  dispatch)

#|
(define test (rand-generator integers))
(stream-ref ((test 'generate-rand)) 0)
(stream-ref ((test 'generate-rand)) 1)
(stream-ref ((test 'generate-rand)) 2)

(define test-2 ((test 'reset-rand) (add-streams integers integers)))
(stream-ref ((test-2 'generate-rand)) 0)
(stream-ref ((test-2 'generate-rand)) 1)
(stream-ref ((test-2 'generate-rand)) 2)
|#




