#lang sicp

(#%require "stream.rkt")
(#%require "infinite-stream.rkt")
(#%require (only racket provide))

(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 ; 是的，force 两次
                 (let ((integrand (force (force delayed-integrand))))
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))

  ; dy 必须 delay，否则 y 会在 undefined 的情况下提前求值，导致报错
  (define dy (delay (stream-map f y)))
  y)

; 计算 e 的近似值
; (stream-ref (solve (lambda (y) y) 1 0.001) 1000)
