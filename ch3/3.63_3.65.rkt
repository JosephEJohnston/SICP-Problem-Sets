#lang sicp

(#%require "stream.rkt")
(#%require "infinite-stream.rkt")
(#%require "stream-process.rkt")

; 3.63, 2022/02/24，为什么下面的过程过于低效
(define (sqrt-stream x)
  (cons-stream 1.0
               (stream-map (lambda (guess)
                             (sqrt-improve guess x))
                           (sqrt-stream x))))
; 没懂，可能是因为 sqrt-stream 作为过程会被求值
;; sqrt-stream 会两次展开，时间复杂度为 O(n^2)， 原版本为 O(n)（记忆化）
; 如果没用记忆化， 效率应该也没有差异
;; 无记忆化后无差异， 没用 guesses 变量导致没有记忆化
; 答案：https://sicp.readthedocs.io/en/latest/chp3/63.html


; 3.64, 2022/02/24, 实现 stream-limit
(define (stream-limit stream diff)
  (let ((diff-stream (add-streams stream
                                  (stream-cdr (scale-stream stream -1)))))
    (define (check-iter n s)
      (let ((cur-diff (abs (stream-car diff-stream))))
        (if (< cur-diff diff)
            n
             (check-iter (+ n 1) (stream-cdr s)))))
    (stream-ref stream (check-iter 0 stream))))

#|
; 还是有点奇怪
(stream-limit (euler-transform pi-stream) 0.5)
|#
