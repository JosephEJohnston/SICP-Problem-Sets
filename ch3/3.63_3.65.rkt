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
    (define (check-iter s n)
      (let ((cur-diff (abs (stream-car s))))
        (if (< cur-diff diff)
            n
            (check-iter (stream-cdr s) (+ n 1)))))
    (stream-ref stream (+ (check-iter diff-stream 0) 1))))

#|
(stream-limit (euler-transform pi-stream) 0.5)
|#

; 3.65, 2022/02/25, 参照上面 pi 的计算方式，计算逼近 2 的自然对数的三个序列
; ln 2 = 1 - 1/2 + 1/3 - 1/4 + ...

(define (ln n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln (+ n 1)))))

(define ln-stream
  (partial-sums (ln 1)))

(define ln-acc-stream
  (accelerated-sequence euler-transform ln-stream))

