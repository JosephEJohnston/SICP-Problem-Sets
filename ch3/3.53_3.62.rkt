#lang sicp

(#%require "infinite-stream.rkt")
(#%require "stream.rkt")


; 3.53, 2022/02/15
; 请不要运行程序，描述一下由下面程序定义出的流里的元素
(define s (cons-stream 1 (add-streams s s)))

; 2 的次方流

; 3.54, 2022/02/15
; 请定一个一个与 add-streams 类似的过程 mul-streams
; 对于两个输入流，它按元素逐个生成乘积
; 用它和 integers 流一起完成下面流的定义，其中第 n 个元素（从 0 开始）是 n + 1 的阶乘

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

; 画下草稿吧。
(define factorials (cons-stream 1 (mul-streams (stream-cdr integers) factorials)))


; 3.55, 2022/02/15，定义函数 partial-sums
(define (partial-sums s)
  (cons-stream (stream-car s) (add-streams (stream-cdr s) (partial-sums s))))

; 3.56，2022/02/15， 按照递增顺序不重复地枚举出所有满足条件的整数
; 填空
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))

(define S (cons-stream 1 (merge (scale-stream S 2)
                                (merge (scale-stream S 3) (scale-stream S 5)))))



