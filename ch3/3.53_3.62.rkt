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
; 这种递归的程序栈类似于树型，画出来就好
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

; 应该是对的
(define S (cons-stream 1 (merge (scale-stream S 2)
                                (merge (scale-stream S 3) (scale-stream S 5)))))

; 3.57, 2022/02/15，当我们用基于 add-streams 过程的 fibs 定义计算出第 n 个斐波那契数时
; 需要执行多少次加法？
; 请证明，当我们没有使用记忆化优化时，那么所需的加法将会指数倍地增加

; 没记忆化的时候需要计算很多的重复子问题，和递归的问题一样


; 3.58, 2022/02/17, (expand 1 7 10) 会顺序产生出哪些元素？ (expand 3 8 10)?
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))


; 就是 num * radix / den 的整数部分，不知道有啥意义
; 生成基数内的所有整数，然后循环

; 3.59, 2022/02/17, 将幂级数表示为流
; a)
(define (integrate-series a)
  (define (integrate-series-iter a n)
    (cons-stream (* (stream-car a) (/ 1 n))
                 (integrate-series-iter (stream-cdr a) (+ n 1))))
  (integrate-series-iter a 1))

; b)
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1 (scale-stream (integrate-series sine-series) -1)))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))
; 绝了，这也行
; 相互递归


; 3.60, 2022/02/17，完成级数乘积过程的定义
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                            (mul-series (stream-cdr s1) s2))))

#|
; 测试
(define t1 (mul-series sine-series sine-series))

(define t2 (mul-series cosine-series cosine-series))

(define t3 (add-streams t1 t2))

(stream-ref t3 0)
(stream-ref t3 1)
(stream-ref t3 2)
(stream-ref t3 3)
|#



; 3.61, 2022/02/17, 写出一个过程，使它能对常数项为 1 的幂级数 S 计算出 1/S
; S * X = 1
; X = 1 - SR * X

; SR 是 S 常数项后面的部分
(define (X S)
  (cons-stream 1
               (scale-stream (mul-series (X S) (stream-cdr S)) -1)))

; 3.62, 2022/02/17, 定义 div-series
(define (div-series s1 s2)
  (let ((c (stream-car s2)))
    (if (= c 0)
        (error "Constant can not be zero -- DIV-SERIES")
        (mul-series s1 (X s2)))))

; https://zhuanlan.zhihu.com/p/413884031，是正确的
(define tangent-series (div-series sine-series cosine-series))

