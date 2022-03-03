#lang sicp

(#%require "stream.rkt")
(#%require "infinite-stream.rkt")
(#%require "pairs-infinite-stream.rkt")

; 3.66, 2022/02/25, 检查流（pairs integers integers），描述序对的数量
(define integers-pairs (pairs integers integers))
; 大概是 2^n-1，(100, 100) 前是 2^100-1。（1，100）就不知道了

; 3.67，2022/02/25，修改过程 pairs
; 使（pairs integers integers） 能生成所有整数序对（i，j）的流（不考虑 i<=j，即包括 i>j 的部分）
; 还是挺难的
(define (pairs-all s t)
  (let ((row (stream-map (lambda (x) (list (stream-car s) x))
                         (stream-cdr t)))
        (col (stream-map (lambda (x) (list x (stream-car t)))
                         (stream-cdr s))))
    (cons-stream
     (list (stream-car s) (stream-car t))
     (interleave row (interleave col
                                 (pairs-all (stream-cdr s) (stream-cdr t)))))))


(define integers-pairs-all (pairs-all integers integers))

; 3.68, 2022/02/25，他的方法能行吗
(define (pairs-wrong s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x))
               t)
   (pairs-wrong (stream-cdr s) (stream-cdr t))))
; 变成（1，1）、（2，2）？ 直接卡炸了
; (define integers-pairs-wrong (pairs-wrong integers integers))
; 估计是因为 pairs-wrong 没有进行延时求值，看看答案（没答案了）

; 3.69，2022/02/25
; a)实现过程 triples
;; 以三个无穷流 S、T、U 为参数，生成三元组（Si, Tj, Uk) 的流
;; 其中 i<=j<=k
; b) 生成所有正的毕达哥拉斯三元组的流
;; 也就是说，生成所有的三元组（i，j，k），其中 i<=j，而且有 i^2+j^2=k^2

; a)
; 麻了，难度不小
(define (triples s t u)
  (let ((left-u (stream-map (lambda (x) (list (stream-car s) (stream-car t) x))
                            (stream-cdr u)))
        (left-t-u (stream-map (lambda (x) (cons (stream-car s) x))
                              (pairs (stream-cdr t) (stream-cdr u)))))
    (cons-stream
     (list (stream-car s) (stream-car t) (stream-car u))
     (interleave left-u (interleave left-t-u
                                    (triples (stream-cdr s)
                                             (stream-cdr t)
                                             (stream-cdr u)))))))

(define integers-triples (triples integers integers integers))
#|
(define integers-triples (triples integers integers integers))
(stream-ref integers-triples 0)
(stream-ref integers-triples 1)
(stream-ref integers-triples 2)
(stream-ref integers-triples 3)
(stream-ref integers-triples 4)
(stream-ref integers-triples 5)
|#

; 暂用
(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

; b) pythagoras
(define (pythagoras-triples s t u)
  (let ((this-triples (triples s t u)))
    (define (square x)
      (* x x))
    ; check-square 写错会一直求值
    (define (check-square tri)
      (= (+ (square (car tri)) (square (car (cdr tri)))) (square (car (cdr (cdr tri))))))
    (stream-filter (lambda (tri) (check-square tri)) this-triples)))

#|
(define integers-pythagoras-triples
  (pythagoras-triples integers integers integers))

(stream-ref integers-pythagoras-triples 0)
(stream-ref integers-pythagoras-triples 1)
(stream-ref integers-pythagoras-triples 2)
; 往下就非常慢了
; (stream-ref integers-pythagoras-triples 3)
; (stream-ref integers-pythagoras-triples 4)
|#


; 3.70, 2022/03/01, 实现权重函数
; 其中 weight 是用于计算序对权重的过程，用于确定元素在归并所产生的流中出现的顺序

; 注意 stream-cdr 的使用。当在 let 中使用时，会提前求值
(define (merge-weighted s1 s2 weight)
  (let ((cur-s1 (stream-car s1))
        (cur-s2 (stream-car s2)))
    (cond ((< (weight cur-s1) (weight cur-s2))
           (cons-stream cur-s1
                        (merge-weighted (stream-cdr s1) s2 weight)))
          ((> (weight cur-s1) (weight cur-s2))
           (cons-stream cur-s2
                        (merge-weighted s1 (stream-cdr s2) weight)))
          (else
           (cons-stream cur-s1
                        (cons-stream cur-s2
                                     (merge-weighted (stream-cdr s1) (stream-cdr s2) weight)))))))


(define (weighted-pairs s1 s2 weight)
  (let ((cur-car (list (stream-car s1) (stream-car s2))))
    (cons-stream cur-car
                 (merge-weighted (stream-map (lambda (x) (list (stream-car s1) x))
                                             (stream-cdr s2))
                                 (weighted-pairs
                                  (stream-cdr s1)
                                  (stream-cdr s2)
                                  weight)
                                 weight))))


; a)

(define (sum-weight pair)
  (let ((first (car pair))
        (second (car (cdr pair))))
    (+ first second)))

(define sum-pairs (weighted-pairs integers integers sum-weight))

;(display-stream sum-pairs)

#|
(stream-ref sum-pairs 0)
(stream-ref sum-pairs 1)
(stream-ref sum-pairs 2)
(stream-ref sum-pairs 3)
(stream-ref sum-pairs 4)
(stream-ref sum-pairs 5)
(stream-ref sum-pairs 6)
(stream-ref sum-pairs 7)
|#

; b)

(define (prime-product-weight pair)
  (let ((first (car pair))
        (second (car (cdr pair))))
    (+ (* 2 first) (* 3 second) (* 5 first second))))

(define (check-pair pair)
  (define (check i)
    (and (= 0 (remainder i 2))
         (= 0 (remainder i 3))
         (= 0 (remainder i 5))))
    
  (and (check (car pair))
       (check (car (cdr pair)))))

(define prime-product-pairs
  (stream-filter check-pair
                 (weighted-pairs integers integers prime-product-weight)))

#|
(stream-ref prime-product-pairs 0)
(stream-ref prime-product-pairs 1)
(stream-ref prime-product-pairs 2)
(stream-ref prime-product-pairs 3)
(stream-ref prime-product-pairs 4)
(stream-ref prime-product-pairs 5)
(stream-ref prime-product-pairs 6)
(stream-ref prime-product-pairs 7)
|#

