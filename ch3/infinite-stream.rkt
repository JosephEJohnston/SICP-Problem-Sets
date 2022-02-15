#lang sicp

(#%require (only racket require))
(#%require (only racket provide))
(#%require "stream.rkt")


(define ones (cons-stream 1 ones))

(provide add-streams)
(define (add-streams s1 s2)
  (stream-map + s1 s2))

(provide integers)
(define integers (cons-stream 1 (add-streams ones integers)))

(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fibs)
                                         fibs))))

(provide scale-stream)
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define double (cons-stream 1 (scale-stream double 2)))

(define primes
  (cons-stream
   2
   (stream-filter prime? (integers-starting-from 3))))

(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) true)
          ((divisible? n (stream-car ps)) false)
          (else (iter (stream-cdr ps)))))
  (iter primes))

(define (square x)
  (* x x))
; 上面的内容是整体前移过来的，因为定义需要顺序

; 正整数流
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

; (define integers (integers-starting-from 1))



; 不能被 7 整除的整数的流
(define (divisible? x y) (= (remainder x y) 0))

(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))

; 斐波那契数的无穷流
(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))

; (define fibs (fibgen 0 1))

; 厄拉多塞筛法
(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))

; (define primes (sieve (integers-starting-from 2)))



