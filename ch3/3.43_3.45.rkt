#lang sicp

(#%require "parallel.rkt")

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

#|
; 这仅仅是个例子
(define (deposit account amount)
  (let ((s (account 'serializer))
        (d (account 'deposit)))
    ((s d) amount)))
|#


(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange))
     account1
     account2)))

; 3.43, 2022/02/11，跳过

; 3.44, 2022/02/11， 他的方法是对的吗？
; 如果不对，在转移问题和交换问题之间存在着什么本质性的不同？

; 下面这个程序是安全的。exchange 问题中存在一个中间变量 difference，存在同步问题
(define (transfer from-account to-account amount)
  ((from-account 'withdraw) amount)
  ((to-account 'deposit) amount))

; 答案：https://sicp.readthedocs.io/en/latest/chp3/44.html
; 即使上述程序在并发上是安全的，也存在执行过程中系统奔溃的问题要考虑，需要使用事务系统

; 3.45, 2022/02/11
; 下述程序为什么是错的，特别是考虑在调用 serialized-exchange 时会发生什么情况
(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (balance-serializer withdraw))
            ((eq? m 'deposit) (balance-serializer deposit))
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

(define (deposit account amount)
  ((account 'deposit) amount))

; 调用 serialized-exchange 时会报错，因为 exchange 过程和 withdraw 过程不能同时执行

; 答案：https://sicp.readthedocs.io/en/latest/chp3/45.html
; 主要问题是两个过程在同一个串行化组中


