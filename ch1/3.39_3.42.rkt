#lang sicp

(#%require "parallel.rkt")


; 由教材在后面的章节中实现
(define (make-serializer)
  (newline))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
            ((eq? m 'deposit) (protected deposit))
            ((eq? m 'balance) balance)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

; 3.39, 2022/02/09，如果我们换用下面的串行化执行
; 上面正文中所示的 5 种并行执行结果中的哪一些还可能出现？

(define x 10)

(define s (make-serializer))

(parallel-execute (lambda () (set! x ((s (lambda () (* x x))))))
                  (s (lambda () (set! x (+ x 1)))))
; 101 和 121

; 3.40, 2022/02/09, 请给出下面的执行可能产生出的所有 x 值：

(define x 10)

(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (* x x x))))
;; 100, 1000, 等等。包括各种乱序，以及读取时值被覆盖的情况

; 如果我们改用下面的串行化过程，上述可能性中的哪些还会存在：

(define x 10)

(define s (make-serializer))

(parallel-execute (s (lambda () (set! x (* x x))))
                  (s (lambda () (set! x (* x x x)))))
;; 100^3 和 1000^2

; 3.41, 2022/02/09，以下实现是否更好：
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
            ((eq? m 'deposit) (protected deposit))
            ((eq? m 'balance)
             ((protected (lambda () balance)))) ; 这里改了
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

; 不同意，外部用户读取到的 balance 要么是没修改的，要么是修改后的

; 3.42, 2022/02/09, 这样的修改安全吗？与原版本有什么不同？
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (let ((protected (make-serializer)))
    (let ((protected-withdraw (protected withdraw))
          (protected-deposit (protected deposit)))
      (define (dispatch m)
        (cond ((eq? m 'withdraw) protected-withdraw)
              ((eq? m 'deposit) protected-deposit)
              ((eq? m 'balance) balance)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m))))
      dispatch)))
; 看答案：https://sicp.readthedocs.io/en/latest/chp3/42.html
; Ben 的程序并不安全,而且会阻止单一对象进行并发。
;; ps：教材里也没说多个同一线程并发串行化会报错，还是有点奇怪




