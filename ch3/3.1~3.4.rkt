#lang sicp

(#%require "main.rkt")

; 3.1 累加器
(define (make-accumulator value)
  (lambda (amount)
    (begin (set! value (+ amount value))
           value)))

#|
(define A (make-accumulator 5))

(A 10)

(A 10)
|#

; 3.2 统计次数

(define (make-monitored fun)
  (let ((times 0))
    (define (dispatch mf)
      (cond ((eq? mf 'how-many-calls?) times)
            ((eq? mf 'reset-count) (set! times 0))
            (else (begin (set! times (+ times 1))
                         (fun mf)))))
    dispatch))

#|
(define s (make-monitored sqrt))

(s 100)

(s 100)

(s 'how-many-calls?)

(s 'reset-count)

(s 'how-many-calls?)
|#

; 3.3 带密码保护的账户
#|
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pw m)
    (if (eq? pw password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m)))
        ; 带个 lambda 把语句变为过程
        (lambda x "Incorrect password")))
  dispatch)
|#

#|
(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)

((acc 'some-other-password 'deposit) 50)

((acc 'secret-password 'withdraw) 40)
|#

; 3.4 警报功能
(define (make-account balance password)
  (let ((visit-times 0))
    
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    
    (define (dispatch pw m)
      (if (eq? pw password)
          (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                (else (error "Unknown request -- MAKE-ACCOUNT"
                             m)))
          ; 带个 lambda 把语句变为过程
          (if (= visit-times 7)
              call-the-cops
              (begin
                (set! visit-times (+ visit-times 1))
                (lambda x "Incorrect password")))))
    
    (define (call-the-cops x)
      "call the cops")
    
    dispatch))

(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)

((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)

((acc 'secret-password 'withdraw) 40)