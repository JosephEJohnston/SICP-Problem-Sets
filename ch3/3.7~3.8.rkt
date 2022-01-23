#lang sicp

(#%require "main.rkt")

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

    (define (call-the-cops x)
      "call the cops")
    
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
    
    dispatch))

; 3.7, 2021/9/25
(define (make-joint account password new-password)
  (define (dispatch pw m)
    (if (eq? pw new-password)
        (account password m)
        "Incorrect password"))
  ; 是函数，外面就不要再加括号了
  dispatch)

#|
(define peter-acc (make-account 100 'open-sesame))

((peter-acc 'open-sesame 'withdraw) 40)

((peter-acc 'open-sesame 'deposit) 50)

(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))

((paul-acc 'rosebud 'withdraw) 40)
|#

; 3.8, 2022/1/22
; 要点在于，把变量值存在 f 内部
(define f
  (let ((count 0)
        (y 0))
    (lambda (x)
      (if (= 0 count)
        (begin (set! count 1)
               (set! y x)
               y)
        y))))





