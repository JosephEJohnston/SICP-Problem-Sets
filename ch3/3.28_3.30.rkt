#lang sicp

; 本节还不需要给出数字电路的具体实现，伪代码即可

; 返回连线上信号的当前值
(define (get-signal wire)
  (newline))

; 将连线上的信号修改为新的值
(define (set-signal! wire value)
  (newline))

; 只要连线上的信号值改变，这里所指定过程就需要运行
(define (add-action! wire procedure)
  (newline))

; 在 delay 之后执行 procedure
(define (after-delay delay procedure)
  (newline))

; 逻辑否
(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

; 反门
(define (inverter input output)
  (define (invert-input)
    ; 对 input 线上取到的信号值取逻辑否
    (let ((new-value (logical-not (get-signal input))))
      ; 在 inverter-delay 后，在 output 线上设置新值
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))

  ; 在 input 线上加上 invert-input 过程
  (add-action! input invert-input)
  'ok)

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)


; 3.28, 2022/02/07，将或门定义为一个基本功能块
(define (or-gate o1 o2 output)
  (define (or-action-procedure)
    (let  ((new-value
            (logical-or (get-signal o1) (get-signal o2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))

  (add-action! o1 or-action-procedure)
  (add-action! o1 or-action-procedure)
  'ok)

; 3.29, 2022/02/07, 用与门和反门构造出或门
; 0->1, 1->0
; 11->1, 01->0, 10->0, 00->0
; 11->1, 01->1, 10->1, 00->0
(define (or-gate o1 o2 output)
  

