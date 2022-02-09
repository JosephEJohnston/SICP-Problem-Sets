#lang sicp

(#%require "constraints.rkt")


; 3.33, 2022/02/08，定义一个 average 过程

(define (average a b c)
  (let ((u (make-connector))
        (v (make-connector)))
    (adder a b u)
    (multiplier v c u)
    (constant 2 v)
    'ok))

#|
; 测试
(define A (make-connector))
(define B (make-connector))
(define C (make-connector))

(average A B C)

(probe "A" A)
(probe "B" B)
(probe "C" C)
|#

; 3.34, 2022/02/09，平方器的缺陷

; 缺陷：就现有实现而言，没法实现开平方——因为加法约束需要至少存在两个值才能求另一个值
#|
(define (squarer a b)
  (multiplier a a b))
|#


; 3.35， 2022/02/09，将平方定义为基本约束
(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0 -- SQUARER" (get-value b))
            (set-value! a (sqrt (get-value b)) me))
        (if (has-value? a)
            (set-value! b (* (get-value a) (get-value a)) me))))

  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))

  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- SQUARER" request))))

  (connect a me)
  (connect b me)
  me)

#|
; 测试
(define A (make-connector))
(define B (make-connector))

(squarer A B)

(probe "A" A)
(probe "B" B)

(set-value! B 36 'user)

(forget-value! B 'user)

(set-value! A 36 'user)
|#

; 3.36, 跳过

; 3.37, 2022/02/09, 定义模拟过程 c-、c*、c/ 和 cv（常量值）

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c- x y)
  (let ((z (make-connector)))
    (adder z y x)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier z y x)
    z))

(define (cv v)
  (let ((c (make-connector)))
    (constant v c)
    c))

; 实际上还是挺抽象的
(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

(define C (make-connector))
(define F (celsius-fahrenheit-converter C))

#|
; 测试

(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)

(set-value! C 25 'user)

(forget-value! C 'user)

(set-value! F 212 'user)
|#





