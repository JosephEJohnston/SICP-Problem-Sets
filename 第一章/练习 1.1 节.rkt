
;练习1.3
(define (max x y)
  (cond ((> x y) x)
        (else y)))

(define (return-max-value x y z)
  (cond ((> x y) (max x z))
        (else (max y z))))


;练习1.5
(define (p) (p))

(define (test x y)
  (if (= x 0) 0
      y))

#|
(test 0 (p))
应用序时先求参数值，即对 0 和（p）求值，出现错误

正则序时先展开，即优先进行比较，由于 x = 0，返回 0
|#

;练习1.6
#|
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

发生段错误
在进行谓词比较之前优先进行参数求值，导致无限循环
|#

;练习1.7

;x是要猜的平方值
;真是不容易
#|
(define (sqrt-iter guess tguess x)
  (if (good-enough? guess tguess)     ;比较两者的比率
      guess                                ;改变量极小
      (sqrt-iter tguess (improve tguess x) x)));继续改善

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (sqrt-iter 1.0 (improve 1.0 x) x))

(define (good-enough? x y)
  (< (abs (- x y)) 0.001))
|#

;练习1.8
;照猫画虎
(define (cube x)
  (define (good-enough? guess x)
    (define (cubic x)
      (* x x x))
    (< (abs (- (cubic guess) x)) 0.001))
  (define (improve x y)
    (define (square x)
      (* x x))
    (/ (+ (* 2 y) (/ x (square y))) 3))
  (define (cube-iter guess x)
    (if (good-enough? guess x)
        guess
        (cube-iter (improve x guess) x)))
  (cube-iter 1.0 x))
