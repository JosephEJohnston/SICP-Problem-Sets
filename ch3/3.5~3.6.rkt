#lang sicp

(#%require "main.rkt")

; 3.5 蒙特卡洛积分
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (Predicate x1 x2 y1 y2)
  (let ((ox (/ (+ x1 y1) 2))
        (oy (/ (+ x2 y2) 2))
        (r (min (/ (- y1 x1) 2) (/ (- y2 x2) 2))))
    (define (square x)
      (* x x))
    (define (compute x y)
      (+ (square (- x ox)) (square (- y oy))))
    (<= (compute (random-in-range x1 y1)
                 (random-in-range x2 y2))
        (square r))))

(define (estimate-integral P x1 x2 y1 y2 times)
  (monte-carlo times (lambda () (P x1 x2 y1 y2))))

(define (estimate-pi x1 x2 y1 y2 times)
  (let ((circle (* (estimate-integral Predicate x1 x2 y1 y2 times)
                   (* (- y1 x1) (- y2 x2))))
        (r (/ (min (- y1 x1) (- y2 x2)) 2)))
    (/ circle (* r r))))


;; 这个方法的精度很低，并且在矩形范围小的时候精度会下降
;(estimate-pi 0 0 1000 1000 1000000)

; 3.6 支持重置的 rand
(define (rand-update seed)
  (let ((A 48271)
        (M 2147483647))
    (let ((Q (floor (/ M A)))
          (R (remainder M A)))
      (define (random)
        (let ((tmp-seed (- (* A (remainder (floor seed) Q))
                           (* R (/ (floor seed) Q)))))
          (if (>= tmp-seed 0)
              tmp-seed
              (+ tmp-seed M))))
      (random))))

(define rand
  (let ((x 1))
    (define reset
      (lambda (n) (set! x n)))
    (define generate
      (lambda () (set! x (rand-update x)) (floor x)))
    (lambda (order) (cond ((eq? order 'reset) reset)
                          ((eq? order 'generate) (generate))
                          (else (error "Unknown Error -- rand-acc"))))))

#|
函数和符号定义的区别：每次进入函数都会执行 let，符号定义只在第一次进入时调用 let
(define (rand order)
  (let ((x 1))
    (lambda () (set! x (rand-update x)) (floor x))))


(define rand-test
  (let ((x 1))
    (lambda () (set! x (rand-update x)) (floor x))))

(rand-test)
(rand-test)
(rand-test)
(rand-test)
(rand-test)
(rand-test)

(rand 'generate)
(rand 'generate)
(rand 'generate)
((rand 'reset) 10)
(rand 'generate)
(rand 'generate)
(rand 'generate)
|#


