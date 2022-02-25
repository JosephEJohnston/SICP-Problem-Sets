#lang sicp

(#%require (only racket provide))
(#%require "infinite-stream.rkt")
(#%require "stream.rkt")

(provide average)
(define (average x y)
  (/ (+ x y) 2))

(provide sqrt-improve)
(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(provide sqrt-stream)
(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

; 会卡
; (display-stream (sqrt-stream 2))

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

; 以流 S 为参数，返回流中的元素是 S0，S0+S1，S0+S1+S2，...
(provide partial-sums)
(define (partial-sums s)
  (cons-stream (stream-car s) (add-streams (stream-cdr s) (partial-sums s))))

(provide pi-stream)
(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

#|
(stream-ref pi-stream 1)
(stream-ref pi-stream 2)
(stream-ref pi-stream 3)
(stream-ref pi-stream 4)
|#

(define (square x)
  (* x x))

(provide euler-transform)
(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

#|
(stream-ref (euler-transform pi-stream) 1)
(stream-ref (euler-transform pi-stream) 2)
(stream-ref (euler-transform pi-stream) 3)
(stream-ref (euler-transform pi-stream) 4)
|#

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(provide accelerated-sequence)
(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))
#|
(stream-ref (accelerated-sequence euler-transform pi-stream) 1)
(stream-ref (accelerated-sequence euler-transform pi-stream) 2)
(stream-ref (accelerated-sequence euler-transform pi-stream) 3)
(stream-ref (accelerated-sequence euler-transform pi-stream) 4)
(stream-ref (accelerated-sequence euler-transform pi-stream) 5)
|#
