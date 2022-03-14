#lang sicp

(#%require "stream.rkt")
(#%require "infinite-stream.rkt")
(#%require (only racket provide))

(define random-init 1)

(provide rand-update)
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
      (floor (random)))))

; 这个随机数需要是整数
(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

(provide random-numbers)
(define random-numbers
  (cons-stream random-init
               (stream-map rand-update random-numbers)))

(define (map-successive-pairs f s)
  (cons-stream
   (f (stream-car s) (stream-car (stream-cdr s)))
   (map-successive-pairs f (stream-cdr (stream-cdr s)))))

(define cesaro-stream
  (map-successive-pairs (lambda (r1 r2) (= (gcd r1 r2) 1))
                        random-numbers))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define pi
  (stream-map (lambda (p) (sqrt (/ 6 p)))
              (monte-carlo cesaro-stream 1 1)))



