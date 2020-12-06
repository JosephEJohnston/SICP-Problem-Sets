#lang sicp

(#%require (only racket provide))



; 3.1.1
(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

#|
(define acc (make-account 100))

((acc 'withdraw) 50)

((acc 'withdraw) 60)

((acc 'deposit) 40)

((acc 'withdraw) 60)
|#

; 3.1.2

; rand-update 抄的是数据结构与算法分析——C语言描述 10.4.1 节

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

(provide rand)
(define rand-acc
  (let ((x 1))
    (lambda ()
      (set! x (rand-update x))
      x)))

(define (rand)
  (floor (rand-acc)))


(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))


(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(provide monte-carlo)
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

