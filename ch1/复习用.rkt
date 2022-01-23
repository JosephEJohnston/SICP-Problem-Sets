(define (fixed-point f guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (if (close-enough? guess (f guess))
      guess
      (fixed-point f (f guess))))
;不动点

(define x^x (lambda (x) (/ (log 1000) (log x))))
;函数

(define (average x y)
  (/ (+ x y) 2))
;平均值

(define ave-x^x (lambda (x) (average x (/ (log 1000) (log x)))))
;平均阻尼化函数

(define guess-x^x
  (fixed-point ave-x^x 2))
;求根

guess-x^x
  