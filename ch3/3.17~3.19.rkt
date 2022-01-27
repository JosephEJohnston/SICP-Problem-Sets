#lang sicp

; 3.16，仅供错误示例

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

#|
; 3:
(define x (cons 1 2))
(define y (cons 3 4))
(define z (cons x y))
|#

#|
; 4:
(define x (cons 1 nil))
(define y (cons x (cons x nil)))
|#

#|
; 7:
; https://sicp.readthedocs.io/en/latest/chp3/16.html
(define one (list 1))
(define three (cons one one))
(define seven (cons three three))
|#

#|
; 不返回：
(define crycle (cons 1 (cons 2 (cons 3 '()))))
(set-cdr! (last-pair cycle) cycle)
(count-pairs cycle)
|#

; 3.17, 2022/01/26
;(define (count-pairs x)
; (x))

