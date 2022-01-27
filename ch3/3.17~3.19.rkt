#lang sicp

; 3.16，仅供错误示例

#|
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))
|#


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
; 下面几题都不会，看答案了。
#|
我们可以通过维持一个记录列表,然后遍历给定的序对结构,
每当遇到一个序对时,判断它是否已经存在于记录列表,
如果不存在就将它加进记录列表,并继续遍历这个序对的 car 和 cdr 部分,
当给定的序对结构遍历完之后,记录列表的长度就是序对的真正个数。

inner 的定义中使用了内置函数 memq ,用于检查给定序对是否存在于记录列表内。
|#
(define (count-pairs x)
    (length (inner x '())))

(define (inner x memo-list)
    (if (and (pair? x)
             (eq? false (memq x memo-list)))
        (inner (car x)
               (inner (cdr x)
                      (cons x memo-list)))
        memo-list))

; 3.18, 3.19, 2022/01/27
; 快慢指针
(define (loop? lst)
    (define (iter x y)
        (let ((x-walk (list-walk 1 x))
              (y-walk (list-walk 2 y)))
            (cond ((or (null? x-walk) (null? y-walk))
                    #f)
                  ((eq? x-walk y-walk)
                    #t)
                  (else
                    (iter x-walk y-walk)))))
    (iter lst lst))

(define (list-walk step lst)
    (cond ((null? lst)
            '())
          ((= step 0)
            lst)
          (else
            (list-walk (- step 1)
                       (cdr lst)))))
