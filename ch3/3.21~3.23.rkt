#lang sicp

(#%require "main.rkt")

; 3.21, 2022/01/27
; ((a) a) = (cons (cons 'a '()) (cons 'a '()))
#|
为什么 Ben 的例子产生出那样的输出结果:
delete-queue! 操作仅仅删除队列首个元素，并没有对队尾指针进行修改
|#

#|
(define (print-queue queue)
  (front-ptr queue))
|#

; 3.22, 2022/01/27
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))

    ; 检查队列是否为空
    (define (empty-queue?)
      (null? front-ptr))

    ; 返回队列前端的对象
    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called with an empty queue")
          (car front-ptr)))

    ; 将数据项插入队列末端
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
         (cond ((empty-queue?)
                (set! rear-ptr new-pair)
                (set! front-ptr new-pair))
               (else
                (set-cdr! rear-ptr new-pair)
                (set! rear-ptr (cdr rear-ptr))))))

    ; 删除队列前端的数据项
    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called with an empty queue"))
             (else
              (set! front-ptr (cdr front-ptr)))))

    ; 打印队列数据
    (define (print-queue)
      front-ptr)
    
    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) empty-queue?)
            ((eq? m 'front-queue) front-queue)
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) delete-queue!)
            ((eq? m 'print-queue) print-queue)
            (else (error "Undefined operation -- MAKE-QUEUE" m))))
    dispatch))

#|
; 自测通过
(define q1 (make-queue))
((q1 'insert-queue!) 'a)
((q1 'print-queue))
((q1 'insert-queue!) 'b)
((q1 'print-queue))
((q1 'insert-queue!) 'c)
((q1 'print-queue))
((q1 'insert-queue!) 'd)
((q1 'print-queue))
((q1 'delete-queue!))
((q1 'print-queue))
((q1 'delete-queue!))
((q1 'print-queue))
|#

