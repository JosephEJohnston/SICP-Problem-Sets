#lang sicp

(#%require (only racket provide))

(#%require (only racket thread-wait))
(#%require (only racket thread))

; 并行执行每个过程，过程必须无参
(provide parallel-execute)
(define (parallel-execute . procs)
  (map thread-wait
       (map (lambda (proc) (thread proc))
            procs)))

; 串行化在后面的章节实现

#|

(define x 10)

(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (+ x 1))))

|#

; 检查单元并返回检查结果
; 必须以原子操作的方式执行
(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))

(define (clear! cell)
  (set-car! cell false))

; 循环检查互斥元是否可用
(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))

; 每个串行化组关联着一个互斥元
; 给定一个过程 p，串行化组将返回一个过程，该过程将获取相应互斥元，而后运行 p，而后释放互斥元
(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))


