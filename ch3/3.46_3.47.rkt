#lang sicp

(#%require "parallel.rkt")

; 3.46, 2022/02/11, 若没有原子化实现 test-and-set！会出什么问题
; 两个进程都在 set 那个地方，也就是都拿到了锁


; 3.47, 2022/02/11，实现（大小为 n）的信号量，其是允许被 n 个进程获取的互斥元
; 写个表，这几个操作也不是串行化的，需要串行化才能保证正确性
(define (make-list n default)
  (define (make-list-iter i default)
    (if (= 1 i)
        (cons default '())
        (cons default (make-list-iter (- i 1) default))))
  (make-list-iter n default))

(define (get-nth list n)
  (define (get-nth-iter list i)
    (if (= 0 i)
        (car list)
        (get-nth-iter (cdr list) (- i 1))))
  (get-nth-iter list n))

(define (set-nth! list n value)
  (define (set-nth-iter list i value)
    (if (= 0 i)
        ; 把这个改成 test-and-set！就满足题目要求
        (set-car! list value)
        (set-nth-iter (cdr list) (- i 1) value)))
  (set-nth-iter list n value))

; a)基于互斥元实现
(define (make-multi-mutex n)
  (let ((mutex-list (make-list n (make-mutex))))

    (define (acquire ith)
      ((get-nth mutex-list ith) 'acquire))

    (define (release ith)
      ((get-nth mutex-list ith) 'release))

    (define (dispatch m)
      (cond ((eq? m 'acquire) acquire)
            ((eq? m 'release) release)))

    dispatch))

; b)基于原子的 test-and-set! 操作实现
; 还没实现自动化分配第 i 个信号量
(define (make-mutex n)
  (let ((cell-list (make-list n false)))
    (define (acquire ith)
      (if (get-nth cell-list ith)
          ((the-mutex 'acquire) ith)
          (begin
            (set-nth! cell-list ith false)
            false)))

    (define (release ith)
      (set-nth! cell-list ith false))
    
    (define (the-mutex m)
      (cond ((eq? m 'acquire) acquire)
            ((eq? m 'release) release)))
    the-mutex))

;; 上面只是实现了个大概
        
