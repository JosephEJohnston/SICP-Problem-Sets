#lang sicp

(#%require "main.rkt")

; 3.24, 2022/01/28
(define (make-table same-key?)
  (let ((local-table (list '*table*)))

    ; 查找 records 中是否存在对应的 key
    (define (assoc key records)
      (cond ((null? records) false)
        ((same-key? key (caar records)) (car records))
        (else (assoc key (cdr records)))))
    
    ; 用 key-1 查找子表格，用 key-2 在这个子表格里确定记录
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))

    ; 在 key-1 子表格中，插入名为 key-2 的记录
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            ; 存在子表，则插入旧表
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            ; 不存在，初始化并插入新子表和值
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)

    (define (print)
      local-table)

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'print-proc) print)
            (else (error "Unknown operation -- TABLE" m))))

    dispatch))

; 测试
#|
(define test-table (make-table equal?))

((test-table 'insert-proc!) 'math '+ 43)
((test-table 'insert-proc!) 'math '- 45)
((test-table 'insert-proc!) 'math '* 42)

((test-table 'insert-proc!) 'letters 'a 97)
((test-table 'insert-proc!) 'letters 'b 98)

((test-table 'print-proc))
|#

; 3.25, 2022/01/28
; 推广一维表格和二维表格的概念
; 应该是递归吧，有几个关键码走几层

; 3.26, 2022/01/28
; 请描述一种表格实现，其中的 (key, value) 记录用二叉树形式组织起来
#|
|l|r|value|
|#

(define (node value)
  (let ((left '())
        (right '()))

    (define (left-node)
      left)

    (define (right-node)
      right)

    (define (get-value)
      value)
    
    (define (set-left-node! value)
      (set! left (node value)))

    (define (set-right-node! value)
      (set! right (node value)))

    (define (dispatch m)
      (cond ((eq? m 'left-node) left-node)
            ((eq? m 'right-node) right-node)
            ((eq? m 'get-value) get-value)
            ((eq? m 'set-left-node!) set-left-node!)
            ((eq? m 'set-right-node!) set-right-node!)
            (else (error "Unknown operation -- NODE" m))))

    dispatch))

; 2022/2/6，成功实现二叉树
(define (binary-tree compare-to)
  (let ((tree (list '*tree*)))

    (define (tree-node)
      (cdr tree))
    
    (define (empty?)
      (null? (cdr tree)))

    (define (lookup value)
      (define (lookup-iter node value)
        (if (null? node)
            '()
            (let ((compare-result (compare-to ((node 'get-value)) value)))
              (cond ((= 0 compare-result) 'exists)
                    ((> 0 compare-result)
                     (lookup-iter ((node 'right-node)) value))
                    (else
                     (lookup-iter ((node 'left-node)) value))))))

      (if (empty?)
          '()
          (lookup-iter (tree-node) value)))

    (define (insert! value)
      (define (insert-iter sub-tree value)
        (let ((compare-result (compare-to ((sub-tree 'get-value)) value)))
          (cond ((= 0 compare-result) tree-node)
                ((> 0 compare-result)
                 (let ((right-node ((sub-tree 'right-node))))
                   (if (null? right-node)
                       ((sub-tree 'set-right-node!) value)
                       (insert-iter right-node value))))
                ((< 0 compare-result)
                 (let ((left-node ((sub-tree 'left-node))))
                   (if (null? left-node)
                       ((sub-tree 'set-left-node!) value)
                       (insert-iter left-node value)))))))
      
      (if (empty?)
          (set! tree (cons (car tree)
                           (node value)))
          (insert-iter (tree-node) value)))

    (define (print)
      (let ((sub-tree (tree-node)))
        (define (print-line num)
          (if (> num 0)
              (begin
                (display '-)
                (print-line (- num 1)))
              '()))

        (define (print-node node level)
          (begin
            (print-line level)
            (display ((node 'get-value)))
            (newline)))
        
        (define (print-iter current-node level)
          (if (null? current-node)
              '()
              (let ((left-tree ((current-node 'left-node)))
                    (right-tree ((current-node 'right-node))))
                (begin
                  (print-node current-node level)
                  (print-iter left-tree (+ level 1))
                  (print-iter right-tree (+ level 1))))))

        (print-iter sub-tree 0)))
    
    (define (dispatch m)
      (cond ((eq? m 'empty?) empty?)
            ((eq? m 'lookup-proc!) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'print-proc) print)
            (else (error "Unknown operation -- BINARY-TREE" m))))

    dispatch))
    

(define (int-compare-to first second)
  (cond ((> first second) 1)
        ((< first second) -1)
        (else 0)))

(define int-tree
  (binary-tree int-compare-to))



; 二叉树测试
#|
(define test-tree int-tree)
((test-tree 'insert-proc!) 2)
((test-tree 'insert-proc!) 1)
((test-tree 'insert-proc!) 3)
((test-tree 'insert-proc!) 4)

((test-tree 'lookup-proc!) 0)
((test-tree 'lookup-proc!) 1)
((test-tree 'lookup-proc!) 2)
((test-tree 'lookup-proc!) 3)
((test-tree 'lookup-proc!) 4)

((test-tree 'print-proc))
|#

; 可以在父表格和子表格里使用二叉树，需要提供相应的排序方法

; 3.27, 2022/02/07

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

#|
; 这只是个伪代码,我们的表格是二维的
(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
        ; 当 previously-computed-result 为 false 时,向 table 插入新值
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))

(define memo-fib  
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))

|#

; 因为不需要计算很多重叠的子问题；对于第 n 个数，之前已经有计算结果，计算一次即可
; 不能工作，fib 函数依然会计算那些子问题



