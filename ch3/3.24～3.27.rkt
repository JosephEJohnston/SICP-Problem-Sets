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
(define (make-tree-table same-key?)
  (let ((local-table (list '*table*)))

    ; 查找 records 中是否存在对应的 key
    (define (assoc key records)
      (cond ((null? records) false)
        ((same-key? key (caar records)) (car records))
        (else (assoc key (cdr records)))))
    
    ; 用 key-1 查找子表格,用 key-2 在这个子表格里确定记录
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))

    ; 在 key-1 子表格中,插入名为 key-2 的记录
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            ; 存在子表,则插入旧表
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            ; 不存在,初始化并插入新子表和值
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

(define (binary-tree bigger?)
  ((let (tree (list '*binary-tree*)))

   (define (empty?)
     (= (length tree) 1))
   
   (define (lookup value)
     (if (empty)
         (error "Tree is empty -- TREE"))
   
   (define (insert! value)
     ())

   (define (print)
     tree)
   
   (define (dispatch m)
     (cond ((eq? m 'lookup-proc) lookup)
           ((eq? m 'insert-proc!) insert!)
           ((eq? m 'print-proc) print)
           (else (error "Unknown operation -- TREE" m))))

   dispatch))


