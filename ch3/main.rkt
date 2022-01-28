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

#|
(rand)
(rand)
(rand)
(rand)
(rand)
|#

#|
(define (cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          (else (error "Undefined operation -- CONS" m))))
    dispatch)

(define (car z) (z 'car))

(define (cdr z) (z 'cdr))

(define (set-car! z new-value)
  ((z 'set-car!) new-value)
  z)

(define (set-cdr! z new-value)
  ((z 'set-cdr!) new-value)
  z)
|#

#|
(provide front-ptr
         rear-ptr
         set-front-ptr!
         set-rear-ptr!
         empty-queue?
         make-queue
         front-queue
         insert-queue!
         delete-queue!)

; 取队列的数据部分
(define (front-ptr queue) (car queue))

; 取队列的尾指针
(define (rear-ptr queue) (cdr queue))

; 设置队列的数据
(define (set-front-ptr! queue item) (set-car! queue item))

; 设置队列的尾指针
(define (set-rear-ptr! queue item) (set-cdr! queue item))

; 检查队列是否为空
(define (empty-queue? queue) (null? (front-ptr queue)))

; 返回一个空队列
(define (make-queue) (cons '() '()))

; 返回队列前端的对象
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

; 将数据项插入队列末端
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           ; 将队尾指针置为原队尾指针 + 新元素
           (set-cdr! (rear-ptr queue) new-pair)
           ; 将队尾指针置为新元素
           (set-rear-ptr! queue new-pair)
           queue))))

; 删除队列前端的数据项
(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))
|#

; 查找 records 中是否存在对应的 key
(provide assoc)
(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

; 查找 table 中对应 key 的数据
(provide lookup)
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))

; 两维表格数据结构
(provide make-table)
(define (make-table)
  (let ((local-table (list '*table*)))

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
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))

    dispatch))

(provide operation-table)
(define operation-table (make-table))

(provide get)
(define get (operation-table 'lookup-proc))

(provide put)
(define put (operation-table 'insert-proc!))


    