#lang sicp

(#%require (only racket provide))

;; 数字电路模拟器

(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))

    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))

    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

; 队列
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

;; 删除队列前端的数据项
(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))

;; 待处理表

; 时间段数据结构，由[时间]和[处理队列]组成
(define (make-time-segment time queue)
  (cons time queue))

(define (segment-time s) (car s))

(define (segment-queue s) (cdr s))

; 待处理表数据结构，其由时间段组成
; 返回一个新的空的待处理表，由[当前时间]和[时间段]组成
(define (make-agenda) (list 0))

; 返回当时的模拟时间
(define (current-time agenda) (car agenda))

(define (set-current-time! agenda time)
  (set-car! agenda time))

(define (segments agenda) (cdr agenda))

(define (set-segments! agenda segments)
  (set-cdr! agenda segments))

(define (first-segment agenda) (car (segments agenda)))

(define (rest-segments agenda) (cdr (segments agenda)))

; 检查待处理表是否为空
(define (empty-agenda? agenda)
  (null? (segments agenda)))

; 修改待处理表，加入一项，要求在特定时间运行给定的动作过程
(define (add-to-agenda! time action agenda)
  ; 检查表是否为空，或者比当前时间小
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))

  ; 创建新的时间段
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))

  ; 将新的时间段插入到待处理表中；递归遍历待处理表，找到合适的位置就插入
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr!
               segments
               (cons (make-new-time-segment time action)
                     (cdr segments)))
              (add-to-segments! rest)))))

  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment time action)
               segments))
        (add-to-segments! segments))))

; 修改待处理表，删除其中的第一个项目
(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))

; 返回待处理表里的第一个项目
(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

(define the-agenda (make-agenda))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))


;; 实例模拟

; 线路上的“监测器”，用于显示模拟器的活动
(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display " New-value = ")
                 (display (get-signal wire)))))

(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

; 添加“监测器”
(probe 'sum sum)
(probe 'carry carry)

; 将线路连接到一个半加器电路上，执行模拟

; 逻辑否
(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

; 反门
(define (inverter input output)
  (define (invert-input)
    ; 对 input 线上取到的信号值取逻辑否
    (let ((new-value (logical-not (get-signal input))))
      ; 在 inverter-delay 后,在 output 线上设置新值
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))

  ; 在 input 线上加上 invert-input 过程
  (add-action! input invert-input)
  'ok)

; 逻辑与
(define (logical-and s1 s2)
  (cond ((= s1 s2 1) 1)
        ((not (= s1 s2)) 0)
        (else 0)))

; 与门
(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

; 逻辑或
(define (logical-or s1 s2)
  (cond ((= s1 s2 0) 0)
        (else 1)))

; 或门
(define (or-gate o1 o2 output)
  (define (or-action-procedure)
    (let  ((new-value
            (logical-or (get-signal o1) (get-signal o2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))

  (add-action! o1 or-action-procedure)
  (add-action! o1 or-action-procedure)
  'ok)

; 半加器
(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

; 全加器
(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

; 模拟 1
(half-adder input-1 input-2 sum carry)

(set-signal! input-1 1)

(propagate)


; 模拟 2
(set-signal! input-2 1)

(propagate)



