#lang sicp

(#%require "parallel.rkt")

; 3.48, 2022/02/12，为什么上面提出的避免死锁的方法能够避免交换问题中的死锁

; 因为能保证串行化总能以一个顺序执行？
; serialized-exchange 的修改思路大概是：
; 若 n1 > n2，则 (serializer1 (serializer2 exchange))，否则 (serializer2 (serializer1 exchange))
; make-account 可能需要集中弄一个新对象

; 3.49, 2022/02/12，设法描述一种情形，使上述的避免死锁机制在这种情况中不能正常工作
; 情形：进程必须在访问了某些共享资源后，才能确定它是否还需要访问其他共享资源
; 暂时想不出来
