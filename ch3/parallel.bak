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


