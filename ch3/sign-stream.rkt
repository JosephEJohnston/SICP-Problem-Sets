#lang sicp

(#%require "stream.rkt")
(#%require "infinite-stream.rkt")
(#%require (only racket provide))

(provide integral)
(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

