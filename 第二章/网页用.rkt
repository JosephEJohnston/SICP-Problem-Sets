#lang racket
(define (for-each proc items)
  (define (do-next)
    (display (proc (car items)))
    (for-each proc (cdr items)))
  (if (null? items)
      (newline)
      (do-next)))

;
(define x (make-vect 0 0))
(define y (make-vect 1 0))
(define z (make-vect 0 1))

(define k (make-vect 1 1))
(define j (make-vect 0 1))

(define g (make-vect 0 0))
(define l (make-vect 1 0))

(define af (make-frame x y z))

(define kj (make-segment k j))
(define kl (make-segment k l))
(define gj (make-segment g j))
(define gl (make-segment g l))
(define sl (list kj kl gj gl))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ;这是一个向量，frame-coord-map是将一个向量映射为一个向量
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

((segments->painter sl) af)
;向量坐标应该在1以内

#|
(define k (make-vect 1 1))
(define j (make-vect 1 0))
;右边框


(define k (make-vect 1 1))
(define j (make-vect 0 1))
;上边框


(define k (make-vect 1 0))
(define j (make-vect 0 0))
;下边框

(define k (make-vect 0 0))
(define j (make-vect 0 1))
;左边框

;frame-coord-map公式
v = origin + x*edge1 + y*edge2


|#