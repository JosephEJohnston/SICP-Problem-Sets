#lang racket

(require sicp)

(require sicp-pict)

(define wave einstein)

;(define wave2
;  (beside wave (flip-vert wave)))


#|
(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))
|#


(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))


(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))


#|
(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))
|#

;练习2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))


;高阶操作


(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))


(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))


;练习2.45
#|
(define (split l r painter n)
  {if (= n 0)
      painter
      (let ((smaller (split l r painter (- n 1))))
        (l painter (r smaller smaller)))})

(define (right-split painter n)
  (split beside below painter n))

(define (up-split painter n)
  (split below beside painter n))

(paint (up-split wave 1))
|#

   
;框架

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))


(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect a b)
  (cons (+ (xcor-vect a) (xcor-vect b))
        (+ (ycor-vect a) (ycor-vect b))))

(define (sub-vect a b)
  (cons (- (xcor-vect a) (xcor-vect b))
        (- (ycor-vect a) (ycor-vect b))))

(define (scale-vect c v)
  (cons (* c (xcor-vect v)) (* c (ycor-vect v))))


;练习2.47

#|
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
|#


(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))


(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (car (cdr frame)))

(define (edge2-frame frame)
  (cdr (cdr frame)))

;另一个实现：懒得写了

(define a-frame (make-frame
                 (make-vect 1 2)
                 (make-vect 1 1)
                 (make-vect 0.5 0.5)))


;画家

;练习2.48

(define (make-segment start-segment end-segment)
  (cons start-segment end-segment))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))


;练习2.49
#|
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))


(define x (make-vect 1 1))

(define y (make-vect 2 2))
没法实现，缺个draw-line
;在网页中练习实现
|#

;a)
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

;b)
(define oa (make-segment k g))
(define ao (make-segment j l))
(define oal (list oa ao))

;c)
(define a1 (make-segment (make-vect 0.5 0)
                         (make-vect 1 0.5)))
(define a2 (make-segment (make-vect 0.5 0)
                         (make-vect 0 0.5)))
(define a3 (make-segment (make-vect 1 0.5)
                         (make-vect 0.5 1)))
(define a4 (make-segment (make-vect 0 0.5)
                         (make-vect 0.5 1)))
;segment中的构成向量与起始点有关系

;d)
(define r1 (make-segment (make-vect 0.2 0)
                         (make-vect 0.3 0.55)))

(define r5 (make-segment (make-vect 0.3 0.55)
                         (make-vect 0.25 0.65)))

(define r6 (make-segment (make-vect 0.25 0.65)
                         (make-vect 0.17 0.45)))

(define r7 (make-segment (make-vect 0.17 0.45)
                         (make-vect 0 0.68)))

(define r8 (make-segment (make-vect 0 0.85)
                         (make-vect 0.17 0.65)))

(define r9 (make-segment (make-vect 0.17 0.65)
                         (make-vect 0.25 0.75)))

(define r10 (make-segment (make-vect 0.25 0.75)
                          (make-vect 0.40 0.75)))

(define r2 (make-segment (make-vect 0.3 0)
                         (make-vect 0.5 0.35)))

(define r3 (make-segment (make-vect 0.5 0.35)
                         (make-vect 0.7 0)))

(define r4 (make-segment (make-vect 0.80 0)
                         (make-vect 0.60 0.5)))
;

;画家的变换和组合
#|
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))
|#
(paint (rotate90 wave))

