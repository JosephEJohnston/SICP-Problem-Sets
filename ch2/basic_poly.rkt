
;coeff:系数
;type-terms:非系数项,如 xy^3;传入的参数应为(list type-term1 type-term2 ...) 
;type-term:非系数单类型项,如 x^2
(define (type-term type order)
  (list type order))
(define (basic-term coeff type-terms)
  (list coeff type-terms))
;先不要管合并的问题

(define (make-from-basic-poly basic-terms)
  (basic-terms))

;基本多项式包
#|
(define (install-basic-polynomial-package)

  ;需要分派函数，否则没法执行
  ;所以干脆把包去掉
  (define (make-from-basic-poly basic-terms)
    (basic-terms))
  

  'done)

(install-basic-polynomial-package)
|#