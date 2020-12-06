;对抽象数据的求导程序

#|
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (make-product (exponent exp)
                                     (make-exponentiation (base exp) (- (exponent exp) 1)))
                       (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

#|
(define (addend s) (cadr s))

(define (augend s) (caddr s))
|#

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

#|
(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))
|#

;练习2.56
(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '**)))


(define (make-exponentiation base exp)
  (cond ((=number? base 0) 0)
        ((or (=number? base 1)
             (=number? exp 0)) 1)
        ((=number? exp 1) base)
        (else
         (list '** base exp))))


(define (exponent exp)
  (caddr exp))

(define (base exp)
  (cadr exp))

;(deriv '(* x (** x 7)) 'x)
;成功

;指数必须是实值

;练习2.57

(define (addend s) (cadr s))

(define (augend s)
  (if (and (or (sum? s) (product? s))
           (null? (cdddr s)))
      (caddr s)
      ;是二元形参式,选取第二个参数
      (cons '+ (cddr s))))
      ;参数超过二元,选取第一个参数以外的参数组成一个式子返回


(define (multiplier p) (cadr p))

(define (multiplicand p)
  (if (and (or (sum? p) (product? p))
           (null? (cdddr p)))
      (caddr p)
      (cons '* (cddr p))))


;(deriv '(* x (* y (+ x 3))) 'x)

;(deriv '(* (* x y) (+ x 3)) 'x)

;'(* x (* y (+ x 3)))

(define t '(* x (+ y y) y))
(define d '(* (* x y) (+ x 3)))

#|
d
(cdr d)
(cddr d)
(cdddr d)
|#

;(cdddr t)
;(null? (caddr d))



;(deriv t 'x)
;需要处理只有一个算式时发生的问题


;(deriv '(* x y y) 'x)
;成功

;(y (+ x 3)) -> (* y (+ x 3))

;(cons '* '(y (+ x 3))) == '(* y (+ x 3))

#|
(deriv '(* x y (+ x 3)) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)
成功!!!
|#

|#
;练习2.58


(define (infix-deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (infix-deriv (addend exp) var)
                   (infix-deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (infix-deriv (multiplicand exp) var))
          (make-product (infix-deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (make-product (exponent exp)
                                     (make-exponentiation (base exp)
                                                          (- (exponent exp) 1)))
                       (infix-deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

;a)

(define (variable? exp)
  (symbol? exp))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? v c)
  (and (number? v) (= v c)))

;加法
(define (sum? exp)
  (and (pair? exp) (eq? '+ (cadr exp))))

(define (addend e)
  (car e))


;(define (augend e)
;  (caddr e))


(define (augend s)
  (if (or (null? (caddr s)) (null? (cdddr s)))
      (caddr s)
      (cddr s)))


(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

;乘法
(define (product? exp)
  (and (pair? exp) (eq? '* (cadr exp))))

(define (multiplier e)
  (car e))

;(define (multiplicand e)
;  (caddr e))

(define (multiplicand s)
  (if (or (null? (caddr s)) (null? (cdddr s)))
      (caddr s)
      (cddr s)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

;指数
(define (exponentiation? exp)
  (and (pair? exp) (eq? '** (cadr exp))))

(define (base exp)
  (car exp))

(define (exponent exp)
  (caddr exp))

(define (make-exponentiation base exp)
  (cond ((=number? base 0) 0)
        ((or (=number? base 1)
             (=number? exp 0)) 1)
        ((=number? exp 1) base)
        (else
         (list base '** exp))))


;(infix-deriv '((x + y) * (x + y)) 'x)
;(infix-deriv '(x * (x + y)) 'x)
;成功

;b)
;可能方案：加法时进入递归，乘法时进行求导


;(define t '(x + 3 * x))
;(define exp '(x + 3 * (x + y + 2 + x)))

;(infix-deriv exp 'x)
;exp

;(augend t)

;尚有问题，今天先休息
;成功

