

;练习1.29

(define (simpson-rule-sum f a b n)
  (define h
    (/ (- b a) n))
  (define (next-item k)
    (+ k 1))
  (define (simpson-sum k)
    (define (para k)
      (+ a (* k h)))
    (define (even? k)
      (= (remainder k 2) 0))
    (cond ((= k 0) (f (para k)))
          ((= k n) (f (para k)))
          ((even? k) (* (f (para k)) 2))
          (else (* (f (para k)) 4))))
  (* (sum simpson-sum 0 next-item n) (/ h 3)))

;以k为参数
;返回值极其精确

;练习1.30
;线性递归的过程sum
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
    (iter a 0))

(define (sum-cubes a b)
  (define (cube x)
    (* x x x))
  (define (inc n)
    (+ n 1))
  (sum cube a inc b))

;练习1.31
;a)写出product过程
;递归过程：先展开后收束
(define (product-rec term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-rec term (next a) next b))))

(define (factorial a b)
  (define (inc a)
    (+ a 1))
  (define (identity x)
    x)
  (product-rec identity a inc b))

(define (pi-product a b)
  (define (pi-term x)
    (define (even? x)
      (= (remainder x 2) 0))
    (define (biger x)
      (+ 1 (/ 1 (+ x 1))))
    (define (smaller x)
      (- 1 (/ 1 (+ x 2))))
    (cond ((even? x) (biger x))
          (else (smaller x))))
  (define (inc x)
    (+ x 1))
  (product-iter pi-term a inc b))
;很粗糙的近似法
;需要n趋很大时才近似于π/4
;用起来还挺慢

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

;练习1.32
;a)

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate
                 combiner null-value term (next a) next b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

;b)
(define (accumulate combiner null-value term a next b)
  (define (iter-acc a result)
    (if (> a b)
        result
        (iter-acc (next a) (combiner result (term a)))))
  (iter-acc a null-value))

;练习1.33
(define (filtered-accumulate
         combiner null-value term a next b filtered)
  (cond ((> a b)
         null-value)
        ((filtered a)
         (combiner (term a)
                   (filtered-accumulate
                    combiner null-value term (next a) next b filtered)))
        (else
         (filtered-accumulate
          combiner null-value term (next a) next b filtered))))
#|
a)
(define (prime-sum a b)
  (define (next x)
    (+ x 1))
  (define (identity x)
    x)
  (filtered-accumulate
   + 0 identity a next b miller-rabin-test))
|#
;用的是练习1.28的miller-rabin-test

#|
b)
(define (gcd-product n)
  (define (next n)
    (+ n 1))
  (define (identity n)
    n)
  (define (gcd-filtered a)
    (define (gcd a b)
      (if (= b 0)
          a
          (gcd b (remainder a b))))
    (if (= 1 (gcd a n))
        (= 0 0)
        (= 0 1)))
  (filtered-accumulate
   * 1 identity 1 next n gcd-filtered))
|#

;练习1.34
(define (square x)
  (* x x))

(define (f g)
  (g 2))

;猜测：会发生栈溢出（不断循环调用）
;实际：解释器报错

;练习1.35
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


#|
(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)
1.6180327868852458
|#

;练习1.36
(define (average a b)
  (/ (+ a b) 2))

(define (fixed-point f first-guess)
  (define  tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  ;next定义域不一样
  (define (try guess)
    (let ((next (f guess)))
      (define (try-next)
        (newline)
        (display guess)
        (try next))
      (define (next-end)
        (newline)
        next)
      (if (close-enough? guess next)
          (next-end)
          (try-next))))
  (try first-guess))

#|
不采用平均阻尼：
(fixed-point (lambda (x) (/ (log 1000) (log x))) 2)
33步
采用平均阻尼：
(fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2)
8步
|#

;练习1.37
;a)
#|
递归定义：
Nk/Dk+F(k+1), k < n
Nk/Dk,        k = 0

(define (cont-frac-rec n d k)
  (if (= 0 k)
      0
      (/ n (+ (cont-frac-rec n d (- k 1)) d))))
|#
;递归版本

(define (cont-frac n d k)
  (define (cont-frac-rec n d k r)
    (let ((ni (n r))
          (di (d r)))
      (if (= 0 k)
          0
          (/ ni (+ (cont-frac-rec n d
                                  (- k 1)
                                  (+ r 1)) di )))))
  (cont-frac-rec n d k 1))

;以迷之方式成功改掉了bug
#|
k = 100000:
0.6180339887498948
k至少为100
|#

;b)

(define (cont-frac n d k)
    (define (cont-frac-iter n d k quo)
      (let ((ni (n k))
            (di (d k)))
        (if (= 0 k)
            quo
            (cont-frac-iter n d (- k 1)
                            (/ ni (+ di quo))))))
  (cont-frac-iter n d k 0))

;问题：第二次进入循环时di未变（作用域错了）
;成功debug
;迭代：反向叠加

;练习1.38

(define (e-d k)
  (define (divide-t? k)
    (= (remainder (+ k 1) 3) 0))
  (let ((cal (/ (* 2 (+ k 1)) 3)))
    (if (divide-t? k)
        cal
        1)))


;(+ 2 (cont-frac (lambda (i) 1.0) e-d 1000))
;(cont-frac (lambda (i) 1.0) e-d 2)
(define (cube x)
  (* x x x))

;练习1.39

(define (tan-cf x k)
  (define (constant)
    (* x x))
  (define (cont-frac n d k combiner)
    (define (cont-frac-iter n d k quo combiner)
      (let ((ni (n k))
            (di (d k)))
        (cond ((= 0 k) quo)
              ((= 1 k) (cont-frac-iter n d (- k 1)
                                       (combiner di quo)
                                       combiner))
              (else (cont-frac-iter n d (- k 1)
                            (/ ni (combiner di quo))
                            combiner)))))
  (cont-frac-iter n d k 0 combiner))
  (/ x (cont-frac (lambda (x) (constant))
                  (lambda (k) (- (* 2 k) 1))
                  k -)))


;总算改对了，问题出在k=1时，花的时间已经多到数不清了。
(define (tan x)
  (/ (sin x) (cos x)))
;tan0.785

(define (test x)
  (/ x (- 1 (/ (* x x) (- 3 (/ (* x x) 5))))))

;练习1.40
(define (newton-transform g)
  (define (deriv g)
    (define dx 0.00001)
    (lambda (x)
      (/ (- (g (+ x dx)) (g x))
         dx)))
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (lambda (x)
    (+ (* x x x) (* a x x) (* b x) c)))

;练习1.41
(define (double f)
  (lambda (x)
    (f (f x))))

;(((double (double double)) inc) 5)
;返回21

;练习1.42
(define (compose f g)
  (lambda (x)
    (f (g x))))

;练习1.43
(define (square x)
  (* x x))



(define (repeated f t)
  (define (repeated-iter f g t)
    (if (= t 1)
        (lambda (x) (f x))
        (lambda (x) ((repeated-iter (compose f g) g (- t 1)) x))))
  (repeated-iter f f t))

;需要一个变量来存储原函数
;((repeated square 2) 5)

;练习1.44

(define (smooth f)
  (define dx 0.00001)
  (lambda (x) (/ (+ (f (- x dx))
                    (f x)
                    (f (+ x dx))) 3)))

(define (smooth-repeated n f)
  (lambda (x) (((repeated smooth n) f) x)))

;练习1.45

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (mul f1 f2)
  (lambda (x) (* (f1 x) (f2 x))))

;函数：求x的n次方根，k为平均阻尼的次数
(define (root x n k)
  (define (exponent n)
    (define (mul f1 f2)
      (lambda (x) (* (f1 x) (f2 x))))
    (define (exp-rec f g n)
      (if (= n 1)
          (lambda (x) (f x))
          (lambda (x) ((exp-rec (mul f g) g (- n 1)) x))))
    (exp-rec (lambda (x) x) (lambda (x) x) n))
  (define (average-damp f)
    (define (average a b)
      (/ (+ a b) 2))
    (lambda (x) (average x (f x))))
  (fixed-point ((repeated average-damp k)
                (lambda (y) (/ x ((exponent (- n 1)) y))))
               1.0))
;n大于1
;下一次求平均阻尼次数
;只有一个平均阻尼时不收敛
#|
2:1
3:1
4:2
5:2
6:2
7:2
8:3
9:3
15:3
16:4
31:4
32:5
即O(logn)
|#

;练习1.46
(define (iterative-improve good-enough? improve)
  (define (iter good-enough? improve guess next)
    (let ((next (improve guess))) 
      (if (good-enough? guess next)
          guess
          (iter good-enough? improve (improve guess) (improve next)))))
  (lambda (x) (iter good-enough? improve 1.0 x)))
;搞定

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define tolerance 0.00001)
  ((iterative-improve close-enough? f) first-guess))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))
