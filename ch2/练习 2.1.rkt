#|
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define x (cons 1 2))

(define y (cons 3 4))

(define z (cons x y))


(define (make-rat n d) (cons n d))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (gcd n d)
  (if (= d 0)
      n
      (gcd d (remainder n d))))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

;练习2.1
(define (make-rat n d)
  (define (abs x)
    (cond ((< x 0) (- 0 x))
          (else x)))
  (let ((g (gcd n d)))
    (if (> 0 (/ n d))
        (cons (- 0 (abs (/ n g))) (abs (/ d g)))
        (cons (/ n g) (/ d g)))))

;练习2.2
;点
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

;线
(define (make-segment a b)
  (cons a b))
(define (start-segment l)
  (car l))
(define (end-segment l)
  (cdr l))
(define (average a b)
  (/ (+ a b) 2))
(define (midpoint-segment l)
  (make-point (average (x-point (start-segment l))
                       (x-point (end-segment l)))
              (average (y-point (start-segment l))
                       (y-point (end-segment l)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))



;测试
(define fi (make-point 1 2))
(define se (make-point 3 4))
(define line (make-segment fi se))
(midpoint-segment line)


;练习2.3
;一个矩形有长、宽共四条边
;则将一个矩形定义为长与宽的集合
(define (square x)
  (* x x))
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
(define (average-damp f)
  (lambda (x) (average x (f x))))
(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (long l)
  (sqrt (+ (square (- (x-point (start-segment l))
                      (x-point (end-segment l))))
           (square (- (y-point (start-segment l))
                      (y-point (end-segment l)))))))


(define (rectangle a b)
  (cons (long a) (long b)))

#|
(define (perimeter rect)
  (* 2 (+ (car rect) (cdr rect))))

(define (area rect)
  (* (car rect) (cdr rect)))
|#

(define (a rect) (car rect))
(define (b rect) (cdr rect))
(define (peri rect)
  (* 2 (+ (a rect) (b rect))))
(define (are rect)
  (* (a rect) (b rect)))

;改表示则需要有一个抽象层计算矩形边

;练习2.4
#|
(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))
|#

#|
(define k (cons 3 5))
(car k)
(k (lambda (p q) p))
((cons 3 5) (lambda (p q) p))
((lambda (m) (m 3 5)) (lambda (p q) p))
;这里的m要求是一个过程
((lambda (p q) p) 3 5)
;不好理解
;lambda可以将函数映射到过程
|#

;练习2.5
;构造函数
#|
(define (fast-expt b n)
  (define (square x)
    (* x x))
  (define (even? n)
    (= (remainder n 2) 0))
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))
|#
(define (log3 x)
  (/ (log x) (log 3)))
(define (log2 x)
  (/ (log x) (log 2)))

;使用
#|
(define (cons a b)
  (* (fast-expt 2 a) (fast-expt 3 b)))

(define (car z)
  (cond ((= 0 (remainder z 3)) (car (/ z 3)))
        (else (log2 z))))

(define (cdr z)
  (cond ((= 0 (remainder z 2)) (cdr (/ z 2)))
        (else (log3 z))))
|#
;(define test (cons 3 4))

(define (square x)
  (* x x))

;练习2.6
;Church计数
(define zero (lambda (f) (lambda (x) x)))
;类似于(lambda (x) x)

(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add-one n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define (+ a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))
;照猫画虎
;(((+ one two) square) 2)
;2^(2^3)=256
;g = ((b f) x)得出一个值，然后((a f) g)又是一个值

;(lambda (f x) (f (n f) x))
;描述：就是将函数复合一次
;zero：((zero f) x) = x
;one: ((one f) x) = f(x)

#|
解释one：
(((add-one zero) square) 2)
(((lambda (f) (lambda (x) (f ((zero f) x)))) square) 2)
((lambda (x) (square ((zero square) x))) 2)
((lambda (x) (square x)) 2)

解释two：
(((add-one one) square) 3)
(((lambda (f) (lambda (x) (f ((one f) x)))) square) 3)
((lambda (x) (square ((one square) x))) 3)
((lambda (x) (square (square x))) 3)
|#

;抽象到极点

|#

;练习2.7
(define (make-interval a b)
  (cons a b))

(define (upper-bound k)
  (if (> (car k) (cdr k))
      (car k)
      (cdr k)))

(define (lower-bound k)
  (if (> (car k) (cdr k))
      (cdr k)
      (car k)))

(define (mi a b)
  (if (> a b)
      b
      a))

(define (min a b c d)
  (mi (mi a b) (mi c d)))

(define (ma a b)
  (if (> a b)
      a
      b))

(define (max a b c d)
  (ma (ma a b) (ma c d)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (ma (lower-bound x) (lower-bound y))
                    (mi (lower-bound x) (lower-bound y)))
                 (- (ma (upper-bound x) (upper-bound y))
                    (mi (upper-bound x) (upper-bound y)))))


(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
  (make-interval (min p1 p2 p3 p4)
                 (max p1 p2 p3 p4))))
;下界是最小值，上界是最大值

(define (div-interval x y)
  (if (or (= (upper-bound y) 0)
          (= (lower-bound y) 0))
      (display "Divsion by zero!")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

;(define k (make-interval 2 3))
;(define kk (make-interval 5 7))
;有问题

;练习2.9
(define (width inter)
  (/ (- (upper-bound inter) (lower-bound inter)) 2))

;(define g1 (mul-interval k kk))
;(define g2 (add-interval k kk))
;题目是什么意思？
;就是说乘除没有线性？

;练习2.10
;(define t1 (make-interval 0 1))
;(define t2 (div-interval k t1))
;(define t3 (div-interval t1 k))
;为什么说意义不明？？？？
;说的是区间界为0就会出问题

;练习2.11
;wtf?
;哪九种情况？
;(-1, 0, 1)和(-1, 0, 1)?
;跳过

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))



;练习2.12
(define (% a)
  (/ a 100))
(define (make-center-percent cen per)
  (define (percent k)
    (* cen per))
  (let ((w (percent cen)))
    (make-interval (- cen w) (+ cen w))))

(define k3 (make-center-percent 5 0.01))
(define k4 (make-center-percent 5 0.02))
;设误差为 i、j
;(5 0.01)*(5 0.02) = (25 0.03)
;(5 0.01) + (5 0.02) = (10 0.015)
;(5 0.01) / (5 0.02) = 
;

;(define k5 (mul-interval k3 k4))
;倒也还行

;练习2.13
;误差：计算误差前的区间减去计算误差后的区间
;2.8525
;0.0785-7.85%
;误差：w+k-w*k

;练习2.14
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
;1/(add-interval r1 r2)时出现严重误差
;1/2*4.95 > 1/2*5.05，换序了。。。
;在mul选择区间时，误差区间比更为准确的区间范围更大

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))
;在最初两个div的时候出现误差，但是最外层的div使得误差消失
;就是两层的div消除了误差

;当误差非常小的时候两者十分接近
;par2拥有更好的区间

(define k5 (par1 k3 k3))
(define k6 (par2 k3 k3))



;练习2.15
;进行误差分析!!!!
;误差是什么造成的？
;误差分析见2.14

;关键是要消去不准确的变量
;当进行除法时很容易出现不准确变量

;练习2.16
;这里的原因就是div导致区间换序，又由于mul的乘法机制导致误差

;可能的方案：对分母进行因数分解
;还可以挖深点，去探究IEEE浮点数与这之间的联系

;如何消除换序误差？
;通过化简似乎可以达到效果

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
  (make-interval (min p1 p2 p3 p4)
                 (max p1 p2 p3 p4))))

(define (error-mul x y)
  (let ((p1 (* (lower-bound x) (upper-bound y)))
        (p2 (* (upper-bound x) (lower-bound y))))
    (make-interval p1 p2)))

#|
(define (div-interval x y)
  (cond ((or (= (upper-bound y) 0)
             (= (lower-bound y) 0))
         (display "Divsion by zero!"))
        ((error) (error-mul x
                            (make-interval (/ 1.0 (upper-bound y))
                                           (/ 1.0 (lower-bound y)))))
        (else (mul-interval x
                            (make-interval (/ 1.0 (upper-bound y))
                                           (/ 1.0 (lower-bound y)))))))
|#

(define (equal-interval x y)
  (if (and (= (upper-bound x) (upper-bound y))
           (= (lower-bound x) (lower-bound y)))
      (= 0 0)
      (= 0 1)))

 
;要么就是特例化，要么就用一组全新的定义

;下次改吧。
;定义出现误差的情况是什么
;如何检测误差
;放弃，太难了。已经花了很多时间了。
;查了答案，这道题是大忽悠

