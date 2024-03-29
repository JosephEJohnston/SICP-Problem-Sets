(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))




(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (expt x y)
  (display "do-expt")
  (display x)
  (display y))

;;实数
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (define (raise-scheme-number x)
    (make-complex-from-real-imag x 0))

   
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put '=zero? '(scheme-number)
       (lambda (z) (= 0 z)))
  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) (tag (expt x y))))
  (put 'raise '(scheme-number)
       (lambda (x) (raise-scheme-number x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

;;有理数
(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((n-n (normalize n))
          (d-n (normalize d))
          (g (gcd n-n d-n)))
      (cons (/ n-n g) (/ d-n g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (raise-rational x)
    (make-scheme-number (/ (numer x) (denom x))))
  (define (project-raiontal x)
    (make-scheme-number (/ (numer x) (denom x))))
  (define (equ-rational? x y)
    (and (equ? (numer x) (numer y))
         (equ? (denom x) (denom y))))
  (define (normalize-rational x)
    (make-scheme-number (/ (numer x) (denom x))))

  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational)
       (lambda (x y) (equ-rational? x y)))
  (put '=zero? '(rational)
       (lambda (z) (and (= 0 (numer z))
                        (= 0 (denom z)))))
  (put 'raise '(rational)
       (lambda (x) (raise-rational x)))
  (put 'project '(rational)
       (lambda (x) (project-rational x)))
  (put 'normalize '(rational)
       normalize-rational)
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (square x)
  (* x x))

(define (install-rectangular-package)
  ;系统操作
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y)
    (cons (normalize x) (normalize y)))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  (define (equ-rectangular? x y)
    ;内部的equ？并没有转到外部分派，所以出了bug
    (and (equ? (real-part x) (real-part y))
         (equ? (imag-part x) (imag-part y))))
  (define (=zero-rectangular? z)
    (and (equ? 0 (real-part z)) (equ? 0 (imag-part z))))
  ;另一个系统也有相同的操作
  (define (project-rectangular z)
    (make-scheme-number (real-part z)))
  (define (normalize-rectangular z)
    (magnitude z))
    

  ;(put <op> <type> <item>)
  ;将项<item>加入表格中 ，以<op>和<type>作为这个表项的索引
  (define (tag x) (attach-tag 'rectangular x))
  ;语言指代：对直角坐标数据的求实部
  ;对type的op
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       ;将用make-from-real-imag生成的数据对象用tag包裹
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'equ? '(rectangular rectangular)
       (lambda (x y) (equ-rectangular? x y)))
  (put '=zero? '(rectangular) =zero-rectangular?)
  (put 'project '(rectangular) project-rectangular)
  (put 'normalize '(rectangular) normalize-rectangular)
  'done)

(define (install-polar-package)
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons (normalize r)
                                        (normalize a)))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  (define (equ-polar? x y)
    (and (equ? (magnitude x) (magnitude y))
         (equ? (angle x) (angle y))))
  (define (=zero? z)
    (and (= 0 (magnitude z)) (= 0 (angle z))))
  (define (project-polar z)
    (make-scheme-number (real-part z)))
  (define (normalize-polar z)
    (magnitude z))
  
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'equ? '(polar polar)
       (lambda (x y) (equ-polar? x y)))
  (put '=zero? '(polar) =zero?)
  (put 'normalize '(polar) normalize-polar)
  'done)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? z) (apply-generic '=zero? z))
(define (project z) (apply-generic 'project z))

;符号剥离及分派函数
#|
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types -- APPLY-GENERIC"
           (list op type-tags))))))
|#
;-------
#|
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

;错误发现处
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (attach-tag type-tag contents)
  (cons type-tag contents))
|#

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else
         (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else
         (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))
;--------

(define (install-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))

  (define (tag z) (attach-tag 'complex z))

  (put 'equ? '(complex complex)
       (lambda (x y) (equ? x y)))
  (put '=zero? '(complex) =zero?)
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'project '(complex) project)
  (put 'normalize '(complex) normalize)
  
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(compelx complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))


;练习2.77
(install-scheme-number-package)
(install-rectangular-package)
(install-rational-package)
(install-polar-package)
(install-complex-package)

;((get 'make-from-mag-ang 'polar) r a)
;(define z1 (make-complex-from-real-imag 3 4))
;(define z2 (make-complex-from-real-imag 0 0))
;(magnitude z)
;(magnitude z) -> 由于z的首符号为complex，寻找complex的magnitude ->
;找到通用型的magnitude -> 进入此层(magnitude z) -> 由于z的首符号为rectangular，寻找此符号的magnitude ->
;找到rectangular的magnitude -> 进入此层，计算结果

;apply-generic被调用了两次
;第一次给的是通用型的magnitude，第二次给的是rectangular的magnitude

;练习2.78

;修改type-tag，contents和attach-tag的定义
;使我们的通用算术系统可以利用Scheme的内部类型系统

#|
(define x (make-scheme-number 7))
(define y (make-scheme-number 8))
(add x y)
(add 7 8)
|#

;成功

;练习2.79

;(define (equ? x y) (apply-generic 'equ x y))

;(angle z1)
(define x1 (make-rational 3 4))
(define x2 (make-rational 3 4))
;(equ? z1 z2)
;处理所出现的问题
;注意命名问题，眼瞎常常看不见。
;都写包里了


;练习2.80

;都写包里了
;(=zero? z2)


;;2.5.2 不同类型数据的组合

#|
(define (add-complex-to-schemenum z x)
  (make-from-real-imag (+ (real-part z) x)
                       (imag-part z)))

(put 'add '(complex scheme-number)
     (lambda (z x) (tag (add-complex-to-schemenum z x))))
|#

;;答案有两个默认函数的实现
(define coercion-table (make-table))
(define get-coercion (coercion-table 'lookup-proc))
(define put-coercion (coercion-table 'insert-proc!))


(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(put-coercion 'scheme-number 'complex scheme-number->complex)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          ;如何用drop改进？
          (let ((result (apply proc (map contents args))))
            ;返回的类型数必然是对偶，但谓词则不然，利用pair?来避免使用real?
            ;程序有问题
            ;为什么更改没有生效？
            (if (and (pair? result)
                     (not (eq? op 'raise))
                     (not (eq? op 'project)))
                ;bug重新出现，继续改写
                (drop result)
                result))
          (if (= (length args) 2)
              ;改这个程序
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (eq? type1 type2)
                    ;两种类型相同的情况
                    (error "No method for these types"
                           (list op type-tags))
                    #|
                    ((let ((t1->t2 (get-coercion type1 type2))
                           (t2->t1 (get-coercion type2 type1)))
                       (cond (t1->t2
                              (apply-generic op (t1->t2 a1) a2))
                             (t2->t1
                              (apply-generic op a1 (t2->t1) a2))
                             (else
                              (error "No method for these types"
                                     (list op type-tags))))))
                    ))
|#
                    (if (eq? (super type1 type2) type1)
                        (apply-generic op a1 (get-type-num type1 a2))
                        (apply-generic op (get-type-num type2 a1) a2))))
              (error "No method for these types"
                     (list op type-tags)))))))

                

;;2.5.2 不同类型数据的组合

;--练习2.81

#|
(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)
(put-coercion 'scheme-number 'scheme-number
              scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)
|#

;所谓对两个相同类型的参数做强制，是在不存在相应类型的操作的情况下发生的
;倒也不是什么大问题，反正最后都会跳转到else

;-a)
(define (exp x y) (apply-generic 'exp x y))


;如何处理put-coercion?
;答案有

(define z1 (make-complex-from-real-imag 3 4))
(define z2 (make-complex-from-real-imag 0 0))

;(exp z1 z2)
;若没有给题目的过程：No method for these types (exp (complex complex))
;给了题目的过程：无限循环

;(exp 1 2)


;会出现什么情况：无限循环
;就是卡在：做强制->试验操作 的循环里了
;详见apply-generic描述

;-b)
;没有，改了之后更莫名其妙
;不能

;-c)
;改完了，参见代码

;--练习2.82

;为什么说它还不够一般？

;那些情况会因此不会被考虑进去？
;假设有过程(define (op 实数 实数 复数))
;那么当传入(op 整数 整数 复数)时，三个参数都会转成复数，显示无此操作
;实际上是可以使用操作op的

;那可能就需要从操作参数的类型来检查起了：
;逐个检查参数，如果第一个参数可以转换成操作第一个参数的类型，
;则继续检查第二个参数是否同理，直至所有参数都确认可以强制（或同类型）
;否则，显示无此方法

;--练习2.83

(define (raise x) (apply-generic 'raise x))
(define z3 (make-rational 2 3))

#|
z3
(raise z3)
(raise (raise z3))

(raise z1)
|#

;成功，见代码

;--练习2.84

;还没做

#|
;检测超类：
1）类型相同：返回一个参数，由于apply-generic已改进，就不需要检测了（还是检测吧）
2）提升type1的类型，如果提升后type1等于type2，则type2是type1的超类，返回type2
3）继续提升超类，检测两者类型，
若将type1提升为complex型仍不相同，则type2不是type1的超类，返回type1
|#

;此函数以实值来传递类型，进行比较

#|
(define (super type1 type2)
  (if (eq? type1 type2)
      type1
      (let ((n1 (get-type-num type1 (make-rational 1 2)))
            (n2 (get-type-num type2 (make-rational 1 2))))
            (super-iter n1 n2 n1))))

(define (super-iter type1 type2 tmp)
  (cond ((eq? (type-tag type1)
              (type-tag type2))
         (type-tag type2))
        ((eq? 'complex (type-tag type1))
         (type-tag tmp))
        (else
         (super-iter (raise type1) type2 tmp))))
|#       

(define (get-type-num type num)
  (if (eq? type (type-tag num))
      num
      (get-type-num type (raise num))))

(define (super type1 type2)
  ;添加新类型时改这个tow
  (define tow '(complex rational scheme-number))
  (define (super-iter type1 type2 tower)
    (let ((sign (car tower)))
      (cond ((null? tower) (error "NO such types" (list type1 type2)))
            ((eq? type1 sign) 1)
            ((eq? type2 sign) 2)
            (else
             (super-iter type1 type2 (cdr tow))))))

  (if (= 1 (super-iter type1 type2 tow))
      type1
      type2))

;其实根本不需要实现整数包，我一开始就搞错了
  
;(add 1 z1)

;(super 'rational 'complex)


;先写出来吧。

;问题：无法将raise对单独的符号标志使用
;(raise z3)

;利用(get op item)的性质？
;似乎只能用实值来传递类型

;新的apply-generic还没测试
;测完了，大概是没错的


;--练习2.85

;思想：
;若num投影后结果同原num，则drop
;否则，返回num
;不可drop整数
;整数没有类型符号，怎么办？

;要求：drop过程，project函数，需要用相等谓词，并以此改造apply-generic

;为什么要用equ?




;(define (project num) (apply-generic 'project num))
;此分派在之前定义


;这里real?为实数谓词，可以改为(eq? (type-tag scheme-number) scheme-number)吗？

#|
(define (drop num)
  (cond ((and (number? num) (equ? num (round num)))
         num)
        ((equ? num (project num))
         (drop (project num)))
        (else num)))
|#

(define (drop num)
  (if (not (eq? (type-tag num) 'scheme-number))
      (let ((drop-num (project num)))
        (if (equ? num drop-num)
            (drop drop-num)
            num))
      num))
      

;第一个情况和else可以合并

;(drop z2)
;改造分派函数，那谓词怎么办？

;(equ? z1 z2)
;(add z2 z2)

;传到super那里无限循环？？？why？？？
;raise之后再drop是最骚的。

;(equ? z2 (project z2))
;无限循环
;大概算是把bug改掉了

;(make-complex-from-real-imag 0 0)
;构造方法没有经过过程分派

;(real-part z1)
;(define z4 (make-complex-from-real-imag z2 z2))
;(define z5 (real-part z4))

;(drop z4)
;(equ? z2 z3)

;(equ? z2 z3)
;还是没整清楚
;进行大规模改写

;差不多就行了，本身题出得和章节示例是不一样的
;题目要求类型塔，但章节示例不是类型塔结构
;没有整数类型，那么最低类只能视为rational

;好像成功了，虽然程序我都不想看了。。

;就不按类型塔改了，把最低类型改为常规数
;;一个很大的问题就是整数类型和实数类型的混淆。
;然后就出bug了，下次改。

;成功改出bug，是equ?出了问题

;--练习2.86
(define z6 (make-complex-from-real-imag
            (make-rational 1 2)
            (make-rational 2 3)))

(define z7 (make-complex-from-real-imag z1 z1))
;提供给操作的参数类型应当都是实数，否则没法运算
;(mul z6 z6)
;运算要求接口？
;有理数还好说，复数则需要实现复数规范化


;实现运算规范化？
;规范化过程：将一个类型数转化为其最简形式
;例：(normalize z6) -> (list 'complex 'rectangular 1/2 2/3)

;或许不是规范化，而是假设其参数为理所应当的类型
;而且提供一个过程将数转化为这种类型
(define (normalize num) (apply-generic 'normalize num))
;所以这里就不实现规范化了，即normalize只用以提供实数


;就以此告一段落吧。
;;似乎多数题没有答案
