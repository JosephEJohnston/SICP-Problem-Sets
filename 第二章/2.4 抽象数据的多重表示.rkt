
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


#|
(define *op-table* (make-hash))

(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

(define (get op type)
  (hash-ref *op-table* (list op type) '()))
|#


;算术系统
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
#|
;实部和虚部
(define (real-part-rectangular z) (car z))

(define (imag-part-rectangular z) (cdr z))

(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))

(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
        (real-part-rectangular z)))

(define (make-from-real-ima-rectangularg x y)
  (attach-tag 'rectangular (cons x y)))

(define (make-from-mag-ang r a)
  (attach-tag 'rectangular
              (cons (* r (cos a)) (* r (sin a)))))


;模和幅角
(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))

(define (imag-part z)
  (* (magnitude-polar z) (sin (angle-polar z))))

(define (magnitude-polar z) (car z))

(define (angle-polar z) (cdr z))

(define (make-from-real-imag x y)
  (attach-tag 'polar
              (cons (sqrt (+ (square x) (square y)))
                    (atan y x))))

(define (make-from-mag-ang r a)
  (attach-tag 'polar (cons r a)))




;标志系统
(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))

;通用型选择函数
(define (real-part z)
  (cond ((rectangular? z)
         (real-part-rectangular (contents z)))
        ((polar? z)
         (real-part-polar (contents z)))
        (else (error "Unknown type -- REAL-PART" z))))

(define (imag-part z)
  (cond ((rectangular? z)
         (imag-part-rectangular (contents z)))
        ((polar? z)
         (imag-part-polar (contents z)))
        (else (error "Unknown type -- IMAG-PART" z))))

(define (magnitude z)
  (cond ((rectangular? z)
         (magnitude-rectangular (contents z)))
        ((polar? z)
         (magnitude-rectangular (contents z)))
        (else (error "Unknown type -- MAGNITUDE" z))))

(define (angle z)
  (cond ((rectangular? z)
         (angle-rectangular (contents z)))
        ((polar? z)
         (angle-polar (contents z)))
        (else (error "Unknown type -- ANGLE" z))))

;构造函数
(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))

(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))
|#


(define (square x)
  (* x x))

(define (install-rectangular-package)
  ;系统操作
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;另一个系统也有相同的操作

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
  'done)

(define (install-polar-package)
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))

  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types -- APPLY-GENERIC"
           (list op type-tags))))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

;可整死我了


(install-rectangular-package)

(define z1 (make-from-real-imag 1 1))
(magnitude z1)


;练习2.73
#|
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ;更多规则可以加在这里
        (else (error "unknwon expression type -- DERIV" exp))))
|#
(define (=number? exp num)
  (and (number? exp) (= exp num)))
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '**)))
(define (make-exponentiation base exp)
  (cond ((=number? base 0) 0)
        ((or (=number? base 1)
             (=number? exp 0)) 1)
        ((=number? exp 1) base)
        (else
         (list '** base exp))))


(define (first operands) (car operands))
(define (second operands) (cadr operands))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
                                           var))))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))


(define (install-deriv-package)
  (define (sum operands var)
    (make-sum (deriv (first operands) var)
              (deriv (second operands) var)))
  (define (product operands var)
    (make-sum
          (make-product (first operands)
                        (deriv (second operands) var))
          (make-product (deriv (first operands) var)
                        (second operands))))
  (define (exponentiation operands var)
    (make-product (make-product (second operands)
                                (make-exponentiation (first operands) (- (second operands) 1)))
                  (deriv (first operands) var)))
  
  (put 'deriv '+ sum)
  (put 'deriv '* product)
  (put 'deriv '** exponentiation)
   'done)
;能用，但是比较莫名其妙，不是数据导向风格

#|
d)

(define (install-deriv-package)
  (define (sum operands var)
    (make-sum (deriv (first operands) var)
              (deriv (second operands) var)))
  (define (product operands var)
    (make-sum
          (make-product (first operands)
                        (deriv (second operands) var))
          (make-product (deriv (first operands) var)
                        (second operands))))
  (define (exponentiation operands var)
    (make-product (make-product (second operands)
                                (make-exponentiation (first operands) (- (second operands) 1)))
                  (deriv (first operands) var)))
  
  (put '+ 'deriv sum)
  (put '* 'deriv product)
  (put '** 'deriv exponentiation)
   'done)

|#

;(install-deriv-package)


;(deriv '(+ x (** x 7)) 'x)
;(deriv '(+ x (+ x 7)) 'x)

;a)
;因为此处的数据分派是根据操作符来做的，这两种谓词与操作符不相关

;b)请写出针对和式与积式的求导过程，并把他们安装到表格里
;成功

;c)增加了指数求导
;成功

;d)
;(put <op> <type> <item>)
;将项<item>加入表格中 ,以<op>和<type>作为这个表项的索引
;(get (operator exp) 'deriv) (operands exp) var)


;求导包里把put里op和type反过来就行了
;但从构建角度来说，就是以操作为类型，操作符为操作
;对求导的加法？？


;练习2.74
;目标：寻找一种策略集成起这些文件，以便在维持各个分支机构中
;现存独立工作方式的同时，又能满足公司总部管理的需要

;想法：各个独立机构的数据都可以转换为同一种通用的数据结构，
;同时这个通用数据结构也可以转换为各独立机构的数据结构
;转换层？

;a)

(define (get-record branch table employee)
  ((get 'get-record branch) table employee))

;各独立分支机构的操作中必须能提供get-record操作
;同时存在employee-record的一一映射
;返回的数据必须与通用数据中的record数据相同

;record应该是构造函数，get-record是选择函数

;b)
(define (get-salary branch table employee)
  ((get 'get-salary branch) table employee))

;同上题

;c)

;table：所有分支文件的表
(define (find-employee-record table employee)
  (if (find? (branch table) table employee)
      (get-record (branch table) table employee)
      (find-employee-record (next table) employee)))

;d)
;新的人事文件结构（分支）应当提供同样的op，以及提供它自己的标识符type
;也就是分支包
;除此之外应该不需要什么了


;;消息传递

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

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

(define (apply-generic op arg) (arg op))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

;(define g (make-from-real-imag 1 2))
;(real-part g)

#|
代换模型：
(apply-generic 'real-part z)
(z 'real-part)
(dispatch 'real-part)

过程(make-from-real-imag x y)
中的参数x、y作为数据域使用，
同时返回dispatch过程匹配操作
以此抽象为消息传递
|#


;练习2.75
(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part)
           (* r (cos a)))
          ((eq? op 'imag-part)
           (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else
           (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)

;(define k (make-from-mag-ang 10 20))
;(real-part k)

;练习2.76
#|
;三种策略：带有显式分派的通用型操作，数据导向，消息传递

;a)描述在加入一个新类型或者新操作时，系统所必须的修改

新类型
-显式分派：每一种通用型选择函数添加一种类型检查及分派
给对应类型构造所有操作
-数据导向：构造对应类型的操作系统（包）
-消息传递：构造对应类型对象

新操作
-显式分派：构造对应操作的通用型选择函数，为每一种类型
构造对应操作
-数据导向：为所有类型的操作系统增加相应操作
-消息传递：为各类类型对象添加相应操作

;b)哪种组织方式最适合那些经常需要加入新类型的系统？
数据导向
答案：数据导向都很适合？


;c)哪种组织方式最适合那些经常需要加入新操作的系统？
消息传递（感觉都是消息传递）

答案：消息传递不适合增加新操作？（已有实例需要重新实例化）
|#
