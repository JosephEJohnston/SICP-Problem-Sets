(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))


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

;;选择函数
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

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))


;----------------------------------------------------------

;;实数
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (define (raise-scheme-number x)
    (make-complex-from-real-imag x 0))
  (define (minus-scheme-number z)
    (- 0 z))

   
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
  (put 'minus '(scheme-number) minus-scheme-number)
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
  (define (minus-rational z)
    (make-rat (- 0 (numer z)) (denom z)))

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
  (put 'minus '(rational) minus-rational)
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
    (cons x y))
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
  (define (minus-rectangular z)
    (make-complex-from-real-imag (- 0 (real-part z)) (- 0 (imag-part z))))
  

  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       ;将用make-from-real-imag生成的数据对象用tag包裹
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (attach-tag 'polar (make-from-mag-ang r a))))
  (put 'equ? '(rectangular rectangular)
       (lambda (x y) (equ-rectangular? x y)))
  (put '=zero? '(rectangular) =zero-rectangular?)
  (put 'project '(rectangular) project-rectangular)
  (put 'minus '(rectangular) minus-rectangular)
  'done)

(define (install-polar-package)
  (define (magnitude z) (car z))
  ;幅度制，不是角度
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
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
  (define (minus-polar z)
    (make-complex-from-mag-ang (magnitude z) (+ (angle z) 3.14159)))
  
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (attach-tag 'rectangular (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'equ? '(polar polar)
       (lambda (x y) (equ-polar? x y)))
  (put '=zero? '(polar) =zero?)
  (put 'minus '(polar) minus-polar)
  'done)

(define (make-from-real-imag r a)
  ((get 'make-from-real-imag 'polar) r a))

(define (make-from-mag-ang x y)
  ((get 'make-from-mag-ang 'rectangular) x y))
  
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? z) (apply-generic '=zero? z))
(define (project z) (apply-generic 'project z))
(define (minus z) (apply-generic 'minus z))


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
  (put 'minus '(complex) minus)
  
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


(install-scheme-number-package)
(install-rectangular-package)
(install-rational-package)
(install-polar-package)
(install-complex-package)



(define coercion-table (make-table))
(define get-coercion (coercion-table 'lookup-proc))
(define put-coercion (coercion-table 'insert-proc!))



(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (eq? type1 type2)
                    (error "No method for these types"
                           (list op type-tags))
                    (if (eq? (super type1 type2) type1)
                        (apply-generic op a1 (get-type-num type1 a2))
                        (apply-generic op (get-type-num type2 a1) a2))))
              (error "No method for these types"
                     (list op type-tags)))))))

                


(define (get-type-num type num)
  (if (eq? type (type-tag num))
      num
      (get-type-num type (raise num))))

(define (super type1 type2)
  ;添加新类型时改这个tow
  (define tow '(polynomial complex rational scheme-number))
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

(define (drop num)
  (if (not (eq? (type-tag num) 'scheme-number))
      (let ((drop-num (project num)))
        (if (equ? num drop-num)
            (drop drop-num)
            num))
      num))

;;上一节的内容，改掉指派函数的自动下降


;;本节的多项式系统

(define (install-sparse-polynomial-package)
  (define (make-from-sparse-poly variable term-list)
    (cons variable term-list))

  ;;没试过，不测试了。
  (define (make-from-dense-poly variable term-list)
    (define (change-sparse-to-dense term-list)
      (define (change-iter term-list n)
        (let ((f-t-coeff (coeff (first-term (term-list)))))
          (cond ((= n 0)
                 (cons f-t-coeff '()))
                ((= n f-t-coeff)
                 (cons f-t-coeff
                       (change-iter (rest-terms (term-list)) (- n 1))))
                ((not (= n f-t-coeff))
                 (cons 0 (change-iter term-list (- n 1))))
                (else
                 (error "Unknown ERROR -- Change-Iter" (list term-list n))))))
      (change-iter term-list (coeff (first-term (term-list)))))
    (cons variable (change-sparse-to-dense term-list)))
  
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                ;用了通用型加法过程add来归并系数
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))

  (define (sub-terms L1 L2)
    (cond ((and (empty-termlist? L1) (empty-termlist? L2))
           (the-empty-termlist))
          ((empty-termlist? L1) (minus-terms L2))
          ((empty-termlist? L2) (minus-terms L1))
          (else
           (let ((t1 (first-term L1))
                 (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     (minus-term t1) (sub-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     (minus-term t2) (sub-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (sub (coeff t1) (coeff t2)))
                     (sub-terms (rest-terms L1)
                                (rest-terms L2)))))))))

  
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      ;基于通用型mul
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))

#|
  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        ;最后的返回结果为什么是两个空表？？？
        ;一个商式，一个余式
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              ;当被除式次数小于除式，长除结束
              ;递归末尾的L1为余式
              (list (the-empty-termlist) L1)
              ;步骤1
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2))))
                (let ((rest-of-result
                       ;步骤2和3？
                       (div-terms (sub-terms L1
                                             (mul-terms (list (make-term new-o new-c)) L2)) L2)))
                  (add-terms (list (make-term new-o new-c))
                             rest-of-result)))))))
|#

  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        ;被除式为空表的情况
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              ;长除结束
              ;返回结果为一个商式一个余式
              (list (the-empty-termlist) L1)
              ;执行长除法
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2))))
                (let ((rest-of-result (div-terms
                                       (sub-terms L1
                                                 (mul-terms L2 (list (make-term new-o new-c)))) L2)))
                       ;需要每一轮的最高次项
                       ;如何传递余式？
                       (list (adjoin-term (make-term new-o new-c)
                                          (car rest-of-result))
                             (cadr rest-of-result))))))))

  ;成功了！！！！！

  
  ;递归结束后的上一层是什么样的？
  ;这道题是真的不好做
  ;程序写完，下一次测试
  
#|

  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2))))
                (let ((rest-of-result
                       <递归地计算结果的其余部分>
                       ))
                  <形成完整的结果>
                  ))))))
|#

  (define (minus-poly p)
    (define (minus-iter ts)
      (let ((term (first-term ts))
            (rs-terms (rest-terms ts)))
        (if (null? rs-terms)
            (cons (make-term (order term) (minus (coeff term)))
                  '())
            (cons (make-term (order term)
                             (minus (coeff term)))
                  (minus-iter rs-terms)))))
    (minus-iter (term-list p)))

  (define (minus-terms l)
    (let ((f-t (first-term l))
          (r-t (rest-terms l)))
      (if (null? r-t)
          (cons (minus-term f-t) '())
          (cons (minus-term f-t) (minus-terms r-t)))))
          

  (define (=zero?-poly p)
    (define (=zero?-iter ts)
      (let ((f-coeff (coeff (first-term ts)))
            (rs-terms (rest-terms ts)))
        (if (empty-termlist? rs-terms)
            (=zero? f-coeff)
            (and (=zero? f-coeff) (=zero?-iter rs-terms)))))
    (=zero?-iter (term-list p)))
  
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-from-sparse-poly (variable p1)
                               (add-terms (term-list p1)
                                          (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))

  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-from-sparse-poly (variable p1)
                               (sub-terms (term-list p1)
                                          (term-list p2)))
        (error "Polys not in same var -- SUB-POLY"
               (list p1 p2))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-from-sparse-poly (variable p1)
                               (mul-terms (term-list p1)
                                          (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))

  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((result-terms (div-terms (term-list p1)
                                       (term-list p2))))
          (list (make-from-sparse-poly (variable p1)
                                      (car result-terms))
                (make-from-sparse-poly (variable p1)
                                      (cadr result-terms))))
                                
        (error "Polys not in same var -- DIV-POLY"
               (list p1 p2))))

  ;人很容易做到这一过程，但程序则相反
  ;难以描述

  ;如果碰到带x的项：刨出这个项
  ;怎么刨？
  #|
  (define (make-variable-poly var p)

    ;想新创建一个 poly 来存储新的多项式
    (define (make-variable-poly-inner var cur-var cur-term-list acc-p)
      ())
        
    
    (let ((current-term-list (term-list p))
          (current-var (variable p))
          (tmp-poly (make-from-sparse-poly var (list (make-term 0 0))))
          (acc-term-list (term-list tmp-poly)))
      (make-variable-poly-inner var current-var current-term-list acc-term-list)))
|#

  #|
  (define (make-variable-poly p tower)
    (let ((cur-var (variable p)))
      (if (eq? cur-var tower)
          ;查找项表中的多项式，进入下层递归
          (map (lambda (l)
                 (if (poly? l)
                     (make-variable-poly l tower)
                     l))
               (term-list p)))))
          ;查找多项式，递归进入下一层，查找与本层var相同的多项式
          ;太乱了
          ;你想做什么？
          ;不如先从查找单个var多项式的过程写起
|#

  ;就算找到了又怎么办？
  ;将对应参数的最小多项式连接起来？
  ;很绝望


  ;或者将所有项做成单项多项式？
  ;都做成 x 型的，然后再进行合并
  ;好像可行


  ;没法对其中的每一个多项式都进行这类操作
  ;用map。


  (define (make-poly-from-items var item)
    (make-from-sparse-polynomial var (list item)))

  #|
  (define (test-poly p)
    (define (poly? t)
      (pair? t))
    (map (lambda (t)
           (let ((coe-t (coeff t)))
             (cond ((null? coe-t) '())
                   ((poly? coe-t)
                    ;(test coe-t)
                    (list (variable p) (order t) (test coe-t))
                    )
                   (else
                    ;(make-poly-from-items 'x t)
                    (list (variable p) (list t))
                    ))))
           (term-list p)))
|#

  #|
  (define (test-poly p)
    (define (poly? t)
      (pair? t))
    (map (lambda (t)
           (let ((coe-t (coeff t))
                 (var-p (variable p)))
             (cond ((null? coe-t) '())
                   ((poly? coe-t) (make-poly-from-items var-p (make-term (order t) (test coe-t))))
                   (else
                    (make-poly-from-items var-p t)))))
         (term-list p))
|#)
    ;需要传递哪些信息给上层？
    ;;variable，order，本项

  ;如果是多项式，递归使用test
  ;下次继续
  ;可能需要用除法实现中用到的技巧
  ;;可能不需要，现在的问题是如何把每项制成的多项式分离出来

  (define (test-poly p)
    (define (poly? t)
      (pair? t))
    (define (test-iter v t)
      (if (list is null)
          '()
          (cons make-str-term (test-iter v next-term-list))))
    (define (make-str-term)
      (if (term: coeff is not poly)
          (get the track and make a data structure)
          (keep the track and get into the poly)))
    (test-iter (variable p) (term-list t)))

  
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (minus-term term) (make-term (order term) (minus (coeff term))))
  (define (make-term order coeff) (list order coeff))
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (the-empty-termlist) '())
  (define (empty-termlist? term-list) (null? term-list))
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))

  
  (define (tag p) (attach-tag 'sparse-poly p))

  (put 'minus-poly '(sparse-poly) minus-poly)
  (put '=zero? '(sparse-poly) =zero?-poly)
  
  (put 'make-from-sparse-poly 'sparse-poly
       (lambda (v t) (tag (make-from-sparse-poly v t))))
  (put 'make-from-dense-poly 'sparse-poly
       (lambda (v t) (tag (make-from-dense-poly v t))))
  
  (put 'add-poly '(sparse-poly sparse-poly)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub-poly '(sparse-poly sparse-poly)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul-poly '(sparse-poly sparse-poly)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'div-poly '(sparse-poly sparse-poly)
       (lambda (p1 p2) (tag (div-poly p1 p2))))
  (put 'test '(sparse-poly)
       (lambda (p) (test-poly p)))
  'done)

(define (install-dense-polynomial-package)
  (define (make-from-dense-poly variable term-list)
    ;term-list要不要类型化？或者说对term要不要类型化？
    (cons variable term-list))

  (define (make-from-sparse-poly variable term-list)
    (define (change-dense-to-sparse len term-list)
      (let ((order (- len 1))
            (coeff (first-term term-list))
            (r-ts (rest-terms term-list)))
        (if (null? r-ts)
            (cons (list order coeff) r-ts)
            (cons (list order coeff) (change-dense-to-sparse (- len 1) (rest-terms term-list))))))
    
    (let ((len (length-terms term-list)))
      (cons variable (change-dense-to-sparse len term-list))))

  
  (define (add-terms t1 t2)
    (define (combine-terms t1 t2 n)
      (cond ((> n 0) (cons (first-term t1)
                           (combine-terms (rest-terms t1)
                                          t2 (- n 1))))
            ((null? t1) '())
            (else
             (cons (add (first-term t1)
                        (first-term t2))
                   (combine-terms (rest-terms t1)
                                  (rest-terms t2) 0)))))
    (let ((l1 (length-terms t1))
          (l2 (length-terms t2)))
      (cond ((> l1 l2)
             (combine-terms t1 t2 (- l1 l2)))
            ((> l2 l1)
             (combine-terms t2 t1 (- l2 l1)))
            (else
             (combine-terms t1 t2 0)))))

  (define (sub-terms t1 t2)
    (define (minus-terms ts)
      (map (lambda (t) (minus t)) ts))
    (add-terms t1 (minus-terms t2)))

  ;n与length和term的位置有关系
  ;n:在末尾填补零的个数
  ;n = length-2
  (define (mul-terms t1 t2)
    (mul-terms-iter t1 t2 (- (length-terms t2) 1)))

  (define (mul-term-by-all-terms t1 ts2 n)
    (define (cons-zeros n)
      (if (= 0 n)
          '()
          (cons 0 (cons-zeros (- n 1)))))
    (if (null? t1)
        (cons-zeros n)
        (cons (mul (first-term t1) ts2)
              (mul-term-by-all-terms (rest-terms t1) ts2 n))))
  
  (define (mul-terms-iter t1 t2 n)
    (if (null? t2)
        '()
        (add-terms
         (mul-term-by-all-terms t1 (first-term t2) n)
         (mul-terms-iter t1 (rest-terms t2) (- n 1)))))

  ;(define (div-terms t1 t2)
   ; ())
  
  (define (minus-poly p)
    (define (minus-iter ts)
      (if (null? ts)
          '()
          (cons (minus (first-term ts))
                (minus-iter (rest-terms ts)))))
    (minus-iter (term-list p)))

  (define (=zero?-poly p)
    ;为#f时其实可以直接截断，但懒得改就算了。
    (define (=zero?-iter ts)
      (let ((f-t (first-term ts))
            (r-t (rest-terms ts)))
        (if (null? r-t)
            (=zero? f-t)
            (and (=zero? f-t) (=zero?-iter r-t)))))
    (=zero?-iter (term-list p)))
  
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-from-dense-poly (variable p1)
                              (add-terms (term-list p1)
                                         (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))

  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-from-dense-poly (variable p1)
                              (sub-terms (term-list p1)
                                         (term-list p2)))
        (error "Polys not in same var -- SUB--POLY"
               (list p1 p2))))
  
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-from-dense-poly (variable p1)
                              (mul-terms (term-list p1)
                                         (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))

  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-from-dense-poly (variable p1)
                               (div-terms (term-list p1)
                                          (term-list p2)))
        (error "Polys not in same var -- DIV--POLY"
               (list p1 p2))))
                                                    

  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (first-term p) (car p))
  (define (rest-terms p) (cdr p))
  (define (minus-term term) (minus term))
  (define (length-terms ts)
    (if (null? ts)
        0
        (+ 1 (length-terms (rest-terms ts)))))
  (define (tag p) (attach-tag 'dense-poly p))


  (put 'minus-poly '(dense-poly) minus-poly)
  (put '=zero? '(dense-poly) =zero?-poly)
  
  (put 'add-poly '(dense-poly dense-poly)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub-poly '(dense-poly dense-poly)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul-poly '(dense-poly dense-poly)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'div-poly '(dense-poly dense-poly)
       (lambda (p1 p2) (tag (div-poly p1 p2))))
  (put 'make-from-dense-poly 'dense-poly
       (lambda (v t) (tag (make-from-dense-poly v t))))
  (put 'make-from-sparse-poly 'dense-poly
       (lambda (v t) (make-from-sparse-poly v t)))
  'done)



(define (install-polynomial-package)
  (define (make-from-sparse-poly variable term-list)
    ((get 'make-from-sparse-poly 'sparse-poly) variable term-list))
  (define (make-from-dense-poly variable term-list)
    ((get 'make-from-dense-poly 'dense-poly) variable term-list))
    
  
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2) (tag (div-poly p1 p2))))

  ;别忘了通用分派函数
  (put 'minus '(polynomial) minus-poly)
  (put '=zero? '(polynomial) =zero?)
  
  (put 'make-from-sparse-poly 'polynomial
       (lambda (v t) (tag (make-from-sparse-poly v t))))
  (put 'make-from-dense-poly 'polynomial
       (lambda (v t) (tag (make-from-dense-poly v t))))
  (put 'test '(polynomial)
       (lambda (p) (test p)))

  'done)

;minus之前有分配函数的
;别忘了给符号加括号。
(define (minus-poly p) (apply-generic 'minus-poly p))
(define (=zero?-poly p) (apply-generic '=zero?-poly p))
(define (add-poly p1 p2) (apply-generic 'add-poly p1 p2))
(define (sub-poly p1 p2) (apply-generic 'sub-poly p1 p2))
(define (mul-poly p1 p2) (apply-generic 'mul-poly p1 p2))
(define (div-poly p1 p2) (apply-generic 'div-poly p1 p2))
(define (test p) (apply-generic 'test p))


#|
(define (install-polynomial-package)
  
  (define (make-poly variable term-list)
    (let ((t (first-term term-list)))
      (if (number? t)
          (cons variable (dense-term-list term-list))
          (cons variable term-list))))
  (define (dense-term-list p)
    (define (dense-term-list-iter p n)
      (let ((coe (car p))
            (rest (cdr p)))
        (if (null? rest)
            (make-term n coe)
            (cons (make-term n coe) (dense-term-list-iter rest (- n 1))))))
    (define (max-order p)
      (if (null? p)
          -1
          (+ 1 (max-order (cdr p)))))
    (dense-term-list-iter p (max-order p)))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))

  ;;设计多项式减法
  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (sub-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- SUB-POLY"
               (list p1 p2))))


 
  
  (define (=zero?-poly p)
    (null? p))
  (define (minus-poly p)
    (if (null? p)
        null
        (cons (minus-term (first-term p)) (rest-terms p))))

  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? 'polynomial
       (lambda (p) (=zero?-poly p)))
  (put 'minus 'polynomial minus-poly)
  'done)
|#
    





(define (make-term order coeff) (list order coeff))
(define (variable p) (car p))
(define (term-list p) (cdr p))
(define (make-from-sparse-polynomial var terms)
  ((get 'make-from-sparse-poly 'polynomial) var terms))
(define (make-from-dense-polynomial var terms)
  ((get 'make-from-dense-poly 'polynomial) var terms))

;(define (make-from-sparse-poly var terms)
;  ((get 'make-from-dense-poly 'dense-poly) var terms))
         
;;仍旧需要上一节的通用算术系统

(install-sparse-polynomial-package)
(install-dense-polynomial-package)
(install-polynomial-package)

;;开始做题

;做题前的小测试


;(add p1 p2)

;成功


;--练习2.87
;完成，见代码

;--练习2.88

;实现多项式减法
;接下来装配每个包的取负过程

;测试：极坐标取负
#|
(define po1 (make-complex-from-mag-ang 3 (/ 3.14159 4)))
(real-part po1)
(imag-part po1)
(define po2 (minus po1))
(real-part po2)
(imag-part po2)
|#

;没有装配多项式的取负过程
;完成

;--练习2.89
;实现稠密多项式
;则在给予系数的时候要返回相应次数
;(define p3 (make-polynomial 'x (list 1 2 0 3 -2 -5)))
;程序有很大问题，下次改
;继续思考，出来的次数仍是反序的
;例如：更新平衡二叉树的length是用类似的方法
;放弃原方案，转用一般方案，完成，见代码
#|
  (define (make-poly variable term-list)
    (let ((t (first-term term-list)))
      (if (number? t)
          (cons variable (dense-term-list term-list))
          (cons variable term-list))))
  (define (dense-term-list p)
    (define (dense-term-list-iter p n)
      (let ((coe (car p))
            (rest (cdr p)))
        (if (null? rest)
            (make-term n coe)
            (cons (make-term n coe) (dense-term-list-iter rest (- n 1))))))
    (define (max-order p)
      (if (null? p)
          -1
          (+ 1 (max-order (cdr p)))))
    (dense-term-list-iter p (max-order p)))

|#


;--练习2.90
;不仅仅是局部修改，而是重新设计成两个多项式系统
;思考：如何处理两种表项，是构建包还是别的什么？

;稀疏多项式和稠密多项式的区别是什么？
;1)term-list
;然后就是以此为基础的各个过程

;给稠密多项式一个转为稀疏多项式的过程
;找到稠密多项式add-terms更好的设计
;可以在最后用(list (before terms) (after terms))呀

;顺序粘合表项似乎只能用递归法？
;好吧，放弃这种一次性提供两部分的想法
;还是得算两遍

;下一次继续写add
(define p1 (make-from-sparse-polynomial 'x (list (make-term 7 5) (make-term 2 3) (make-term 1 2) (make-term 0 7))))
(define p2 (make-from-sparse-polynomial 'x (list (make-term 4 1) (make-term 2 2/3) (make-term 0 5))))

(define p3 (make-from-dense-polynomial 'x (list 2 0 3 -2 -5)))
(define p4 (make-from-dense-polynomial 'x (list 1 2 0 3 -2 -5)))

(define p5 (make-from-dense-polynomial 'x (list 0)))
(define p6 (make-from-dense-polynomial 'x (list 1 2 3 4)))

(define p7 (make-from-sparse-polynomial 'x (list (make-term 0 0))))
;(add p3 p4)
;(mul p1 p2)

;(add p5 p6)

;(mul p5 p6)
;????之前怎么会出错的


;出了问题，下次改。
;完成dense的mul

;3)
;--接下来实现minus
;(minus p3)
;找问题吧。
;成功：sparse-poly
;成功：dense-poly

;4)
;--接下来实现sub
;(sub p6 p5)

;成功：sparse-poly
;成功：dense-poly

;5)
;--实现=zero?
;(=zero? p7)
;问题：没有向外分派
;下一次继续测试

;成功：dense-poly
;成功：sparse-poly

;empty-list与zero应当是两种情况

;6)
;--实现不同多项式之间的转换
;下次写
;(make-from-sparse-poly (variable p6) (term-list p6))
;variable和term-list可能需要分配
;下次继续


;下面是一个例子，利用极坐标中的直角坐标构造方法需要分派
;(define z1 (make-complex-from-mag-ang 1 2))
;(define z2 (make-from-real-imag (magnitude z1) (angle z1)))
;此外，添加了分派方法
;但此时符号就需要改变了
;出了问题，z2此时只有一个符号

;大概是用以做内部方法吧

;基本结束

;--练习2.91
(define p8 (make-from-sparse-polynomial 'x (list (make-term 5 1) (make-term 0 -1))))
(define p9 (make-from-sparse-polynomial 'x (list (make-term 2 1) (make-term 0 -1))))

;(div p8 p9)
;有问题，再看吧
;成功完成，就不管包装了




;;符号代数中类型的层次结构

(define a1 (make-from-sparse-polynomial 'y (list (make-term 5 1) (make-term 0 -1))))
(define a2 (make-from-sparse-polynomial 'y (list (make-term 3 2) (make-term 2 1))))
(define b1 (make-from-sparse-polynomial 'x (list (make-term 2 1) (make-term 0 -1))))
(define b2 (make-from-sparse-polynomial 'x (list (make-term 6 2) (make-term 2 1) (make-term 1 1))))

(define pt1 (make-from-sparse-polynomial 'x (list (make-term 2 a1) (make-term 1 a2))))
(define pt2 (make-from-sparse-polynomial 'y (list (make-term 1 b1) (make-term 0 b2))))


;(add pt1 pt2)

;首变元为最高级变元，其余为较低级变元

;--练习2.92

(test pt1)
;程序在678行
;pt1

