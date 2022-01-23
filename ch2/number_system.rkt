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
    ;内部的equ?并没有转到外部分派,所以出了bug
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
  ;幅度制,不是角度
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
