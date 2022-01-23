(require "number_system.rkt")


;---------------------------------------------稀疏多项式---------------------------------------
(define (install-sparse-polynomial-package)
  (define (make-from-sparse-poly variable term-list)
    (cons variable term-list))

  ;;没试过,不测试了。
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


  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2))))
                (let ((rest-of-result (div-terms
                                       (sub-terms L1
                                                 (mul-terms L2 (list (make-term new-o new-c)))) L2)))
                       (list (adjoin-term (make-term new-o new-c)
                                          (car rest-of-result))
                             (cadr rest-of-result))))))))


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


  (define (make-poly-from-items var item)
    (make-from-sparse-polynomial var (list item)))

  
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

;---------------------------------------稠密多项式--------------------------------------------------------------------
(define (install-dense-polynomial-package)
  (define (make-from-dense-poly variable term-list)
    ;term-list要不要类型化?或者说对term要不要类型化?
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
  
  (define (minus-poly p)
    (define (minus-iter ts)
      (if (null? ts)
          '()
          (cons (minus (first-term ts))
                (minus-iter (rest-terms ts)))))
    (minus-iter (term-list p)))

  (define (=zero?-poly p)
    ;为#f时其实可以直接截断,但懒得改就算了。
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

;--------------------------------安装包-------------------------------------

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




(define (make-term order coeff) (list order coeff))
(define (variable p) (car p))
(define (term-list p) (cdr p))
(define (make-from-sparse-polynomial var terms)
  ((get 'make-from-sparse-poly 'polynomial) var terms))
(define (make-from-dense-polynomial var terms)
  ((get 'make-from-dense-poly 'polynomial) var terms))



(install-sparse-polynomial-package)
(install-dense-polynomial-package)
(install-polynomial-package)



;;