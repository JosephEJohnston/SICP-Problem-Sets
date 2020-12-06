#|
(define a 1)
(define b 2)
;(list a b)

(list 'a 'b)

(list 'a b)

(car '(a b c))

(cdr '(a b c))

(car (quote (x y z)))
|#

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

;(memq 'apple' (pear banana prune))

;(memq 'apple' (x (apple sauce) y apple pear))

;练习2.53
;(list 'a 'b 'c)

;(list (list 'george))

;(pair? (quote ((x1 x2) (y1 y2))))
;(list (quote (x1 x2)) (quote (y1 y2)))

;(cdr '((x1 x2) (y1 y2)))

;(cadr '((x1 x2) (y1 y2)))

;(pair? '(a short list))
;(pair? (car '(a short list)))

;(memq 'red '((red shoes) (blue socks)))

;(memq 'red '(red shoes blue socks))


;练习2.54

(define (equal? x y)
  (cond ((and (null? x) (null? y)) true)
        ((and (pair? x) (pair? y))
         (and (eq? (car x) (car y))
              (equal? (cdr x) (cdr y))))
        (else false)))


#|
(define (equal? x y)
  (if (and (pair? x) (pair? y))
      (and (eq? (car x) (car y))
           (equal? (cdr x) (cdr y)))
      false))
;上面没注释的是对的
|#

;(equal? '(this is red) '(this (is) red))

;练习2.55

;(car '' abracadabra)

;其实就是这样：(car (quote (quote abracadabra)))

