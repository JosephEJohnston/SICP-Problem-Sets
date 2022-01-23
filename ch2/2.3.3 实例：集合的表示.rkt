;;实例:集合的表示
#|
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))


;练习2.59
(define (union-set set1 set2)
  (cond ((or (null? set1) (null? set2)) set2)
        ((not (element-of-set? (car set1) set2))
         (cons (car set1)
               (union-set (cdr set1) set2)))
        (else (union-set (cdr set1) set2))))


(define t1 '(2 3 4 5 1 9))
(define t2 '(6 7 8 9 2 3))

|#

;练习2.60
#|
(define (adjoin-set x set)
  (cons x set))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection (cdr set1) set2))))


(define (union-set set1 set2)
  (if (or (null? set1) (null? set2))
      set2
      (cons (car set1) (union-set (cdr set1) set2))))
(union-set t1 t2)
|#
;显然这里效率是更高的
;没有过多重复元素的集合中

;;集合作为排序的表
#|
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))


;练习2.61
(define (adjoin-set x set)
  (let ((y (car set)))
    (cond ((null? (cdr set)) (list y x))
          ((= x y) set)
          ((< x y) (cons x set))
          (else (cons y (adjoin-set x (cdr set)))))))

;(adjoin-set 10 (list 1 3 5 7 9))
;注意当x>y时要先cons之前的元素
;成功

;练习2.62
(define (union-set set1 set2)
  (let ((x (car set1)) (y (car set2)))
    (cond ((null? (cdr set1)) set2)
           ((null? (cdr set2)) set1)
           ((< x y) (cons x (union-set (cdr set1) set2)))
           ((> x y) (cons y (union-set set1 (cdr set2))))
           (else (cons x (union-set (cdr set1) (cdr set2)))))))

;(union-set (list 1 2 3 5 7 8) (list 3 6 9 10))
;成功
|#

;;集合作为二叉树

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((< x (entry-set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

;练习2.63
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

;a)
(define t1 (make-tree 7
                      (make-tree 3
                                 (make-tree 1 '() '())
                                 (make-tree 5 '() '()))
                      (make-tree 9
                                 '()
                                 (make-tree 11 '() '()))))
                           
;3 9 1 5 11

(define t2 (make-tree 3
                      (make-tree 1 '() '())
                      (make-tree 7
                                 (make-tree 5 '() '())
                                 (make-tree 9 '()
                                            (make-tree 11 '() '())))))

(define t3 (make-tree 5
                      (make-tree 3
                                 (make-tree 1 '() '())
                                 '())
                      (make-tree 9
                                 (make-tree 7 '() '())
                                 (make-tree 11 '() '()))))

;(tree->list-2 t1)
;(tree->list-2 t2)
;(tree->list-2 t3)
;是,这两个过程将一棵树转换为顺序表
;b)
;不一样,若表与树有相同的结点数,则表更慢一些

;练习2.64
(define (list-tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

;极其恐怖的递归过程
(define p (list 1 2 3))
;(list-tree p)
;这个过程保证转换后的树是平衡二叉树
;平衡二叉树:(左或右)子树的结点树大约是父树的一半

;(list-tree (list 1 3 5 7 9 11))
;(list-tree (list 2 4 5 6 7))
;(list-tree (list 1 2 3 4 5 6 7 9 11))

;a)
#|
将表划分为左、中、右三个部分,
然后递归地在左、右表中进行上述过程
|#
;(list-tree (list 1 3 5 7 9 11))

;b)
;Θ(n)
;以partial-tree执行的次数来计算步数
;可以确定:步数 = 2*n + 1,当n为奇数时
;见解析图partial-tree

;练习2.65
;(define (union-set tree1 tree2))
;递归地,从子树到父树
;过程的行为是什么?
;粘合的时候要考虑结点数量
;每一层栈都有左、中、右三个部分
;可以把一个子树看成一个整体




#|
(define (union-list set1 set2)
  (let ((x (car set1)) (y (car set2)))
    (cond ((null? (cdr set1)) set2)
           ((null? (cdr set2)) set1)
           ((< x y) (cons x (union-list (cdr set1) set2)))
           ((> x y) (cons y (union-list set1 (cdr set2))))
           (else (cons x (union-list (cdr set1) (cdr set2)))))))

(define (union-set tree1 tree2)
  (let ((list1 (tree->list-1 tree1))
        (list2 (tree->list-1 tree2)))
    (list-tree (union-list list1 list2))))
|#
;(union-set t2 t3)
;这样做当然能行,但复杂度是不是Θ(n)?
;复杂度可能是Θ(n^3)
;放弃

;注:用了三个复杂度为Θ(n)的程序,但总复杂度是加而不是乘
;intersection-tree没写,忘了。

#|
答案和我基本一个做法:
(define (intersection-tree tree another)
  (list->tree
   (intersection-tree (tree->list-2 tree)
                      (tree->list-2 another))))
|#



;;集合与信息检索

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))

;练习2.66
;假设记录的集合采用二叉树实现,按照其中作为键值的数值排序

(define (lookup given-key tree-of-records)
  (cond ((null? tree-of-records) false)
        ((< given-key (key (entry tree-of-records)))
         (lookup given-key (left-branch (tree-of-records))))
        ((> given-key (key (entry tree-of-records)))
         (lookup given-key (right-branch (tree-of-records))))
        (else (entry tree-of-records))))
;完成