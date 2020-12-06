(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

;练习2.17
(define (last-pair list)
  (if (null? (cdr list))
      (car list)
      (last-pair (cdr list))))

;练习2.18
(define (reverse list)
  (define (reve list1 list2)
    (if (null? list1)
        list2
        (reve (cdr list1) (cons (car list1) list2))))
  (reve list nil))

;练习2.19
;(define us-coins (list 1 5 10 25 50))
(define us-coins (list 5 1 10))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (first-denomination coin-values)
  (car coin-values))

(define (no-more? coin-values)
  (null? coin-values))
;表coin-values的排列顺序没有影响cc给出的回答
;参见三次测试
;可能和计算机制有关
;无论顺序如何，amount总能以各种顺序累加其方法次数
      
;练习2.20
;(define f (lambda (x y . z) ))
;然后将x、y、z应用与过程中
;(define g (lambda w ))
;有点迷，说的可能是多参数过程

(define (same-parity f . l)
  (define (judge f v)
    (if (= (remainder f 2) (remainder v 2))
        (= 0 0)
        (= 0 1)))
  (define (make-list f list)
    (cond ((null? list)
           nil)
          ((judge f (car list))
           (cons (car list) (make-list f (cdr list))))
          (else
           (make-list f (cdr list)))))
  (cons f (make-list f l)))
;正确

;练习2.21
#|
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))
|#

#|
(define (square-list items)
  (map (lambda (x) (* x x)) items))
|#


(define (square-list items)
  (if (null? items)
      nil
      (cons ((lambda (x) (* x x)) (car items))
            (square-list (cdr items)))))

;(square-list (list 1 2 3 4))

;练习2.22
(define square (lambda (x) (* x x)))
#|
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))
|#
;为什么相反？
;(square-list (list 1 2 3 4))

#|
(iter items nil)
(iter (1 2 3 4) nil)
(iter (2 3 4) (cons (square 1) nil))
(iter (3 4) (cons (square 2) (cons (square 1) nil)))
(iter (4) (cons (square 3) (cons (square 2) (cons (square 1) nil))))
|#
;用answer进行组合的顺序是从内到外的

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

;(square-list (list 1 2 3 4))          
;这次错得更离谱
;把指针连接到结点的左边去了

;练习2.23

(define (for-each proc items)
  (define (do-next)
    (display (proc (car items)))
    (for-each proc (cdr items)))
  (if (null? items)
      (newline)
      (do-next)))

;(for-each (lambda (x) (newline) (display x))
;          (list 57 321 88))

;练习2.24
#|
(list 1 (list 2 (list 3 4)))
{mcons
 1
 {mcons
  {mcons
   2
   {mcons
    {mcons 3 {mcons 4 '()}}
    '()}}
  '()}}
|#

;练习2.25
#|
(define first (list 1 3 (list 5 7) 9))
(car (cdr (car (cdr (cdr first)))))
;注：第一个car是取值，其余都是取结点的操作

(define second (list (list 7)))
(car (car second))

(define third (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(car (cdr (car (cdr (car (cdr (car (cdr
                                    (car
                                         (cdr
                                              (car
                                                   (cdr third))))))))))))
|#

;练习2.26
(define x (list 1 2 3))
(define y (list 4 5 6))

#|
(append x y)
{mcons
 1
 {mcons
  2
  {mcons
   3
   {mcons
    4
    {mcons 5 {mcons 6 '()}}}}}}

(cons x y)
{mcons
 {mcons
  1
  {mcons 2 {mcons 3 '()}}}
 {mcons
  4
  {mcons 5 {mcons 6 '()}}}}

(list x y)
{mcons
 {mcons
  1
  {mcons 2 {mcons 3 '()}}}
 {mcons
  {mcons
   4
   {mcons 5 {mcons 6 '()}}}
  '()}}
|#

;练习2.27
;反转子树
(define (deep-reverse list)
  (let ((new (reverse list)))
    (if (null? new)
        nil
        (cons (reverse (car new)) (deep-reverse (cdr new))))))

       

#|
(deep-reverse x)
(list (list 4 3) (list 2 1))
(list (list 1 2) (list 3 4))
(list (list 1 2) (list 3 4) (list 5 6))
|#

;{mcons {mcons 1 {mcons 2 '()}} '()}
;{mcons {mcons 4 {mcons 3 '()}} {mcons {mcons 2 {mcons 1 '()}} '()}}
;{mcons
; {mcons 1 {mcons 2 '()}}
; {mcons {mcons 3 {mcons 4 '()}} {mcons {mcons 5 {mcons 6 '()}} '()}}}

;练习2.28

#|
(define (fringe list)
  (define (rec-fringe tlist llist)
    (cond ((null? llist) nil)
          ((null? tlist) (rec-fringe (cdr llist) (cdr llist)))
          ((pair? (car tlist)) (rec-fringe (car tlist) llist))
          (else (cons (car tlist) (rec-fringe (cdr tlist) llist)))))
  (rec-fringe list list))
|#
  

(define x (list (list 1 2) (list 3 4)))
;(fringe x)

;(fringe (list x x))
;仍有问题

;下面这个解决方案应该是对的，但是哪儿没想出来？

(define (fringe tree)
  (cond ((null? tree)
         nil)
        ((not (pair? tree))
         (list tree))
        (else (append (fringe (car tree))
                      (fringe (cdr tree))))))


;(fringe (list x x))
;实在是令人崩溃,这里是看了答案
;倒在了没有办法粘合叶子
;上面的实现是将叶子变成一个单链表，然后用append粘合

;练习2.29

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))
;尴尬的发现实现其实是一样的

(define (branch? branch)
  (and (not (pair? (branch-length branch)))
       (pair? (branch-structure branch))))

(define (load? branch)
  (not (pair? (branch-structure branch))))


(define (total-weight mobile)
  (cond  ((branch? mobile) (total-weight (branch-structure mobile)))
         ((load? mobile) (branch-structure mobile))
         (else (+ (total-weight (left-branch mobile))
                  (total-weight (right-branch mobile))))))

;这个构造其实不适当，没有用高层函数构造
(define m (list (list 2 (list (list 2 (list (list 4 5) (list 3 4))) (list 5 6))) (list 2 3)))
;(total-weight m)
;情况：
;二叉活动体：
;三个结点，其中两个branch、一个nil

;支杆：
;三个结点，其中一个length、一个mobile、一个nil

;负重点：
;三个结点，其中一个length、一个weigh、一个nil

(define (total-length branch)
  (cond ((branch? branch) (+ (branch-length branch)
                             (total-length (branch-structure branch))))
        ((load? branch) (branch-length branch))
        (else (+ (total-length (left-branch branch))
                 (total-length (right-branch branch))))))

;(total-length (left-branch m))

(define (balance? mobile)
  (= (* (total-length (left-branch mobile))
        (total-weight (left-branch mobile)))
     (* (total-length (right-branch mobile))
        (total-weight (right-branch mobile)))))

;d)
;底层实现改变
(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define m (make-mobile (make-branch 7 (make-mobile (make-branch 8 2) (make-branch 7 2)))
                       (make-branch 8 (make-mobile (make-branch 7 8) (make-branch 9 8)))))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))

(define (branch? branch)
  (and (not (pair? (branch-length branch)))
       (pair? (branch-structure branch))))

(define (load? branch)
  (not (pair? (branch-structure branch))))

(define (total-weight mobile)
  (cond  ((branch? mobile) (total-weight (branch-structure mobile)))
         ((load? mobile) (branch-structure mobile))
         (else (+ (total-weight (left-branch mobile))
                  (total-weight (right-branch mobile))))))

(define (total-length branch)
  (cond ((branch? branch) (+ (branch-length branch)
                             (total-length (branch-structure branch))))
        ((load? branch) (branch-length branch))
        (else (+ (total-length (left-branch branch))
                 (total-length (right-branch branch))))))

;从第二层抽象开始就不需要改实现了
;第三层抽象也是完全没动
;不错的实现

;练习2.30
(define (square x)
  (* x x))

(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       tree))
;map过程通过car、cdr来推进递归

(define m1 (list 1 (list 2 (list 3 4) 5)
                 (list 6 7)))
;(square-tree m1)

;练习2.31

(define (square-tree tree) (tree-map square tree))

(define (tree-map proc tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (proc tree))
        (else (cons (tree-map proc (car tree))
                    (tree-map proc (cdr tree))))))
;成功

;练习2.32


(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x)
                            (cons (car s) x))
                          rest)))))


(define m2 (list 1 2 3))
;(subsets m2)
;看答案
;怎么会用到组合数学？？？
;难度其实很大

#|
1.空集？
返回空链表
2.非空
粘合rest以及rest的
|#

;(1 2 3)
;(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))


;(1 2 3 4)
;(2 3 4) (3 4) (4)
;(() (4) (3) (3 4) (2) (2 3) (2 4) (2 3 4) (1) (1 4) (1 3) (1 2 3) (1 2 3) (1 2 3 4))
;规律显然是有的，但过程为什么要那么定义

;练习2.33
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

#|
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))
|#

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))
;x:car
;y:递归项

;(length (list 1 2 3 4 5 6 10))

;练习2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ (* x higher-terms) this-coeff))
              0
              coefficient-sequence))

;(horner-eval 2 (list 1 3 0 5 0 1))

;练习2.35

#|
另一种实现
(define (count-leaves t)
  (accumulate (lambda (x y) (+ 1 y))
              0
              (fringe t)))
|#

;在map的proc上做文章

;map:对所有节点实施某种过程（包括非数据节点）
;即全表映射

;(define x (cons (list 1 5 6) (list 3 4 5)))
;(define y (cons 1 2))

#|
错误答案
(define (count-leaves t)
  (accumulate + 0 (map (lambda (x)
                       (cond ((not (pair? x)) 1)
                             (else (+ (count-leaves (car t))
                                      (count-leaves (cdr t)) x)))) t)))
(count-leaves x)

;愚蠢地多了一个x（else里）
;sequence部分返回(list 4 1 1)
;愚蠢地把t当成了x（else里）
|#
;count-leaves:
;x的car的count-leaves
;与x的cdr的count-leaves之和

;看答案的

(define (count-leaves tree)
  (accumulate +
              0
              (map (lambda (sub-tree)
                     (if (pair? sub-tree)
                         (count-leaves sub-tree)
                         1))
                   tree)))
;(count-leaves x)

;答案规则：
;1.节点是叶子节点，如果是这样的话，那么返回1，作为这个节点的树叶数量
;2.节点有左右两个分支，
;那么这个节点的树叶数量就是这个节点调用count-leaves函数的结果

#|
;自己答案改进
(define (count-leaves t)
  (accumulate + 0 (map (lambda (x)
                       (cond ((not (pair? x)) 1)
                             (else (+ (count-leaves (car x))
                                      (count-leaves (cdr x)))))) t)))
(count-leaves x)
;还是错了，递归到底层时t会变成数，此时会报错
;(define x (cons (list 1 2) (list 3 4)))
|#

;练习2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define x (list (list 1 2 3) (list 4 5 6)
                (list 7 8 9) (list 10 11 12)))
;(accumulate-n + 0 x)
;没想到一次就成了，猜的成分有点大

;练习2.37
(define x (list (list 1 2 3 4) (list 4 5 6 6)
                (list 6 7 8 9)))
(define y (list 1 2 3))
(define z (list (list 1) (list 2) (list 3) (list 4)))

(define (common-product c v)
  (tree-map (lambda (x) (* c x)) v))

(define (vector-sum v)
  (accumulate + 0 v))

(define (matrix-sum w)
  (accumulate + 0 (map (lambda (x)
                         (if (not (pair? x))
                             x
                             (matrix-sum x)))
                       w)))
;对这种方式还是不太理解

(define (vector-product w v)
  (accumulate + 0 (map <> w)))

(define (return-val v n)
  (if (= n 1)
      (car (car v))
      (return-val (cdr v) (- n 1))))

(define (return-val v n)
  (cond ((null? v) 0)
        ((= n 1) (car (car v)))
        (else (return-val (cdr v) (- n 1)))))

(define (vec-pro-iter w v n)
  (if (null? w)
        nil
        (cons (* (return-val v n) (car w))
                 (vec-pro-iter (cdr w) v (+ n 1)))))

(define (vec-pro w v)
  (accumulate + 0 (vec-pro-iter w v 1)))

;(vec-pro y z)
;(return-val z 3)


(define (dot-product w v)
  (accumulate + 0 (matrix-*-vector w v)))


;先定义矩阵乘法再定义点积
(define (matrix-*-vector m v)
  (map (lambda (x) (vec-pro x v)) m))

;(matrix-*-vector x z)
;(dot-product x z)
;矩阵乘法：m的每一行和v的每一列对应相乘相加

;(matrix-*-vector x y)
#|
(1 2 3) * ((1 2 3 4) (4 5 6 6) (6 7 8 9))
= (27 33 39 43)
|#

(define (transpose-v v)
  (if (null? v)
      nil
      (cons (list (car v)) (transpose-v (cdr v)))))
;只允许行向量转置

(define (vector-*-matrix v m)
  (map (lambda (x) (vec-pro v (transpose-v x))) m))

;(vector-*-matrix y x)

;(list (list 1 2 2) (list 3 4 4))
;(list (list 1 3) (list 2 4) (list 2 4))
(define (transpose mat)
  (accumulate-n cons nil mat))

;成功
;这里的转置只允许转置矩阵，向量不被视为矩阵

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (vector-*-matrix x cols)) m)))
;不是很理解为什么要用转置
;(matrix-*-matrix x x)


;练习2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (fold-right op intitial sequence)
  (accumulate op intitial sequence))
;将序列的第一个元素组合到左边所有元素的组合结果上

;(fold-right / 1 (list 1 2 3))
;3/2
;从右到左

;(fold-left / 1 (list 1 2 3))
;1/6
;从左到右

;(fold-right list nil (list 1 2 3))
;(list 1 (list 2 (list 3 nil)))

;(fold-left list nil (list 1 2 3))
;(list (list (list nil 1) 2) 3)
;运算op满足交换律和结合律

;练习2.39

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))
;(reverse y)
;可能要用cond
;看答案吧，想不出来。
;写答案的大佬太厉害了。

;把之前的实现忘得差不多了
;下次再想吧

#|
(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))
;成功
|#


;练习2.40

(define (square x)
  (* x x))

(define (even? n)
  (= (remainder n 2) 0))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (prime? n)
  (define (fast-prime? n times)
    (define (fermat-test n)
      (define (try-it a)
        (= (expmod a n n) a))
      (try-it (+ 1 (random (- n 1)))))
    (cond ((= times 0) true)
          ((fermat-test n) (fast-prime? n (- times 1)))
          (else false)))
  (fast-prime? n 10))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum (filter prime-sum? (unique-pair n))))
;(prime-sum-pairs 6)

(define (unique-pair n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

#|
详解：
1.
flatmap的lambda参数：
(enumerate-interval 1 (- i 1)) 返回(list 1 2 3 ... (- i 1))
例：(enumerate-interval 1 (- 6 1)) 返回(list 1 2 3 4 5)

2.
(map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1)))
将生成数的list转换为序对的list
例：(map (lambda (j) (list 6 j)) (enumerate-interval 1 (6 1)))
生成(list (list 6 1) (list 6 2) (list 6 3) ... (list 6 6))
(list 1 2 3 4 5 6) -> (list (list 6 1) (list 6 2) ... (list 6 6))
3.
(lambda (i) (map (lambda (j) (list i j))
                 (enumerate-interval 1 (- i 1))))
将给定i映射为从1到i生成序对的list
例：((lambda (i) (map (lambda (j) (list i j))
                 (enumerate-interval 1 (- i 1)))) 6)
生成(list (list 6 1) (list 6 2) (list 6 3) ... (list 6 6))


4.
(flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n))
n -> (list 1 ... n) -> 将列表中每一元素映射为序对的list，然后将他们组合起来

|#


;(prime-sum-pairs 6)
;完成

;练习2.41
#|
(define (three-unit n)
  (flatmap (lambda (i)
             (map (lambda (j) (list j i))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))
|#

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (pair n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 n)))
           (enumerate-interval 1 n)))
;仍旧是不好理解

(define (triad n)
  (flatmap (lambda (i)
             (map (lambda (j) (cons i j))
                  (pair n)))
           (enumerate-interval 1 n)))

(define (triad-sum s)
  (cond ((null? s) 0)
        ((not (pair? (car s)))
         (+ (car s) (triad-sum (cdr s))))
        (else
         (triad-sum (car s)))))

(define (equal-triad-sum? s n)
  (= (triad-sum s) n))

;最终程序
(define (equal-sum-triad s)
  (filter (lambda (x) (equal-triad-sum? x s)) (triad s)))


;(equal-sum-triad 6)
;成功


;练习2.42

#|
;先把filter去掉试试
(define empty-board 0)

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row board-size rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1)))))
  (queen-cols board-size))

;程序执行的顺序是什么？

;(list 1 2)
(define (adjoin new-row rest-of-queens)
  (cond ((null? rest-of-queens) nil)
        ((= 1 new-row)
         (cons (append (car rest-of-queens) (list 1))
               (adjoin (- new-row 1) (cdr rest-of-queens))))
        (else (cons (append (car rest-of-queens) (list 0))
                    (adjoin (- new-row 1) (cdr rest-of-queens))))))

(define (adjoin-ini-iter board-size new-row)
  (cond ((= 1 new-row)
         (cons (list 1) (adjoin-ini-iter (- board-size 1) (- new-row 1))))
        ((= 0 board-size) nil)
        (else
         (cons (list 0) (adjoin-ini-iter (- board-size 1) (- new-row 1))))))

(define (adjoin-ini board-size new-row)
  (list (adjoin-ini-iter board-size new-row)))

(define (adjoin-position new-row board-size rest-of-queens)
  (if (not (pair? rest-of-queens))
      (adjoin-ini board-size new-row)
      (adjoin new-row (car rest-of-queens))))
                                      
                                       
(queens 3)
;明天搞定层次的问题

;(adjoin 1 t)

#|
(adjoin-ini 2 1)

(define t (list (list (list 1)
                      (list 0))))
t
(define i (list (list (list 0)
                      (list 1))))

(append t i)

(define g (list
           (list (list 1)
                 (list 0))
           (list (list 0)
                 (list 1))))
g
|#

;(adjoin-ini-iter 2 1)

;(define t (list nil))
;(adjoin-position 1 0 t)
;成功

#|
(define empty-board 0)
k=0时，(list 0) ->(list         //k为1
                          (list (list 1)
                                (list 0))

                          (list (list 0)
                                (list 1))) ->

(list        //k为2
 (list (list 1 1)
       (list 0 0))

 (list (list 1 0)
       (list 0 1))

 (list (list 0 1)
       (list 1 0))

 (list (list 0 0)
       (list 1 1)))
2皇后无解

;似乎总是要用到board-size
|#

#|
(queen-cols 1) -> 执行else -> (filter ...) -> (flatmap ...) ->
(queen-cols 0) -> (list empty-board)，弹出此层 -> (flatmap ...)
（此时可以推断出有八种空格局）-> 为每种格局添加一个皇后的位置
|#
;先从board-size为2的情况做起

#|
思考：为2时，此时empty-board中应该有两个空格局
empty-board无法根据board-size来创建格局

我仍旧认为adjoin-position中的k是不需要的
|#


;那个flatmap是皇后位置的集合，最终是用filter筛

#|
(define (queen-cols k)
  (if (= k 0)
      (list empty-board)
      (filter
       (lambda (positions) (safe? k position))
       (flatmap
        (lambda (rest-of-queens)
          (map (lambda (new-row)
                 (adjoin-position new-row k rest-of-queens))
               (enumerate-interval 1 board-size)))
        (queen-cols (- k 1))))))
|#

;new-row：枚举间隔的每个节点是它的实参，不用管
;递归：
;理解这个程序的难度相当大

;明天再想，写出一个类似结构的程序
   
;这节练习做完后去整理知识

#|
;行扩充实现
(define (make-col-iter n c bs)
  (cond ((= c bs) nil)
        ((= c n) (cons 1 (make-col-iter n (+ 1 c) bs)))
        (else (cons 0 (make-col-iter n (+ 1 c) bs)))))
        
(define (make-col n bs)
  (make-col-iter (- n 1) 0 bs))

(define test
  (lambda (rest-of-queens)
    (map (lambda (new-row)
           (adjoin-position new-row rest-of-queens))
         (enumerate-interval 1 5))))
;生成5个格局集合:每个格局集合包含一个新的皇后位置

(define (adjoin-position new-row rest-of-queens)
  (append rest-of-queens (list (make-col new-row 5))))

;(define k (list (list 1 0 0 0 0) (list 0 0 0 1 0)))
(define t (list (list 0 0 0 0 0)
                (list 0 0 0 0 0)
                (list 0 0 0 0 0)))

;(append k (list (list 1 0 0 0 0)))
;就是如此粘合
|#

;每次在每一列里放一个皇后
;生成出将下一个皇后放在第k列中每一行的扩充集合
;而后过滤他们，只留下能使位于第k列的皇后与其他皇后相安无事的那些扩充

;明明按行扩充要简单得多

#|
(define t (list (list 0) (list 0) (list 0) (list 0)))
;(define g (list (list 0 0) (list 0 1) (list 0 0) (list 0 0)))

(define (make-col-rec new-row seq)
  (cond ((null? seq) nil)
        ((= 0 new-row)
         (cons (append (car seq) (list 1))
               (make-col-rec (- new-row 1) (cdr seq))))
        (else
         (cons (append (car seq) (list 0))
               (make-col-rec (- new-row 1) (cdr seq))))))

(define (adjoin-position new-row k rest-of-queens)
  (make-col-rec (- new-row 1) rest-of-queens))

(define test (lambda (rest-of-queens)
          (map (lambda (new-row)
                 (adjoin-position new-row 2 rest-of-queens))
               (enumerate-interval 1 4))))


(define (queens board-size)
  (define (make-ini board-size)
    (define (make-ini-iter board-size i)
      (if (= 1 board-size)
          i
          (make-ini-iter (- board-size 1)
                         (append i (list (list 0))))))
    (make-ini-iter board-size (list (list 0))))
  (define (make-queens queens board-size)
    (map (lambda (new-row)
           (adjoin-position new-row queens))
         (enumerate-interval 1 board-size)))
  (define (queen-cols k queens)
    (if (= k board-size)
        queens
        (queen-cols (+ 1 k)
                    (fliter
                     (lambda (positions) (safe? k positions))
                     (make-queens queens)))))
  (queen-cols 0 (make-ini board-size)))
;未完成的实现
|#

|#

;直接看了答案，发现和我的想法完全不一样hhhhh

;"因为题目要求给出八皇后问题的所有解法,所以 queens 求出的最终结果
;将是一个二维列表:
;(list (list 6 3 1 7 5 8 2 4) (list ...) (list ...) ...) 。"



(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))
;k用不上

(define (safe? k position)
  (iter-check (car position)
              (cdr position)
              1))

(define (iter-check row-of-new-queen rest-of-queens i)
  (if (null? rest-of-queens)
      (let ((row-of-current-queen (car rest-of-queens)))
        (if (or (= row-of-new-queen row-of-current-queen)
                (= row-of-new-queen (+ i row-of-current-queen))
                (= row-of-new-queen (- row-of-current-queen i)))
            (iter-check row-of-new-queen
                        (cdr rest-of-queens)
                        (+ i 1))))))


;练习2.43

;因为对(queen-cols (- k 1))进行map需要耗费极多的时间
;假设对queen-cols执行map的时间为a（函数），对enumerate-interval进行
;flatmap的时间为c（常数），adjoin过程的执行时间为b（不是特别大）

;(cab)^k
;(cab)*k
;（有瞎计算的成分）交换后的复杂度是指数级，原先是一元

;看了答案，没太理解
