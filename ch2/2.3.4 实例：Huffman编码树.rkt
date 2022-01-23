(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))


(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
                (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))


(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))


;练习2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(define t1 (decode sample-message sample-tree))



#|
(define test-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-leaf 'B 2)))

test-tree
|#

;练习2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))
;这些程序似乎对你而言还没有什么印象，感触不深吧。
;把关键程序按照自己的想法推一遍吧
;懒得推




(define (encode-symbol symbol tree)
  (cond ((and (leaf? (left-branch tree)) (contain? symbol (left-branch tree)))
         (cons 0 nil))
        ((and (leaf? (right-branch tree)) (contain? symbol (right-branch tree)))
         (cons 1 nil))
        ((contain? symbol (left-branch tree))
         (cons 0 (encode-symbol symbol (left-branch tree))))
        ((contain? symbol (right-branch tree))
         (cons 1 (encode-symbol symbol (right-branch tree))))
        (else (error "no such symbol -- ENCODE-SYMBOL" symbol))))
;关键点是对情况的分析，每种情况对应的连接方式是什么？

(define (contain? symbol tree)
  (iter-contain symbol (symbols tree)))

(define (iter-contain symbol t-s)
  (cond ((null? t-s) (= 0 1))
        ((eq? symbol (car t-s)) (= 1 1))
        (else (iter-contain symbol (cdr t-s))))) 
          
#|
(define (equal-leaf? symbol tree)
  (cond ((not (leaf? tree)) 0)
        ((eq? symbol (car (symbols tree))) 1)
        (else 0)))
|#

;tree:(list left right entry)

;(encode t1 sample-tree)
;sample-message

;数字会被当true，包括0
;尚有问题

;完成，其实没那么难

;练习2.69
#|
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))


(define t-p '((A 4) (B 2) (C 1) (D 1)))

;((leaf A 4) ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4) (A B D C) 8))
(define t3 (make-leaf-set '((A 4))))

;注意：
(define tpp (make-leaf-set t-p))
;sample-tree

(define (code-set? sets)
  (code-set-rec? sets 0))

(define (code-set-rec? sets n)
  (cond ((null? sets) (= 0 1))
        ((and (= n 3) (number? (car sets)))
         (= 0 0))
        (else (code-set-rec? (cdr sets) (+ n 1)))))

;规定：符号-频度对偶表以频度从大到小排列
;find-min-sum:找到频度的最小二元和
;要用make-code-tree



;最后两个元素的和一定是最小的
;上面的论断错了ヽ(●-`Д´-)ノ

#|
(define (min-merge set1 set2)
  (let ((elem1 (car set1))
        (elem2 (car set2)))
    (if (null? (cdr set2))
        (make-code-tree elem1 elem2)
        (cons elem1
              (cons elem2
                    (min-merge (cdr set1) (cdr set2)))))))
|#

;很可能是完全另一种思路
;递归进行make-code-tree

#|
(define test-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-leaf 'B 2)))

(weight test-tree)
|#

;关键问题：如何找到最小的元素和？？？

(define (list-sum list-numbers)
  (if (null? (cdr list-numbers))
      nil
      (cons (+ (car list-numbers)
               (cadr list-numbers))
            (list-sum (cdr list-numbers)))))

(define (find-min-pos list-sum)
  (iter-find-min-num list-sum (car list-sum) 0 0))
;返回的是position

(define (iter-find-min-num list-sum n tpos min-pos)
  (cond ((null? (cdr list-sum)) min-pos)
        ((< (car list-sum) n)
         (iter-find-min-num (cdr list-sum) (car list-sum) (+ tpos 1) tpos))
        (else (iter-find-min-num (cdr list-sum) n (+ tpos 1) min-pos))))

(define (make-cost-list sets)
  (if (null? sets)
      nil
      (cons (weight (car sets))
            (make-cost-list (cdr sets)))))

;tpp：它是从后往前创建pair的

#|
(make-cost-list tpp)
(list-sum (make-cost-list tpp))
(find-min-pos (list-sum (make-cost-list tpp)))
|#

(define (successive-merge sets)
  (cond ((null? sets) nil)
        ((not (code-set? sets))
         (successive-merge (merge (find-min-pos
                                   (list-sum (make-cost-list sets)))
                                  sets)))
        (else sets)))



;接下来是merge函数

;merge:只需要两个参数，因为一个叶子只能与其后的一个叶子归并

;tpp:叶子化后的符号-频度对偶表

(define (merge pos sets)
  (iter-merge pos 0 sets))

(define (iter-merge pos n sets)
  (cond ((null? sets) nil)
        ;这里有个坑，n=pos且为code-set时会多进行一层嵌套,要把这层嵌套去掉
        ((and (= pos n) (null? (cddr sets)))
         (make-code-tree (car sets) (cadr sets)))
        ((= pos n) (cons (make-code-tree (car sets)
                                         (cadr sets))
                         (iter-merge pos (+ n 1) (cddr sets))))
        (else (cons (car sets) (iter-merge pos (+ n 1) (cdr sets))))))
;(successive-merge tpp)
;基本完成，进入调试阶段

;tree => ((leaf A 4) ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4) (A B D C) 8)

;sets => (((((leaf C 1) (leaf D 1) (C D) 2) (leaf B 2) (C D B) 4) (leaf A 4) (C D B A) 8))


;为什么sets不是code-set而tree是？
;多了一层嵌套。

;多的一层嵌套哪儿来的？
;n=pos且为code-set时会多进行一层嵌套

;(generate-huffman-tree t-p)
;成功，把杂乱的代码集整理一下吧。
|#

;代码整理：

;哈夫曼树判断组
(define (code-set? sets)
  (code-set-rec? sets 0))

(define (code-set-rec? sets n)
  (cond ((null? sets) (= 0 1))
        ((and (= n 3) (number? (car sets)))
         (= 0 0))
        (else (code-set-rec? (cdr sets) (+ n 1)))))

;最小元素和检查组
(define (find-min-pos list-sum)
  (iter-find-min-num list-sum (car list-sum) 0 0))
;;返回的是position

(define (iter-find-min-num list-sum n tpos min-pos)
  (cond ((null? (cdr list-sum)) min-pos)
        ((< (car list-sum) n)
         (iter-find-min-num (cdr list-sum) (car list-sum) (+ tpos 1) tpos))
        (else (iter-find-min-num (cdr list-sum) n (+ tpos 1) min-pos))))

(define (make-cost-list sets)
  (if (null? sets)
      nil
      (cons (weight (car sets))
            (make-cost-list (cdr sets)))))

(define (list-sum list-numbers)
  (if (null? (cdr list-numbers))
      nil
      (cons (+ (car list-numbers)
               (cadr list-numbers))
            (list-sum (cdr list-numbers)))))

;归并组
(define (successive-merge sets)
  (cond ((null? sets) nil)
        ((not (code-set? sets))
         (successive-merge (merge (find-min-pos
                                   (list-sum (make-cost-list sets)))
                                  sets)))
        (else sets)))

(define (merge pos sets)
  (iter-merge pos 0 sets))

(define (iter-merge pos n sets)
  (cond ((null? sets) nil)
        ((and (= pos n) (null? (cddr sets)))
         (make-code-tree (car sets) (cadr sets)))
        ((= pos n) (cons (make-code-tree (car sets)
                                         (cadr sets))
                         (iter-merge pos (+ n 1) (cddr sets))))
        (else (cons (car sets) (iter-merge pos (+ n 1) (cdr sets))))))

;最终过程
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

#|
答案：
(define (successive-merge ordered-set)
  (cond ((= 0 (length ordered-set))
         '())
        ((= 1 (length ordered-set))
         (car ordered-set))
        (else
         (let ((new-sub-tree (make-code-tree (car ordered-set)
                                             (cadr ordered-set)))
               (remained-ordered-set (cddr ordered-set)))
           (successive-merge (adjoin-set new-sub-tree remained-ordered-set))))))
|#


;练习2.70
(define c-table '((a 2) (boom 1) (Get 2) (job 2) (na 16) (Sha 3) (yip 9) (Wah 1)))
(define c-tree (generate-huffman-tree c-table))
(define c-message '(Get a job
                        Sha na na na na na na na na
                        Get a job
                        Sha na na na na na na na na
                        Wah yip yip yip yip yip yip yip yip yip
                        Sha boom))
(define c-code (encode c-message c-tree))

(define (count list)
  (if (null? list)
      0
      (+ 1 (count (cdr list)))))

;(count c-code)
;84
;定长编码呢？
;712

;答案：定长需要108
;不是用ASCII编码！！
;8个字符每个最少占用3个二进制位

;练习2.71

#|
n=5
最频繁：1
最不频繁：4

n=10
最频繁：1
最不频繁：9

|#

;练习2.72

#|
最频繁：1
最不频繁：n

;答案：
最频繁：Θ(n)
最不频繁：Θ(n^2)

|#
