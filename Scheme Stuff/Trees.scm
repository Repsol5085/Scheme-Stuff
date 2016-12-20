;Trees
;Basic
(define (root t) (car t))
(define (left t) (cadr t))
(define (right t) (caddr t))
(define (the-empty-tree)'())
(define (tree-empty? t) (null? t))
;Konstruktor
(define(make-tree root left right)
  (list root left right))

;Examples
;(if root is leaf) 
(define (leaf? t)
  (and (not(tree-empty? t))
       (tree-empty? (left t))
       (tree-empty? (right t))))

;Example Tree
(define sample-tree
  (make-tree 1
             (make-tree 2
                        (the-empty-tree)
                        (the-empty-tree))
              (make-tree 3
                         (make-tree 4
                                    (the-empty-tree)
                                    (the-empty-tree))
                         (the-empty-tree))))

;1
(define (pre-order t)
  (if (tree-empty? t)
      '()
      (append (list (root t) (pre-order (left t))(pre-order (right t))))))

(define (in-order t)
  (if (tree-empty? t)
      '()
      (append (in-order(left t))(list (root t)) (in-order (right t)))))
(define (post-order t)
  (if(tree-empty? t)
     '()
     (append (post-order(right t))  (post-order (left t)) (list (root t)))))
;2
(define (map-tree f t)
  (if(tree-empty? t)
     (the-empty-tree)
     (make-tree (f (root t))
                (map-tree f (left t))
                (map-tree f (right t)))))
;1
(define (diagonal m)
  (if (null? m)
      '()
      (cons (caar m)
            (diagonal (map cdr (cdr m))))))
;2
;za spisak(pomoshtna func)
(define (skip l n)
  (cond
    ((null? l) '())
    ((= n 0) (cdr l))
    (else (cons (car l)
                (skip (cdr l) (- n 1))))))


;(define (skip-row

  
















  




































  
