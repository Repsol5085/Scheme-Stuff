(define (isOdd? x)
  (if (=(remainder x 2) 1) #t #f))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (iseEven? x)
  (= (remainder x 2) 0))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (signum x)
  (if (< x 0) -1 (if (= x 0) 0 1)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (sig x)
  (cond
    ((< x 0) -1)
    ((> x 0) 1)
    (else 0)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Factoriel n!
(define (fact n)
  (if (= n 1)
      1
      (* n (fact (- n 1)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (sum start end)
  (if (= start end)
      end
      (if(< start end)(+ start (sum(+ start 1 ) end )))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (pow2 x)
  (* x x))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (pow x n)
  (cond
    ((= n 0) 1)
    ((even? n)(pow2(pow x(/ n 2))))
    (else(* x (pow x (- n 1))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (count n)
  (if(< n 10)
     1
     (+ 1 (count (quotient n 10)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (sum-digits n)
  (if(< n 10)
     n
     (+ (quotient n 10)(sum-digits (remainder n 10)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (prime? n)
  (define (helper1 possible-divisor)
  (cond
    ((<= possible-divisor 1) #t)
    ((= (mod n possible-divisor) 0) #f)
    (else (helper(- possible-divisor 1)))))
  (helper (- n 1)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define (automorphic? n)
;  (define (count counter newn)
;    (if (< newn 10)
;        1
;        (count (+ counter 1)
(define (automorphic? n)
  ( = n (remainder (expt n 2) (expt 10 (count-digits n)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (sum start end)
  (if(>= start end)
     end
     (+ start (sum (+ start 1) end))))
;slow ^
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (fast-sum start end)
  (define (helper crr result)
    (if(> crr end)
       result
       (helper(+ crr 1)(+ result crr))))
  (helper start 0))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (count-digits-iter n)
  (define (helper n result)
    (if(< n 10)
       1
       (helper (quotient n 10) (+ result 1))))
  (helper n 0))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (sum-digits-iter n)
  (define(helper n result)
    (if (< n 10)
        n
        (helper (quotient n 10) (+ reuslt (remainder n 10)))))
  (helper n 0))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (my-reverse n)
  (define (helper result n )
    (if(< n 10)
       (+(* 10 result) n)
       (helper (+ (* result 10) (remainder n 10)))))
  (helper 0 n))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Good palindrome !
(define (palindrome? n)
  (define (helper crr-n result)
    (cond
      ((> result crr-n)#f)
      ((= result crr-n)#t)
      ((= result (quotient crr-n 10)) #t)
      (else (helper (quotient crr-n 10) (+ (* result 10) (remainder crr-n 10))))))
  (helper n 0))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;count palindromes
(define (count-pal start end)
  (define (helper crr counter)
    (cond
      ((> crr end ) counter)
      ((palindrome? crr) (helper (+ crr 1)(+ counter 1)))
      (else(helper (+ crr 1) counter))))
  (helper start 0))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (substr? n a)
  (define (helper n)
    (cond
      ((< (count n) (count a) )#f)
      ((= (remainder n (expt 10 (count a))) a) #t)
      (else(helper (quotient n 10)))))
  (helper n))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;27.10.2016

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (derive f)
  ((lambda (x0)
     (let((x (+ x0 0.001)))
       (/ (- (f x) (f x0)) (- x x0))
       )
     ))
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (pow2 x)
  (* x x))
;;;;;;;;;;;;;;
(define (derive-1 f)
  (lambda (x0)
    (let ((x (+ x0 0.001)))
      (/ (- (f x) (f x0)) (- x x0)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (derive-x f)
  (lambda (x0 y)
    (let ((x (+ x0 0.001)))
      (/ (- (f x y) (f x0 y)) (- x x0))
      )
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (fib n)
  (if (<= n 2)
      1
      (+ (fib (- n 1))
         (fib (- n 2)))))
;;slow fib ^
(define (fib-iter n)
  (define (helper crr prev count)
    (if (= count 1)
        crr
        (helper (+ crr prev) crr (- count 1))))
  (helper 1 0 n))
;;fast fib^
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (sqrt-iter x)
  (define (helper y)
    (if (< (abs(- (* y y) x)) 0.001)
        (exact->inexact y)
        (helper ( / (+ y (/ x y)) 2))))
  (helper 1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (f1 x)
  (+ x 1))
(define f2 (lambda(x)
             (+ x 1)))
(define (f3 x)
  (/ (+ (* x x) 1)
     (*x x)))
(define (f4 x)
  ((lambda(y)
     (/ (+ y 1)
        y))
   (* x x)))
(define (f5 x)
  (let ((y (* x x))
    (result (/ (+ y 1) y)))
  result))
;;; f3 f4 f5 are the same
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;f(x) g(x) => h(x)=f(g(x)) f+g
(define (compose f g)
  (lambda (x)
    (f (g x))))
;; how to call it  ((compose f g) x)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;03.11.2016

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (repeat f n)
  ;g(x)=f(f(...f(x)))
  ;repeat vrushta id(x) ako n=0 inache compose
  (if( = n 0)
     (lambda (x) x)
     (compose f (repeat f (- n 1)))))
(define (plus-1p1 x) (+ x 1))
;;((repeat plus-1p1 5) 2) == 7
(define (derive-n f n)
  (lambda(f-prime)(derive f))
    (repeat (derive f) n))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (derive-n1 f n)
  ((repeat derive n) f))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; '() prazen spisak
; '(1 2 3 4)
;(car '(1 2 3))==1
;(cdr '(1 2 3))== (2 3)
;(member 6 '(1 2 3))== #f
;(member 2 '(1 3 4))== (2 3)
(define (member? list el)
  (if(list? list)
     (if(null? list)
        #f
        (if(= (car list) el)
           #t
           (member? (cdr list) el)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (my-length list)
  (define (helper list1 counter)
    (if(null? list1)
       counter
       (helper (cdr list1) (+ counter 1))))
  (helper list 0))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (nth list n)
  (if(list? list)
     (if (null? list)
         #f
         (if(= n 0) (car list)
            (nth (cdr list)(- n 1))))
     #f))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (my-append l1 l2)
  (cond
    ((null? l1) l2)
    ((cons(car l1 (my-append (cdr l1) l2))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;10.11.2016

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (clone list)
  (if (null? list)
      '()
      (cons (car list)(clone (cdr list)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (generate-list n)
  (define (helper list n)
    (if (< n 1)
        list
        (cons n (helper list (- n 1)))))
  (helper '() n))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (my-reverse list)
  (define (helper l revl)
    (if (null? l)
        revl
        (helper (cdr l) (cons (car l) revl))))
  (helper list '()))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;map & filter
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Filter implementation
(define (filter p l)
  (define (helper current-l  result)
    (cond
      ((null? current-l) result)
      ((p (car current-l)) (helper (cdr current-l)
                                   (append result (list (car current-l)))))
      (else (helper (cdr current-l) result))))
  (helper l '()))
;;tasks with map & filter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (square-even-number list)
  (map (lambda (x) (* x x))
       (filter even? list)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (union l1 l2)
  (append
   l1 (filter (lambda (x) (not(member? x l1)))
              l2)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (intersection l1 l2)
  (filter (lambda (x) (member? x l1))
          l2))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (diff l1 l2)
  (filter (lambda (x) (not(member? x l2)))
            l1))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (compositions l f)
  (map (lambda (x) (repeat f x))
       l))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (construct-list n)
  (map generate-list (generate-list n)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;17.11.16

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (fold-left f u xs)
  (if (null? xs)
      u
      (fold-left f (f u (car xs)) (cdr xs))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (fold-right f u xs)
  (if(null? xs)
     u
     (f (car xs) (fold-right f u (cdr xs)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (sum l)
  (fold-left + 0 l))
(define (sum2 l)
  (fold-left  (lambda (result x) (+ result x))
              0
              l))
(define (sum-apply l)
  (apply + l))
(define (clone-list l)
  (fold-right cons '() l))
(define (clone-list2 l)
  (fold-right (lambda (x result) (cons x result))
              '()
              l))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;compose
(define (compose3 l)
  (fold-left (lambda (res f)
               (lambda (x) (f (res x))))
             (lambda (x) x)
             l))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (filter p l)
  (define (helper current-l result)
    (cond
      ((null? current-l) result)
      ((p (car current-l)) (helper (cdr current-l)(append result (list (car current-l)))))
      (else (helper (cdr current-l) result))))
  (helper l '()))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (member1? l x)
  (if(null? l) #f
     (if (= (car l) x)
         #t
         (member1? (cdr l) x))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (column m i)
  (map (lambda (row) (list-ref row i))
       m))
(define (filter-matrix  p m)
  (map (lambda (row)
         (filter (lambda (x) (p x) row));(filter p x)
         m)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (diagonal m)
  (define (helper matr i diag)
    (if (= (length m) i)
        diag
        (helper (cdr matr) (+ i 1) (cons (list-ref (car m) i) diag))))
  (helper m 0 '()))





  















  




































































