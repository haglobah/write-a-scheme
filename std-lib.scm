(define (not x) (if x #f #t))
(define (null? obj) (if (eqv? obj '()) #t #f))
(define (list . objs) objs)
(define (id obj) obj)
(define (flip func) (lambda (a b) (func b a)))

(define (curry func a) (lambda (as) (apply func (cons a (list as)))))
(define (compose f g) (lambda (a) (f (apply g arg))))

(define zero?       (curry = 0))
(define positive?   (curry < 0))
(define negative?   (curry > 0))
(define (odd? num)  (= (mod num 2) 1))
(define (even? num) (= (mod num 2) 0))

(define (foldr f init lst)
  (if (null? lst)
      init
      (f (car lst) (foldr f init (cdr lst)))))
(define (foldl f acc lst)
  (if (null? lst)
      acc
      (foldl f (f acc (car lst)) (cdr lst))))
(define fold foldl)
(define reduce foldr)

(define (unfold f init pred)
  (if (pred init)
      (cons init '())
      (cons init (unfold f (f init) pred))))

(define (sum . lst)     (fold + 0 lst))
(define (product . lst) (fold * 0 lst))
(define (and . lst)     (fold && #t lst))
(define (or . lst)      (fold || #f lst))

(define (max first . rest)
  (fold (lambda (prev now)
                (if (> now prev)
                    now
                    prev))
        first
        rest))
(define (min first . rest)
  (fold (lambda (prev now)
                (if (< now prev)
                    now
                    prev))
        first
        rest))

(define (length lst)  (fold (lambda (x y) (+ x 1) 0 lst)))
(define (reverse lst) (fold (flip cons) '() lst))

(define (mem-helper pred op) (lambda (acc next) (if (and (not acc) (pred (op next))) next acc)))
(define (memq obj lst)       (fold (mem-helper (curry eq? obj) id) #f lst))
(define (memv obj lst)       (fold (mem-helper (curry eqv? obj) id) #f lst))
(define (member obj lst)     (fold (mem-helper (curry equal? obj) id) #f lst))
(define (assq obj alist)     (fold (mem-helper (curry eq? obj) car) #f alist))
(define (assv obj alist)     (fold (mem-helper (curry eqv? obj) car) #f alist))
(define (assoc obj alist)    (fold (mem-helper (curry equal? obj) car) #f alist))

(define (map f lst) (foldr (lambda (x y) (cons (f x) y)) '() lst))
(define (filter pred lst) (foldr (lambda (x y) (if (pred x) (cons x y) y)) '() lst))
