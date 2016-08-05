;; car 
;; DEPENDENCIES: x, c, lambda
;; DEPENDED ON BY: reverse, cdar, cadar, cadr, reduce, cond, caadr, caddr, map, cdadr, cddar, append, caar
(defun car (c) (c (lambda (x y) x)))

;; caar 
;; DEPENDENCIES: car
;; DEPENDED ON BY: cond, firsts
(defun caar (l) (car (car l)))

;; null? 
;; DEPENDENCIES: if
;; DEPENDED ON BY: 
(defun null? (x) (if x () x))

;; and 
;; DEPENDENCIES: if
;; DEPENDED ON BY: 
(defun and (x y) (if x y x))

;; cdr 
;; DEPENDENCIES: x, c, lambda
;; DEPENDED ON BY: pair?, reverse, cdddr, cdar, seconds, cadar, cadr, reduce, cond, caadr, firsts, caddr, map, cdadr, cddr, cddar, append
(defun cdr (c) (c (lambda (x y) y)))

;; caddr 
;; DEPENDENCIES: cdr, car
;; DEPENDED ON BY: 
(defun caddr (l) (car (cdr (cdr l))))

;; caadr 
;; DEPENDENCIES: cdr, car
;; DEPENDED ON BY: 
(defun caadr (l) (car (car (cdr l))))

;; cdar 
;; DEPENDENCIES: cdr, car
;; DEPENDED ON BY: 
(defun cdar (l) (cdr (car l)))

;; cddr 
;; DEPENDENCIES: cdr
;; DEPENDED ON BY: 
(defun cddr (l) (cdr (cdr l)))

;; cdadr 
;; DEPENDENCIES: cdr, car
;; DEPENDED ON BY: 
(defun cdadr (l) (cdr (car (cdr l))))

;; cons 
;; DEPENDENCIES: f, lambda
;; DEPENDED ON BY: reverse, firsts, append, seconds, map
(defun cons (x y) (lambda (f) (f x y)))

;; map 
;; DEPENDENCIES: cons, cdr, car, if, fn
;; DEPENDED ON BY: 
(defun map (fn l) (if l (cons (fn (car l)) (map fn (cdr l)))))

;; pair? 
;; DEPENDENCIES: cdr
;; DEPENDED ON BY: reverse, reduce
(defun pair? (x) (cdr x))

;; reduce 
;; DEPENDENCIES: cdr, car, if, pair?, fn
;; DEPENDED ON BY: 
(defun reduce (fn l) (if (pair? l) (fn (car l) (reduce fn (cdr l))) (car l)))

;; append 
;; DEPENDENCIES: if, cons, cdr, car
;; DEPENDED ON BY: reverse
(defun append (lista listb) (if lista (cons (car lista) (append (cdr lista) listb)) listb))

;; reverse 
;; DEPENDENCIES: cons, cdr, car, if, pair?, append
;; DEPENDED ON BY: 
(defun reverse (l) (if (pair? l) (append (reverse (cdr l)) (cons (car l) ())) l))

;; cddar 
;; DEPENDENCIES: cdr, car
;; DEPENDED ON BY: 
(defun cddar (l) (cdr (cdr (car l))))

;; firsts 
;; DEPENDENCIES: if, cons, caar, cdr
;; DEPENDED ON BY: 
(defun firsts (l) (if l (cons (caar l) (firsts (cdr l)))))

;; cadar 
;; DEPENDENCIES: cdr, car
;; DEPENDED ON BY: cond, seconds
(defun cadar (l) (car (cdr (car l))))

;; cond 
;; DEPENDENCIES: cadar, caar, cdr, car, if
;; DEPENDED ON BY: 
(defun cond (options) (if (car options) (if (caar options) (cadar options) (cond (cdr options)))))

;; cadr 
;; DEPENDENCIES: cdr, car
;; DEPENDED ON BY: 
(defun cadr (l) (car (cdr l)))

;; seconds 
;; DEPENDENCIES: if, cons, cadar, cdr
;; DEPENDED ON BY: 
(defun seconds (l) (if l (cons (cadar l) (seconds (cdr l)))))

;; cdddr 
;; DEPENDENCIES: cdr
;; DEPENDED ON BY: 
(defun cdddr (l) (cdr (cdr (cdr l))))

;; or 
;; DEPENDENCIES: if
;; DEPENDED ON BY: 
(defun or (x y) (if x x y))

