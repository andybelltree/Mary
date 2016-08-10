;; cdr 
;; DEPENDENCIES: lambda
;; DEPENDED ON BY: seconds, cdddr, caadr, map, cdar, cddar, pair?, cddr, cadr, firsts, cadar, cdadr, reduce, cond, caddr, append, reverse
(defun cdr (c) (c (lambda (x y) y)))

;; cdddr 
;; DEPENDENCIES: cdr
;; DEPENDED ON BY: 
(defun cdddr (l) (cdr (cdr (cdr l))))

;; or 
;; DEPENDENCIES: if
;; DEPENDED ON BY: 
(defun or (x y) (if x x y))

;; car 
;; DEPENDENCIES: lambda
;; DEPENDED ON BY: caadr, reduce, caar, map, cdar, cddar, cadr, reverse, cadar, cdadr, cond, caddr, append
(defun car (c) (c (lambda (x y) x)))

;; cadar 
;; DEPENDENCIES: cdr, car
;; DEPENDED ON BY: seconds, cond
(defun cadar (l) (car (cdr (car l))))

;; cadr 
;; DEPENDENCIES: cdr, car
;; DEPENDED ON BY: 
(defun cadr (l) (car (cdr l)))

;; cddar 
;; DEPENDENCIES: cdr, car
;; DEPENDED ON BY: 
(defun cddar (l) (cdr (cdr (car l))))

;; cons 
;; DEPENDENCIES: lambda
;; DEPENDED ON BY: seconds, map, reverse, append, firsts
(defun cons (x y) (lambda (f) (f x y)))

;; append 
;; DEPENDENCIES: if, cons, cdr, car
;; DEPENDED ON BY: reverse
(defun append (lista listb) (if lista (cons (car lista) (append (cdr lista) listb)) listb))

;; caar 
;; DEPENDENCIES: car
;; DEPENDED ON BY: cond, firsts
(defun caar (l) (car (car l)))

;; cond 
;; DEPENDENCIES: caar, cadar, car, if, cdr
;; DEPENDED ON BY: 
(defun cond (options) (if (car options) (if (caar options) (cadar options) (cond (cdr options)))))

;; pair? 
;; DEPENDENCIES: cdr
;; DEPENDED ON BY: reduce, reverse
(defun pair? (x) (cdr x))

;; reduce 
;; DEPENDENCIES: cdr, if, pair?, car
;; DEPENDED ON BY: 
(defun reduce (fn l) (if (pair? l) (fn (car l) (reduce fn (cdr l))) (car l)))

;; cdadr 
;; DEPENDENCIES: cdr, car
;; DEPENDED ON BY: 
(defun cdadr (l) (cdr (car (cdr l))))

;; caddr 
;; DEPENDENCIES: cdr, car
;; DEPENDED ON BY: 
(defun caddr (l) (car (cdr (cdr l))))

;; map 
;; DEPENDENCIES: car, cons, cdr, if
;; DEPENDED ON BY: 
(defun map (fn l) (if l (cons (fn (car l)) (map fn (cdr l)))))

;; firsts 
;; DEPENDENCIES: caar, cons, cdr, if
;; DEPENDED ON BY: 
(defun firsts (l) (if l (cons (caar l) (firsts (cdr l)))))

;; caadr 
;; DEPENDENCIES: cdr, car
;; DEPENDED ON BY: 
(defun caadr (l) (car (car (cdr l))))

;; cddr 
;; DEPENDENCIES: cdr
;; DEPENDED ON BY: 
(defun cddr (l) (cdr (cdr l)))

;; null? 
;; DEPENDENCIES: if
;; DEPENDED ON BY: 
(defun null? (x) (if x () x))

;; reverse 
;; DEPENDENCIES: cons, cdr, append, car, if, pair?
;; DEPENDED ON BY: 
(defun reverse (l) (if (pair? l) (append (reverse (cdr l)) (cons (car l) ())) l))

;; cdar 
;; DEPENDENCIES: cdr, car
;; DEPENDED ON BY: 
(defun cdar (l) (cdr (car l)))

;; and 
;; DEPENDENCIES: if
;; DEPENDED ON BY: 
(defun and (x y) (if x y x))

;; seconds 
;; DEPENDENCIES: cadar, cons, cdr, if
;; DEPENDED ON BY: 
(defun seconds (l) (if l (cons (cadar l) (seconds (cdr l)))))

