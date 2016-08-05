;; or 
;; DEPENDENCIES: if
;; DEPENDED ON BY: 
(defun or (x y) (if x x y))

;; cons 
;; DEPENDENCIES: lambda
;; DEPENDED ON BY: seconds, firsts, append, reverse, map
(defun cons (x y) (lambda (f) (f x y)))

;; null? 
;; DEPENDENCIES: if
;; DEPENDED ON BY: 
(defun null? (x) (if x () x))

;; cdr 
;; DEPENDENCIES: lambda
;; DEPENDED ON BY: cddr, firsts, caddr, reduce, map, pair?, cdddr, cond, cddar, caadr, append, cadr, cadar, cdadr, seconds, reverse, cdar
(defun cdr (c) (c (lambda (x y) y)))

;; cdddr 
;; DEPENDENCIES: cdr
;; DEPENDED ON BY: 
(defun cdddr (l) (cdr (cdr (cdr l))))

;; pair? 
;; DEPENDENCIES: cdr
;; DEPENDED ON BY: reduce, reverse
(defun pair? (x) (cdr x))

;; and 
;; DEPENDENCIES: if
;; DEPENDED ON BY: 
(defun and (x y) (if x y x))

;; car 
;; DEPENDENCIES: lambda
;; DEPENDED ON BY: caddr, reduce, map, cond, cddar, caadr, append, cadr, caar, cadar, cdadr, cdar, reverse
(defun car (c) (c (lambda (x y) x)))

;; cdar 
;; DEPENDENCIES: cdr, car
;; DEPENDED ON BY: 
(defun cdar (l) (cdr (car l)))

;; cdadr 
;; DEPENDENCIES: cdr, car
;; DEPENDED ON BY: 
(defun cdadr (l) (cdr (car (cdr l))))

;; cadar 
;; DEPENDENCIES: cdr, car
;; DEPENDED ON BY: cond, seconds
(defun cadar (l) (car (cdr (car l))))

;; seconds 
;; DEPENDENCIES: cdr, cons, if, cadar
;; DEPENDED ON BY: 
(defun seconds (l) (if l (cons (cadar l) (seconds (cdr l)))))

;; caar 
;; DEPENDENCIES: car
;; DEPENDED ON BY: firsts, cond
(defun caar (l) (car (car l)))

;; firsts 
;; DEPENDENCIES: cdr, cons, if, caar
;; DEPENDED ON BY: 
(defun firsts (l) (if l (cons (caar l) (firsts (cdr l)))))

;; cadr 
;; DEPENDENCIES: cdr, car
;; DEPENDED ON BY: 
(defun cadr (l) (car (cdr l)))

;; append 
;; DEPENDENCIES: cdr, cons, if, car
;; DEPENDED ON BY: reverse
(defun append (lista listb) (if lista (cons (car lista) (append (cdr lista) listb)) listb))

;; reverse 
;; DEPENDENCIES: cdr, if, append, car, cons, pair?
;; DEPENDED ON BY: 
(defun reverse (l) (if (pair? l) (append (reverse (cdr l)) (cons (car l) ())) l))

;; caadr 
;; DEPENDENCIES: cdr, car
;; DEPENDED ON BY: 
(defun caadr (l) (car (car (cdr l))))

;; cddar 
;; DEPENDENCIES: cdr, car
;; DEPENDED ON BY: 
(defun cddar (l) (cdr (cdr (car l))))

;; map 
;; DEPENDENCIES: cdr, cons, if, car
;; DEPENDED ON BY: 
(defun map (fn l) (if l (cons (fn (car l)) (map fn (cdr l)))))

;; reduce 
;; DEPENDENCIES: cdr, pair?, if, car
;; DEPENDED ON BY: 
(defun reduce (fn l) (if (pair? l) (fn (car l) (reduce fn (cdr l))) (car l)))

;; caddr 
;; DEPENDENCIES: cdr, car
;; DEPENDED ON BY: 
(defun caddr (l) (car (cdr (cdr l))))

;; cond 
;; DEPENDENCIES: cdr, if, car, cadar, caar
;; DEPENDED ON BY: 
(defun cond (options) (if (car options) (if (caar options) (cadar options) (cond (cdr options)))))

;; cddr 
;; DEPENDENCIES: cdr
;; DEPENDED ON BY: 
(defun cddr (l) (cdr (cdr l)))

