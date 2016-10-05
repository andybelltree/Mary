;; cons 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: firsts, append, seconds, reverse, map
(defun cons (x y) (lambda (f) (f x y)))

;; cdr 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: cddar, cddr, reverse, map, pair?, cadar, firsts, caddr, reduce, cadr, caadr, append, cond, cdadr, cdddr, seconds, cdar
(defun cdr (c) (c (lambda (x y) y)))

;; or 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: NONE
(defun or (x y) (if x x y))

;; cddr 
;; DEPENDENCIES: cdr
;; DEPENDED ON BY: NONE
(defun cddr (l) (cdr (cdr l)))

;; and 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: NONE
(defun and (x y) (if x y x))

;; cdddr 
;; DEPENDENCIES: cdr
;; DEPENDED ON BY: NONE
(defun cdddr (l) (cdr (cdr (cdr l))))

;; null? 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: NONE
(defun null? (x) (if x () x))

;; pair? 
;; DEPENDENCIES: cdr
;; DEPENDED ON BY: reduce, reverse
(defun pair? (x) (cdr x))

;; car 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: cddar, reverse, map, caddr, cadar, reduce, cadr, caadr, caar, append, cond, cdadr, cdar
(defun car (c) (c (lambda (x y) x)))

;; cdadr 
;; DEPENDENCIES: cdr, car
;; DEPENDED ON BY: NONE
(defun cdadr (l) (cdr (car (cdr l))))

;; append 
;; DEPENDENCIES: car, cons, cdr
;; DEPENDED ON BY: reverse
(defun append (lista listb) (if lista (cons (car lista) (append (cdr lista) listb)) listb))

;; caar 
;; DEPENDENCIES: car
;; DEPENDED ON BY: firsts, cond
(defun caar (l) (car (car l)))

;; firsts 
;; DEPENDENCIES: cdr, caar, cons
;; DEPENDED ON BY: NONE
(defun firsts (l) (if l (cons (caar l) (firsts (cdr l)))))

;; cadr 
;; DEPENDENCIES: car, cdr
;; DEPENDED ON BY: NONE
(defun cadr (l) (car (cdr l)))

;; reduce 
;; DEPENDENCIES: car, pair?, cdr
;; DEPENDED ON BY: NONE
(defun reduce (fn l) (if (pair? l) (fn (car l) (reduce fn (cdr l))) (car l)))

;; cadar 
;; DEPENDENCIES: car, cdr
;; DEPENDED ON BY: cond, seconds
(defun cadar (l) (car (cdr (car l))))

;; seconds 
;; DEPENDENCIES: cdr, cadar, cons
;; DEPENDED ON BY: NONE
(defun seconds (l) (if l (cons (cadar l) (seconds (cdr l)))))

;; cond 
;; DEPENDENCIES: caar, cadar, cdr, car
;; DEPENDED ON BY: NONE
(defun cond (options) (if (car options) (if (caar options) (cadar options) (cond (cdr options)))))

;; caddr 
;; DEPENDENCIES: car, cdr
;; DEPENDED ON BY: NONE
(defun caddr (l) (car (cdr (cdr l))))

;; map 
;; DEPENDENCIES: cdr, car, cons
;; DEPENDED ON BY: NONE
(defun map (fn l) (if l (cons (fn (car l)) (map fn (cdr l)))))

;; reverse 
;; DEPENDENCIES: append, cdr, pair?, car, cons
;; DEPENDED ON BY: NONE
(defun reverse (l) (if (pair? l) (append (reverse (cdr l)) (cons (car l) ())) l))

;; cdar 
;; DEPENDENCIES: cdr, car
;; DEPENDED ON BY: NONE
(defun cdar (l) (cdr (car l)))

;; caadr 
;; DEPENDENCIES: car, cdr
;; DEPENDED ON BY: NONE
(defun caadr (l) (car (car (cdr l))))

;; cddar 
;; DEPENDENCIES: cdr, car
;; DEPENDED ON BY: NONE
(defun cddar (l) (cdr (cdr (car l))))

