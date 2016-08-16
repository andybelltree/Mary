;; car 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: cdar, caadr, cddar, map, reverse, append, cadr, caar, cadar, reduce, cond, cdadr, caddr
(defun car (c) (c (lambda (x y) x)))

;; or 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: NONE
(defun or (x y) (if x x y))

;; and 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: NONE
(defun and (x y) (if x y x))

;; cdr 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: cdar, firsts, seconds, caadr, cddar, map, pair?, reverse, append, cadr, cdddr, cadar, reduce, cond, cdadr, cddr, caddr
(defun cdr (c) (c (lambda (x y) y)))

;; cdadr 
;; DEPENDENCIES: car, cdr
;; DEPENDED ON BY: NONE
(defun cdadr (l) (cdr (car (cdr l))))

;; cadar 
;; DEPENDENCIES: car, cdr
;; DEPENDED ON BY: seconds, cond
(defun cadar (l) (car (cdr (car l))))

;; cdddr 
;; DEPENDENCIES: cdr
;; DEPENDED ON BY: NONE
(defun cdddr (l) (cdr (cdr (cdr l))))

;; cddar 
;; DEPENDENCIES: car, cdr
;; DEPENDED ON BY: NONE
(defun cddar (l) (cdr (cdr (car l))))

;; cdar 
;; DEPENDENCIES: car, cdr
;; DEPENDED ON BY: NONE
(defun cdar (l) (cdr (car l)))

;; cadr 
;; DEPENDENCIES: car, cdr
;; DEPENDED ON BY: NONE
(defun cadr (l) (car (cdr l)))

;; caar 
;; DEPENDENCIES: car
;; DEPENDED ON BY: firsts, cond
(defun caar (l) (car (car l)))

;; cond 
;; DEPENDENCIES: cadar, caar, cdr, car
;; DEPENDED ON BY: NONE
(defun cond (options) (if (car options) (if (caar options) (cadar options) (cond (cdr options)))))

;; cons 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: append, map, firsts, seconds, reverse
(defun cons (x y) (lambda (f) (f x y)))

;; firsts 
;; DEPENDENCIES: cons, cdr, caar
;; DEPENDED ON BY: NONE
(defun firsts (l) (if l (cons (caar l) (firsts (cdr l)))))

;; map 
;; DEPENDENCIES: car, cons, cdr
;; DEPENDED ON BY: NONE
(defun map (fn l) (if l (cons (fn (car l)) (map fn (cdr l)))))

;; append 
;; DEPENDENCIES: car, cons, cdr
;; DEPENDED ON BY: reverse
(defun append (lista listb) (if lista (cons (car lista) (append (cdr lista) listb)) listb))

;; null? 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: NONE
(defun null? (x) (if x () x))

;; caadr 
;; DEPENDENCIES: car, cdr
;; DEPENDED ON BY: NONE
(defun caadr (l) (car (car (cdr l))))

;; seconds 
;; DEPENDENCIES: cons, cdr, cadar
;; DEPENDED ON BY: NONE
(defun seconds (l) (if l (cons (cadar l) (seconds (cdr l)))))

;; cddr 
;; DEPENDENCIES: cdr
;; DEPENDED ON BY: NONE
(defun cddr (l) (cdr (cdr l)))

;; pair? 
;; DEPENDENCIES: cdr
;; DEPENDED ON BY: reduce, reverse
(defun pair? (x) (cdr x))

;; reverse 
;; DEPENDENCIES: pair?, append, cons, cdr, car
;; DEPENDED ON BY: NONE
(defun reverse (l) (if (pair? l) (append (reverse (cdr l)) (cons (car l) ())) l))

;; reduce 
;; DEPENDENCIES: car, pair?, cdr
;; DEPENDED ON BY: NONE
(defun reduce (fn l) (if (pair? l) (fn (car l) (reduce fn (cdr l))) (car l)))

;; caddr 
;; DEPENDENCIES: car, cdr
;; DEPENDED ON BY: NONE
(defun caddr (l) (car (cdr (cdr l))))

