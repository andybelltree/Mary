;; and 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: NONE
(defun and (x y) (if x y x))

;; null? 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: NONE
(defun null? (x) (if x () x))

;; car 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: cdar, caadr, cddar, map, reverse, append, cadr, caar, cadar, reduce, cond, cdadr, caddr
(defun car (c) (c (lambda (x y) x)))

;; cdr 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: cddr, cdar, firsts, seconds, caadr, cddar, map, pair?, reverse, append, cadr, cadar, reduce, cond, cdadr, cdddr, caddr
(defun cdr (c) (c (lambda (x y) y)))

;; caddr 
;; DEPENDENCIES: cdr, car
;; DEPENDED ON BY: NONE
(defun caddr (l) (car (cdr (cdr l))))

;; cadar 
;; DEPENDENCIES: cdr, car
;; DEPENDED ON BY: seconds, cond
(defun cadar (l) (car (cdr (car l))))

;; cddar 
;; DEPENDENCIES: cdr, car
;; DEPENDED ON BY: NONE
(defun cddar (l) (cdr (cdr (car l))))

;; cdar 
;; DEPENDENCIES: cdr, car
;; DEPENDED ON BY: NONE
(defun cdar (l) (cdr (car l)))

;; cddr 
;; DEPENDENCIES: cdr
;; DEPENDED ON BY: NONE
(defun cddr (l) (cdr (cdr l)))

;; cadr 
;; DEPENDENCIES: cdr, car
;; DEPENDED ON BY: NONE
(defun cadr (l) (car (cdr l)))

;; cdddr 
;; DEPENDENCIES: cdr
;; DEPENDED ON BY: NONE
(defun cdddr (l) (cdr (cdr (cdr l))))

;; caar 
;; DEPENDENCIES: car
;; DEPENDED ON BY: firsts, cond
(defun caar (l) (car (car l)))

;; cond 
;; DEPENDENCIES: car, caar, cadar, cdr
;; DEPENDED ON BY: NONE
(defun cond (options) (if (car options) (if (caar options) (cadar options) (cond (cdr options)))))

;; pair? 
;; DEPENDENCIES: cdr
;; DEPENDED ON BY: reduce, reverse
(defun pair? (x) (cdr x))

;; cons 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: append, map, firsts, seconds, reverse
(defun cons (x y) (lambda (f) (f x y)))

;; seconds 
;; DEPENDENCIES: cadar, cdr, cons
;; DEPENDED ON BY: NONE
(defun seconds (l) (if l (cons (cadar l) (seconds (cdr l)))))

;; firsts 
;; DEPENDENCIES: caar, cdr, cons
;; DEPENDED ON BY: NONE
(defun firsts (l) (if l (cons (caar l) (firsts (cdr l)))))

;; map 
;; DEPENDENCIES: cons, cdr, car
;; DEPENDED ON BY: NONE
(defun map (fn l) (if l (cons (fn (car l)) (map fn (cdr l)))))

;; append 
;; DEPENDENCIES: cdr, car, cons
;; DEPENDED ON BY: reverse
(defun append (lista listb) (if lista (cons (car lista) (append (cdr lista) listb)) listb))

;; reverse 
;; DEPENDENCIES: car, cons, pair?, append, cdr
;; DEPENDED ON BY: NONE
(defun reverse (l) (if (pair? l) (append (reverse (cdr l)) (cons (car l) ())) l))

;; reduce 
;; DEPENDENCIES: pair?, cdr, car
;; DEPENDED ON BY: NONE
(defun reduce (fn l) (if (pair? l) (fn (car l) (reduce fn (cdr l))) (car l)))

;; cdadr 
;; DEPENDENCIES: cdr, car
;; DEPENDED ON BY: NONE
(defun cdadr (l) (cdr (car (cdr l))))

;; or 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: NONE
(defun or (x y) (if x x y))

;; caadr 
;; DEPENDENCIES: cdr, car
;; DEPENDED ON BY: NONE
(defun caadr (l) (car (car (cdr l))))

