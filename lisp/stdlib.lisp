;; cdar 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: cdrs
(defun cdar (l) (cdr (car l)))

;; + 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: ++, fibrec_r, fibiter, nievefib, *, /, %
(defun + (x y) (- x (- 0 y)))

;; lambdachain 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: NONE
(defmacro lambdachain (params vals &rest body) (if (cdr params) `((lambda (,(car params)) (lambdachain ,(cdr params) ,(cdr vals) ,body)) ,(car vals)) `((lambda (,(car params)) ,@body) ,(car vals))))

;; neg? 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: samesign, %
(defun neg? (x) (< x 0))

;; allwhich 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: quicksort
(defun allwhich (l fn) (if l (if (fn (car l)) (cons (car l) (allwhich (cdr l) fn)) (allwhich (cdr l) fn))))

;; printspace 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: print, printlist
(defun printspace () (printsym '\s))

;; last 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: progn
(defun last (l) (if (cdr l) (last (cdr l)) (car l)))

;; cadr 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: minlist, maxlist, fibrec_r
(defun cadr (l) (car (cdr l)))

;; pos? 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: samesign
(defun pos? (x) (< 0 x))

;; > 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: %, eq?, maxlist
(defun > (a b) (< b a))

;; % 
;; DEPENDENCIES: neg?, >, +
;; DEPENDED ON BY: even?, div
(defun % (x y) (if (neg? y) (if (> x y) (if (> x 0) (% (+ x y) y) x) (% (- x y) y)) (if (< x y) (if (< x 0) (% (+ x y) y) x) (% (- x y) y))))

;; -- 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: fibiter, *, /, fibrec_r
(defun -- (x) (- x 1))

;; pair? 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: max, minlist, min, maxlist, reversestr, reverse, coerce, reduce, sort
(defun pair? (x) (cdr x))

;; coerce 
;; DEPENDENCIES: pair?
;; DEPENDED ON BY: sortatom, readline, quicksortatom
(defun coerce (l) (if (pair? l) (cons (car l) (coerce (cdr l))) (car l)))

;; cadar 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: cond, seconds, letrec
(defun cadar (l) (car (cdr (car l))))

;; not 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: null?, quicksort, fast_exp, exp, eqz?, *, eq?
(defun not (x) (if x () 't))

;; cdrs 
;; DEPENDENCIES: cdar
;; DEPENDED ON BY: ziplist
(defun cdrs (l) (if l (cons (cdar l) (cdrs (cdr l)))))

;; append 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: flatten, reversestr, reverse, inputnchars, quicksort
(defun append (lista listb) (if lista (cons (car lista) (append (cdr lista) listb)) listb))

;; quicksort 
;; DEPENDENCIES: append, not, allwhich
;; DEPENDED ON BY: quicksortatom
(defun quicksort (l) (if (cdr l) (append (quicksort (allwhich (cdr l) (lambda (x) (< x (car l))))) (cons (car l) (quicksort (allwhich (cdr l) (lambda (x) (not (< x (car l)))))))) l))

;; quicksortatom 
;; DEPENDENCIES: coerce, quicksort
;; DEPENDED ON BY: NONE
(defun quicksortatom (a) (coerce (quicksort a)))

;; inputnchars 
;; DEPENDENCIES: append
;; DEPENDED ON BY: NONE
(defun inputnchars (n) (if (- n 1) (append (inputnchars (- n 1)) (inputchar)) (inputchar)))

;; reverse 
;; DEPENDENCIES: append, pair?
;; DEPENDED ON BY: fibiter, fibrec, reversestr
(defun reverse (l) (if (pair? l) (append (reverse (cdr l)) (cons (car l) ())) l))

;; reversestr 
;; DEPENDENCIES: append, reverse, pair?
;; DEPENDED ON BY: NONE
(defun reversestr (l) (if (pair? l) (append (reverse (cdr l)) (car l)) l))

;; flatten 
;; DEPENDENCIES: append
;; DEPENDED ON BY: NONE
(defun flatten (l) (if l (if (atom? (car l)) (cons (car l) (flatten (cdr l))) (append (flatten (car l)) (flatten (cdr l))))))

;; ++ 
;; DEPENDENCIES: +
;; DEPENDED ON BY: fibiter, /
(defun ++ (x) (+ x 1))

;; printnewline 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: print
(defun printnewline () (printsym '\n))

;; caar 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: cond, letrec, firsts
(defun caar (l) (car (car l)))

;; firsts 
;; DEPENDENCIES: caar
;; DEPENDED ON BY: cars, let
(defun firsts (l) (if l (cons (caar l) (firsts (cdr l)))))

;; cars 
;; DEPENDENCIES: firsts
;; DEPENDED ON BY: ziplist
(defun cars (l) (firsts l))

;; cond 
;; DEPENDENCIES: cadar, caar
;; DEPENDED ON BY: remove, eq?, or, and
(defmacro cond (&rest options) (if (car options) `(if ,(caar options) ,(cadar options) (cond ,@(cdr options)))))

;; map 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: in?
(defun map (fn l) (if l (cons (fn (car l)) (map fn (cdr l)))))

;; cdadr 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: NONE
(defun cdadr (l) (cdr (car (cdr l))))

;; cddr 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: minlist, maxlist
(defun cddr (l) (cdr (cdr l)))

;; abs 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: /
(defun abs (x) (if (< x 0) (- 0 x) x))

;; list 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: progn, div
(defmacro list (&rest alist) (if alist `(cons ,(car alist) (list ,@(cdr alist)))))

;; progn 
;; DEPENDENCIES: last, list
;; DEPENDED ON BY: letrec, print, flet, while, do, printlist, **, prompt, let
(defmacro progn (&rest body) `(last (list ,@body)))

;; printlist 
;; DEPENDENCIES: progn, printspace
;; DEPENDED ON BY: NONE
(defun printlist (words) (if words (progn (printsym (car words)) (printspace) (printlist (cdr words)))))

;; flet 
;; DEPENDENCIES: progn
;; DEPENDED ON BY: NONE
(defmacro flet (bindings &rest body) (if bindings `((lambda () (progn (defun ,@(car bindings)) (flet ,(cdr bindings) ,@body)))) `(progn ,@body)))

;; apply 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: NONE
(defmacro apply (fn &rest params) `(,fn ,@params))

;; caddr 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: NONE
(defun caddr (l) (car (cdr (cdr l))))

;; reduce 
;; DEPENDENCIES: pair?
;; DEPENDED ON BY: any?, all?
(defun reduce (fn l) (if (pair? l) (fn (car l) (reduce fn (cdr l))) (car l)))

;; all? 
;; DEPENDENCIES: reduce
;; DEPENDED ON BY: ziplist
(defun all? (l) (reduce and l))

;; ziplist 
;; DEPENDENCIES: cars, cdrs, all?
;; DEPENDED ON BY: NONE
(defun ziplist (ls) (if (all? (cars ls)) (cons (cars ls) (ziplist (cdrs ls)))))

;; caadr 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: NONE
(defun caadr (l) (car (car (cdr l))))

;; while 
;; DEPENDENCIES: progn
;; DEPENDED ON BY: NONE
(defmacro while (condition &rest body) `(if ,condition (progn ,@body (while ,condition ,@body))))

;; null? 
;; DEPENDENCIES: not
;; DEPENDED ON BY: remove, eq?, or, and
(defun null? (x) (not x))

;; and 
;; DEPENDENCIES: null?, cond
;; DEPENDED ON BY: isdigit, eq?, samesign
(defmacro and (&rest vals) `(cond ((null? ',(cdr vals)) (if ,(car vals) 't)) (,(car vals) (and ,@(cdr vals)))))

;; or 
;; DEPENDENCIES: null?, cond
;; DEPENDED ON BY: nievefib, eqz?, eq?, samesign, /
(defmacro or (&rest vals) `(cond ((null? ',(cdr vals)) (if ,(car vals) 't)) (,(car vals) 't) ('t (or ,@(cdr vals)))))

;; samesign 
;; DEPENDENCIES: or, pos?, neg?, and
;; DEPENDED ON BY: /
(defun samesign (x y) (or (and (pos? y) (pos? x)) (and (neg? y) (neg? x))))

;; eqz? 
;; DEPENDENCIES: not, or
;; DEPENDED ON BY: exp, *, fast_exp
(defun eqz? (a) (not (or (< a 0) (< 0 a))))

;; * 
;; DEPENDENCIES: eqz?, not, --, +
;; DEPENDED ON BY: exp, fast_exp, sq
(defun * (x y) (if (< x y) (* y x) (if (not (eqz? y)) (if (< 0 y) (+ x (* x (-- y))) (- (* x (+ y 1)) x)) 0)))

;; sq 
;; DEPENDENCIES: *
;; DEPENDED ON BY: NONE
(defun sq (x) (* x x))

;; exp 
;; DEPENDENCIES: eqz?, *, not
;; DEPENDED ON BY: **
(defun exp (x y) (if (not (eqz? y)) (* x (exp x (- y 1))) 1))

;; eq? 
;; DEPENDENCIES: null?, and, not, or, >, cond
;; DEPENDED ON BY: in?, even?, newline?, nth, replaceall, fibrec_r, remove, nievefib, /
(defun eq? (a b) (cond ((and (atom? a) (atom? b)) (not (or (< a b) (> a b)))) ((and (null? a) (null? b)) 't) ((or (atom? a) (atom? b)) ()) ('t (and (eq? (car a) (car b)) (eq? (cdr a) (cdr b))))))

;; / 
;; DEPENDENCIES: ++, eq?, abs, samesign, or, --, +
;; DEPENDED ON BY: fast_exp, div
(defun / (x y) (if (< (- (abs x) (abs y)) 0) (if (or (eq? x 0) (samesign x y)) 0 -1) (if (samesign x y) (++ (/ (- x y) y)) (-- (/ (+ x y) y)))))

;; div 
;; DEPENDENCIES: %, list, /
;; DEPENDED ON BY: NONE
(defun div (x y) (list (/ x y) (% x y)))

;; nievefib 
;; DEPENDENCIES: eq?, or, +
;; DEPENDED ON BY: NONE
(defun nievefib (n) (if (or (eq? n 0) (eq? n 1)) 1 (+ (nievefib (- n 1)) (nievefib (- n 2)))))

;; replaceall 
;; DEPENDENCIES: eq?
;; DEPENDED ON BY: NONE
(defun replaceall (a b l) (if l (if (atom? l) (if (eq? l a) b l) (cons (replaceall a b (car l)) (replaceall a b (cdr l))))))

;; nth 
;; DEPENDENCIES: eq?
;; DEPENDED ON BY: NONE
(defun nth (l n) (if (eq? n 0) (car l) (nth (cdr l) (- n 1))))

;; newline? 
;; DEPENDENCIES: eq?
;; DEPENDED ON BY: readline
(defun newline? (char) (eq? char '\n))

;; even? 
;; DEPENDENCIES: %, eq?
;; DEPENDED ON BY: fast_exp
(defun even? (x) (eq? (% x 2) 0))

;; remove 
;; DEPENDENCIES: eq?, null?, cond
;; DEPENDED ON BY: sort, defunv
(defun remove (item l) (cond ((null? l) ()) ((eq? (car l) item) (cdr l)) ('t (cons (car l) (remove item (cdr l))))))

;; defunv 
;; DEPENDENCIES: remove
;; DEPENDED ON BY: NONE
(defmacro defunv (name params &rest body) `(defmacro ,name ,params `((lambda ,',(remove '&rest params) ,',@body) ',,@(remove '&rest params))))

;; any? 
;; DEPENDENCIES: reduce
;; DEPENDED ON BY: in?
(defun any? (l) (reduce or l))

;; in? 
;; DEPENDENCIES: map, any?, eq?
;; DEPENDED ON BY: isdigit
(defun in? (sym list) (any? (map (lambda (x) (eq? x sym)) list)))

;; isdigit 
;; DEPENDENCIES: in?, and
;; DEPENDED ON BY: NONE
(defun isdigit (sym) (if sym (and (in? (car sym) '(0 1 2 3 4 5 6 7 8 9)) (isdigit (cdr sym))) 't))

;; letrec 
;; DEPENDENCIES: progn, cadar, caar
;; DEPENDED ON BY: do
(defmacro letrec (bindings &rest body) (if bindings `((lambda (,(caar bindings)) (letrec ,(cdr bindings) ,@body)) ,(cadar bindings)) `(progn ,@body)))

;; print 
;; DEPENDENCIES: progn, printspace, printnewline
;; DEPENDED ON BY: println, write
(defmacro print (newline? &rest params) `(if ',params (progn (printsym ,(car params)) (printspace) (print ,newline? ,@(cdr params))) (if ,newline? (printnewline))))

;; write 
;; DEPENDENCIES: print
;; DEPENDED ON BY: prompt
(defmacro write (&rest params) `(print () ,@params))

;; println 
;; DEPENDENCIES: print
;; DEPENDED ON BY: **
(defmacro println (&rest params) `(print 't ,@params))

;; ** 
;; DEPENDENCIES: progn, println, exp
;; DEPENDED ON BY: NONE
(defun ** (x y) (if (< y 0) (progn (println 'Exponent 'must 'not 'be 'less 'than 'zero) -1) (exp x y)))

;; seconds 
;; DEPENDENCIES: cadar
;; DEPENDED ON BY: cadrs, let
(defun seconds (l) (if l (cons (cadar l) (seconds (cdr l)))))

;; let 
;; DEPENDENCIES: seconds, firsts, progn
;; DEPENDED ON BY: max, minlist, min, maxlist, foreach, fast_exp, do, fibrec_r, fibiter, readline, sort
(defmacro let (bindings &rest body) `((lambda ,(firsts bindings) (progn ,@body)) ,@(seconds bindings)))

;; readline 
;; DEPENDENCIES: coerce, newline?, let
;; DEPENDED ON BY: prompt
(defun readline () (let ((nextchar (inputchar))) (if (newline? nextchar) () (coerce (cons nextchar (readline))))))

;; prompt 
;; DEPENDENCIES: progn, write, readline
;; DEPENDED ON BY: NONE
(defun prompt (prompt) (progn (write prompt) (readline)))

;; fibrec_r 
;; DEPENDENCIES: eq?, let, --, cadr, +
;; DEPENDED ON BY: fibrec, fib
(defun fibrec_r (n) (if (eq? n 1) '(1 1) (let ((discovered (fibrec_r (-- n)))) (cons (+ (car discovered) (cadr discovered)) discovered))))

;; fib 
;; DEPENDENCIES: fibrec_r
;; DEPENDED ON BY: NONE
(defun fib (n) (car (fibrec_r n)))

;; fibrec 
;; DEPENDENCIES: fibrec_r, reverse
;; DEPENDED ON BY: NONE
(defun fibrec (n) (reverse (fibrec_r n)))

;; do 
;; DEPENDENCIES: letrec, progn, let
;; DEPENDED ON BY: fibiter, foreach
(defmacro do (bindings test &rest body) `(letrec ,bindings (let ((result (progn ,@body))) (if ,test (do ,bindings ,test ,@body) result))))

;; fibiter 
;; DEPENDENCIES: reverse, let, --, ++, do, +
;; DEPENDED ON BY: NONE
(defun fibiter (n) (reverse (let ((x 0) (f1 0) (f2 1) (nums '(1)) (n (-- n))) (do ((x (++ x)) (f3 (+ f1 f2)) (nums (cons f3 nums)) (f1 f2) (f2 f3)) (< x (++ n)) nums))))

;; fast_exp 
;; DEPENDENCIES: eqz?, *, even?, /, not, let
;; DEPENDED ON BY: NONE
(defun fast_exp (x y) (if (not (eqz? y)) (let ((half_exp (fast_exp x (/ y 2)))) (if (even? y) (* half_exp half_exp) (* half_exp (* half_exp x)))) 1))

;; foreach 
;; DEPENDENCIES: do, let
;; DEPENDED ON BY: NONE
(defmacro foreach (itemname alist bindings &rest body) `(let ((alist ,alist)) (do ((,itemname (car alist)) (alist (cdr alist)) ,@bindings) alist ,@body)))

;; maxlist 
;; DEPENDENCIES: let, pair?, >, cadr, cddr
;; DEPENDED ON BY: NONE
(defun maxlist (l) (if (pair? l) (let ((this (car l))) (if (> this (cadr l)) (maxlist (cons this (cddr l))) (maxlist (cdr l)))) (car l)))

;; min 
;; DEPENDENCIES: let, pair?
;; DEPENDED ON BY: NONE
(defmacro min (&rest l) `(if ',(pair? l) (let ((nextmin (min ,@(cdr l)))) (if (< ,(car l) nextmin) nextmin ,(car l))) ,(car l)))

;; minlist 
;; DEPENDENCIES: let, pair?, cadr, cddr
;; DEPENDED ON BY: sort
(defun minlist (l) (if (pair? l) (let ((this (car l))) (if (< this (cadr l)) (minlist (cons this (cddr l))) (minlist (cdr l)))) (car l)))

;; sort 
;; DEPENDENCIES: remove, let, minlist, pair?
;; DEPENDED ON BY: sortatom
(defun sort (l) (if (pair? l) (let ((themin (minlist l))) (cons themin (sort (remove themin l)))) l))

;; sortatom 
;; DEPENDENCIES: sort, coerce
;; DEPENDED ON BY: NONE
(defun sortatom (a) (coerce (sort a)))

;; max 
;; DEPENDENCIES: let, pair?
;; DEPENDED ON BY: NONE
(defmacro max (&rest l) `(if ',(pair? l) (let ((nextmax (max ,@(cdr l)))) (if (< ,(car l) nextmax) nextmax ,(car l))) ,(car l)))

;; cadrs 
;; DEPENDENCIES: seconds
;; DEPENDED ON BY: NONE
(defun cadrs (l) (seconds l))

