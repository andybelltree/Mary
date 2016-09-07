;; caar 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: cond, letrec, firsts
(defun caar (l) (car (car l)))

;; append 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: flatten, reversestr, reverse, inputnchars, quicksort
(defun append (lista listb) (if lista (cons (car lista) (append (cdr lista) listb)) listb))

;; -- 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: fibiter, *, /, fibrec_r
(defun -- (x) (- x 1))

;; cadar 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: cond, seconds, letrec
(defun cadar (l) (car (cdr (car l))))

;; printnewline 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: print
(defun printnewline () (printsym '\n))

;; caadr 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: NONE
(defun caadr (l) (car (car (cdr l))))

;; neg? 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: samesign, %
(defun neg? (x) (< x 0))

;; firsts 
;; DEPENDENCIES: caar
;; DEPENDED ON BY: cars, let
(defun firsts (l) (if l (cons (caar l) (firsts (cdr l)))))

;; lambdachain 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: NONE
(defmacro lambdachain (params vals &rest body) (if (cdr params) `((lambda (,(car params)) (lambdachain ,(cdr params) ,(cdr vals) ,body)) ,(car vals)) `((lambda (,(car params)) ,@body) ,(car vals))))

;; apply 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: NONE
(defmacro apply (fn &rest params) `(,fn ,@params))

;; caddr 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: NONE
(defun caddr (l) (car (cdr (cdr l))))

;; not 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: null?, quicksort, fast_exp, exp, eqz?, *, eq?
(defun not (x) (if x () 't))

;; allwhich 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: quicksort
(defun allwhich (l fn) (if l (if (fn (car l)) (cons (car l) (allwhich (cdr l) fn)) (allwhich (cdr l) fn))))

;; null? 
;; DEPENDENCIES: not
;; DEPENDED ON BY: remove, and, eq?, or
(defun null? (x) (not x))

;; cdadr 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: NONE
(defun cdadr (l) (cdr (car (cdr l))))

;; map 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: in?
(defun map (fn l) (if l (cons (fn (car l)) (map fn (cdr l)))))

;; cond 
;; DEPENDENCIES: cadar, caar
;; DEPENDED ON BY: remove, and, eq?, or
(defmacro cond (&rest options) (if (car options) `(if ,(caar options) ,(cadar options) (cond ,@(cdr options)))))

;; printspace 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: print, printlist
(defun printspace () (printsym '\s))

;; pair? 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: minlist, min, maxlist, sort, reversestr, reverse, coerce, reduce, max
(defun pair? (x) (cdr x))

;; reduce 
;; DEPENDENCIES: pair?
;; DEPENDED ON BY: any?, all?
(defun reduce (fn l) (if (pair? l) (fn (car l) (reduce fn (cdr l))) (car l)))

;; any? 
;; DEPENDENCIES: reduce
;; DEPENDED ON BY: in?
(defun any? (l) (reduce or l))

;; quicksort 
;; DEPENDENCIES: allwhich, not, append
;; DEPENDED ON BY: quicksortatom
(defun quicksort (l) (if (cdr l) (append (quicksort (allwhich (cdr l) (lambda (x) (< x (car l))))) (cons (car l) (quicksort (allwhich (cdr l) (lambda (x) (not (< x (car l)))))))) l))

;; seconds 
;; DEPENDENCIES: cadar
;; DEPENDED ON BY: cadrs, let
(defun seconds (l) (if l (cons (cadar l) (seconds (cdr l)))))

;; cadrs 
;; DEPENDENCIES: seconds
;; DEPENDED ON BY: NONE
(defun cadrs (l) (seconds l))

;; abs 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: /
(defun abs (x) (if (< x 0) (- 0 x) x))

;; and 
;; DEPENDENCIES: null?, cond
;; DEPENDED ON BY: samesign, eq?, isdigit
(defmacro and (&rest vals) `(cond ((null? ',(cdr vals)) (if ,(car vals) 't)) (,(car vals) (and ,@(cdr vals)))))

;; cars 
;; DEPENDENCIES: firsts
;; DEPENDED ON BY: ziplist
(defun cars (l) (firsts l))

;; cddr 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: minlist, maxlist
(defun cddr (l) (cdr (cdr l)))

;; inputnchars 
;; DEPENDENCIES: append
;; DEPENDED ON BY: NONE
(defun inputnchars (n) (if (- n 1) (append (inputnchars (- n 1)) (inputchar)) (inputchar)))

;; flatten 
;; DEPENDENCIES: append
;; DEPENDED ON BY: NONE
(defun flatten (l) (if l (if (atom? (car l)) (cons (car l) (flatten (cdr l))) (append (flatten (car l)) (flatten (cdr l))))))

;; or 
;; DEPENDENCIES: null?, cond
;; DEPENDED ON BY: nievefib, eqz?, /, samesign, eq?
(defmacro or (&rest vals) `(cond ((null? ',(cdr vals)) (if ,(car vals) 't)) (,(car vals) 't) ('t (or ,@(cdr vals)))))

;; eqz? 
;; DEPENDENCIES: not, or
;; DEPENDED ON BY: exp, *, fast_exp
(defun eqz? (a) (not (or (< a 0) (< 0 a))))

;; coerce 
;; DEPENDENCIES: pair?
;; DEPENDED ON BY: quicksortatom, readline, sortatom
(defun coerce (l) (if (pair? l) (cons (car l) (coerce (cdr l))) (car l)))

;; quicksortatom 
;; DEPENDENCIES: quicksort, coerce
;; DEPENDED ON BY: NONE
(defun quicksortatom (a) (coerce (quicksort a)))

;; reverse 
;; DEPENDENCIES: pair?, append
;; DEPENDED ON BY: fibiter, fibrec, reversestr
(defun reverse (l) (if (pair? l) (append (reverse (cdr l)) (cons (car l) ())) l))

;; reversestr 
;; DEPENDENCIES: pair?, reverse, append
;; DEPENDED ON BY: NONE
(defun reversestr (l) (if (pair? l) (append (reverse (cdr l)) (car l)) l))

;; all? 
;; DEPENDENCIES: reduce
;; DEPENDED ON BY: ziplist
(defun all? (l) (reduce and l))

;; last 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: progn
(defun last (l) (if (cdr l) (last (cdr l)) (car l)))

;; pos? 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: samesign
(defun pos? (x) (< 0 x))

;; samesign 
;; DEPENDENCIES: neg?, pos?, or, and
;; DEPENDED ON BY: /
(defun samesign (x y) (or (and (pos? y) (pos? x)) (and (neg? y) (neg? x))))

;; + 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: ++, fibrec_r, fibiter, nievefib, *, /, %
(defun + (x y) (- x (- 0 y)))

;; * 
;; DEPENDENCIES: --, eqz?, +, not
;; DEPENDED ON BY: exp, fast_exp, sq
(defun * (x y) (if (< x y) (* y x) (if (not (eqz? y)) (if (< 0 y) (+ x (* x (-- y))) (- (* x (+ y 1)) x)) 0)))

;; sq 
;; DEPENDENCIES: *
;; DEPENDED ON BY: NONE
(defun sq (x) (* x x))

;; exp 
;; DEPENDENCIES: eqz?, not, *
;; DEPENDED ON BY: **
(defun exp (x y) (if (not (eqz? y)) (* x (exp x (- y 1))) 1))

;; ++ 
;; DEPENDENCIES: +
;; DEPENDED ON BY: fibiter, /
(defun ++ (x) (+ x 1))

;; cadr 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: minlist, maxlist, fibrec_r
(defun cadr (l) (car (cdr l)))

;; list 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: progn, div
(defmacro list (&rest alist) (if alist `(cons ,(car alist) (list ,@(cdr alist)))))

;; progn 
;; DEPENDENCIES: last, list
;; DEPENDED ON BY: prompt, letrec, flet, do, while, printlist, **, print, let
(defmacro progn (&rest body) `(last (list ,@body)))

;; let 
;; DEPENDENCIES: firsts, seconds, progn
;; DEPENDED ON BY: minlist, min, maxlist, sort, foreach, fast_exp, do, fibrec_r, fibiter, readline, max
(defmacro let (bindings &rest body) `((lambda ,(firsts bindings) (progn ,@body)) ,@(seconds bindings)))

;; max 
;; DEPENDENCIES: let, pair?
;; DEPENDED ON BY: NONE
(defmacro max (&rest l) `(if ',(pair? l) (let ((nextmax (max ,@(cdr l)))) (if (< ,(car l) nextmax) nextmax ,(car l))) ,(car l)))

;; min 
;; DEPENDENCIES: let, pair?
;; DEPENDED ON BY: NONE
(defmacro min (&rest l) `(if ',(pair? l) (let ((nextmin (min ,@(cdr l)))) (if (< ,(car l) nextmin) nextmin ,(car l))) ,(car l)))

;; minlist 
;; DEPENDENCIES: pair?, let, cadr, cddr
;; DEPENDED ON BY: sort
(defun minlist (l) (if (pair? l) (let ((this (car l))) (if (< this (cadr l)) (minlist (cons this (cddr l))) (minlist (cdr l)))) (car l)))

;; print 
;; DEPENDENCIES: printspace, progn, printnewline
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
;; DEPENDENCIES: exp, progn, println
;; DEPENDED ON BY: NONE
(defun ** (x y) (if (< y 0) (progn (println 'Exponent 'must 'not 'be 'less 'than 'zero) -1) (exp x y)))

;; printlist 
;; DEPENDENCIES: printspace, progn
;; DEPENDED ON BY: NONE
(defun printlist (words) (if words (progn (printsym (car words)) (printspace) (printlist (cdr words)))))

;; while 
;; DEPENDENCIES: progn
;; DEPENDED ON BY: NONE
(defmacro while (condition &rest body) `(if ,condition (progn ,@body (while ,condition ,@body))))

;; flet 
;; DEPENDENCIES: progn
;; DEPENDED ON BY: NONE
(defmacro flet (bindings &rest body) (if bindings `((lambda () (progn (defun ,@(car bindings)) (flet ,(cdr bindings) ,@body)))) `(progn ,@body)))

;; letrec 
;; DEPENDENCIES: cadar, progn, caar
;; DEPENDED ON BY: do
(defmacro letrec (bindings &rest body) (if bindings `((lambda (,(caar bindings)) (letrec ,(cdr bindings) ,@body)) ,(cadar bindings)) `(progn ,@body)))

;; do 
;; DEPENDENCIES: let, letrec, progn
;; DEPENDED ON BY: fibiter, foreach
(defmacro do (bindings test &rest body) `(letrec ,bindings (let ((result (progn ,@body))) (if ,test (do ,bindings ,test ,@body) result))))

;; foreach 
;; DEPENDENCIES: let, do
;; DEPENDED ON BY: NONE
(defmacro foreach (itemname alist bindings &rest body) `(let ((alist ,alist)) (do ((,itemname (car alist)) (alist (cdr alist)) ,@bindings) alist ,@body)))

;; fibiter 
;; DEPENDENCIES: ++, +, let, --, do, reverse
;; DEPENDED ON BY: NONE
(defun fibiter (n) (reverse (let ((x 0) (f1 0) (f2 1) (nums '(1)) (n (-- n))) (do ((x (++ x)) (f3 (+ f1 f2)) (nums (cons f3 nums)) (f1 f2) (f2 f3)) (< x (++ n)) nums))))

;; > 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: maxlist, eq?, %
(defun > (a b) (< b a))

;; % 
;; DEPENDENCIES: neg?, >, +
;; DEPENDED ON BY: even?, div
(defun % (x y) (if (neg? y) (if (> x y) (if (> x 0) (% (+ x y) y) x) (% (- x y) y)) (if (< x y) (if (< x 0) (% (+ x y) y) x) (% (- x y) y))))

;; eq? 
;; DEPENDENCIES: >, and, or, null?, cond, not
;; DEPENDED ON BY: in?, even?, newline?, nth, replaceall, fibrec_r, remove, nievefib, /
(defun eq? (a b) (cond ((and (atom? a) (atom? b)) (not (or (< a b) (> a b)))) ((and (null? a) (null? b)) 't) ((or (atom? a) (atom? b)) ()) ('t (and (eq? (car a) (car b)) (eq? (cdr a) (cdr b))))))

;; / 
;; DEPENDENCIES: or, ++, abs, --, +, samesign, eq?
;; DEPENDED ON BY: fast_exp, div
(defun / (x y) (if (< (- (abs x) (abs y)) 0) (if (or (eq? x 0) (samesign x y)) 0 -1) (if (samesign x y) (++ (/ (- x y) y)) (-- (/ (+ x y) y)))))

;; div 
;; DEPENDENCIES: list, /, %
;; DEPENDED ON BY: NONE
(defun div (x y) (list (/ x y) (% x y)))

;; nievefib 
;; DEPENDENCIES: or, +, eq?
;; DEPENDED ON BY: NONE
(defun nievefib (n) (if (or (eq? n 0) (eq? n 1)) 1 (+ (nievefib (- n 1)) (nievefib (- n 2)))))

;; remove 
;; DEPENDENCIES: null?, cond, eq?
;; DEPENDED ON BY: sort, defunv
(defun remove (item l) (cond ((null? l) ()) ((eq? (car l) item) (cdr l)) ('t (cons (car l) (remove item (cdr l))))))

;; defunv 
;; DEPENDENCIES: remove
;; DEPENDED ON BY: NONE
(defmacro defunv (name params &rest body) `(defmacro ,name ,params `((lambda ,',(remove '&rest params) ,',@body) ',,@(remove '&rest params))))

;; sort 
;; DEPENDENCIES: remove, minlist, let, pair?
;; DEPENDED ON BY: sortatom
(defun sort (l) (if (pair? l) (let ((themin (minlist l))) (cons themin (sort (remove themin l)))) l))

;; sortatom 
;; DEPENDENCIES: coerce, sort
;; DEPENDED ON BY: NONE
(defun sortatom (a) (coerce (sort a)))

;; fibrec_r 
;; DEPENDENCIES: let, cadr, --, +, eq?
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

;; readline 
;; DEPENDENCIES: let, newline?, coerce
;; DEPENDED ON BY: prompt
(defun readline () (let ((nextchar (inputchar))) (if (newline? nextchar) () (coerce (cons nextchar (readline))))))

;; prompt 
;; DEPENDENCIES: readline, progn, write
;; DEPENDED ON BY: NONE
(defun prompt (prompt) (progn (write prompt) (readline)))

;; even? 
;; DEPENDENCIES: eq?, %
;; DEPENDED ON BY: fast_exp
(defun even? (x) (eq? (% x 2) 0))

;; fast_exp 
;; DEPENDENCIES: let, eqz?, /, not, even?, *
;; DEPENDED ON BY: NONE
(defun fast_exp (x y) (if (not (eqz? y)) (let ((half_exp (fast_exp x (/ y 2)))) (if (even? y) (* half_exp half_exp) (* half_exp (* half_exp x)))) 1))

;; in? 
;; DEPENDENCIES: eq?, any?, map
;; DEPENDED ON BY: isdigit
(defun in? (sym list) (any? (map (lambda (x) (eq? x sym)) list)))

;; isdigit 
;; DEPENDENCIES: in?, and
;; DEPENDED ON BY: NONE
(defun isdigit (sym) (if sym (and (in? (car sym) '(0 1 2 3 4 5 6 7 8 9)) (isdigit (cdr sym))) 't))

;; maxlist 
;; DEPENDENCIES: pair?, >, let, cadr, cddr
;; DEPENDED ON BY: NONE
(defun maxlist (l) (if (pair? l) (let ((this (car l))) (if (> this (cadr l)) (maxlist (cons this (cddr l))) (maxlist (cdr l)))) (car l)))

;; cdar 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: cdrs
(defun cdar (l) (cdr (car l)))

;; cdrs 
;; DEPENDENCIES: cdar
;; DEPENDED ON BY: ziplist
(defun cdrs (l) (if l (cons (cdar l) (cdrs (cdr l)))))

;; ziplist 
;; DEPENDENCIES: cdrs, cars, all?
;; DEPENDED ON BY: NONE
(defun ziplist (ls) (if (all? (cars ls)) (cons (cars ls) (ziplist (cdrs ls)))))

