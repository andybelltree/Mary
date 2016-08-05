;; > 
;; DEPENDENCIES: <
;; DEPENDED ON BY: eq?, %, maxlist
(defun > (a b) (< b a))

;; list 
;; DEPENDENCIES: if, quasiquote, unquote, cons, splice, cdr, car
;; DEPENDED ON BY: progn, div
(defmacro list (&rest alist) (if alist `(cons ,(car alist) (list ,@(cdr alist)))))

;; cddr 
;; DEPENDENCIES: cdr
;; DEPENDED ON BY: maxlist, minlist
(defun cddr (l) (cdr (cdr l)))

;; -- 
;; DEPENDENCIES: -
;; DEPENDED ON BY: *, /, fibiter, fibrec_r
(defun -- (x) (- x 1))

;; apply 
;; DEPENDENCIES: quasiquote, unquote, splice
;; DEPENDED ON BY: 
(defmacro apply (fn &rest params) `(,fn ,@params))

;; last 
;; DEPENDENCIES: if, car, cdr
;; DEPENDED ON BY: progn
(defun last (l) (if (cdr l) (last (cdr l)) (car l)))

;; caadr 
;; DEPENDENCIES: car, cdr
;; DEPENDED ON BY: 
(defun caadr (l) (car (car (cdr l))))

;; id 
;; DEPENDENCIES: 
;; DEPENDED ON BY: 
(defun id (x) x)

;; caar 
;; DEPENDENCIES: car
;; DEPENDED ON BY: firsts, letrec, cond
(defun caar (l) (car (car l)))

;; null? 
;; DEPENDENCIES: if, quote
;; DEPENDED ON BY: eq?, or, remove, not, and
(defun null? (x) (if x () 't))

;; printnewline 
;; DEPENDENCIES: quote, printsym
;; DEPENDED ON BY: print
(defun printnewline () (printsym '\n))

;; allwhich 
;; DEPENDENCIES: if, car, fn, cons, cdr
;; DEPENDED ON BY: quicksort
(defun allwhich (l fn) (if l (if (fn (car l)) (cons (car l) (allwhich (cdr l) fn)) (allwhich (cdr l) fn))))

;; append 
;; DEPENDENCIES: cons, if, car, cdr
;; DEPENDED ON BY: flatten, inputnchars, reversestr, quicksort, reverse
(defun append (lista listb) (if lista (cons (car lista) (append (cdr lista) listb)) listb))

;; inputnchars 
;; DEPENDENCIES: if, inputchar, append, -
;; DEPENDED ON BY: 
(defun inputnchars (n) (if (- n 1) (append (inputnchars (- n 1)) (inputchar)) (inputchar)))

;; caddr 
;; DEPENDENCIES: car, cdr
;; DEPENDED ON BY: 
(defun caddr (l) (car (cdr (cdr l))))

;; not 
;; DEPENDENCIES: null?
;; DEPENDED ON BY: eq?, eqz?, fast_exp, exp, quicksort, *
(defun not (x) (null? x))

;; printspace 
;; DEPENDENCIES: quote, printsym
;; DEPENDED ON BY: printlist, print
(defun printspace () (printsym '\s))

;; cdar 
;; DEPENDENCIES: car, cdr
;; DEPENDED ON BY: cdrs
(defun cdar (l) (cdr (car l)))

;; cdrs 
;; DEPENDENCIES: cons, if, cdar, cdr
;; DEPENDED ON BY: ziplist
(defun cdrs (l) (if l (cons (cdar l) (cdrs (cdr l)))))

;; pair? 
;; DEPENDENCIES: cdr
;; DEPENDED ON BY: maxlist, sort, coerce, reverse, min, reduce, reversestr, quicksort, minlist, max
(defun pair? (x) (cdr x))

;; cadr 
;; DEPENDENCIES: car, cdr
;; DEPENDED ON BY: minlist, maxlist, fibrec_r
(defun cadr (l) (car (cdr l)))

;; cadar 
;; DEPENDENCIES: car, cdr
;; DEPENDED ON BY: seconds, letrec, cond
(defun cadar (l) (car (cdr (car l))))

;; cond 
;; DEPENDENCIES: if, quasiquote, unquote, cadar, splice, cdr, caar, car
;; DEPENDED ON BY: eq?, or, remove, and
(defmacro cond (&rest options) (if (car options) `(if ,(caar options) ,(cadar options) (cond ,@(cdr options)))))

;; seconds 
;; DEPENDENCIES: cons, if, cadar, cdr
;; DEPENDED ON BY: let, cadrs
(defun seconds (l) (if l (cons (cadar l) (seconds (cdr l)))))

;; cadrs 
;; DEPENDENCIES: seconds
;; DEPENDED ON BY: 
(defun cadrs (l) (seconds l))

;; firsts 
;; DEPENDENCIES: cons, if, caar, cdr
;; DEPENDED ON BY: let, cars
(defun firsts (l) (if l (cons (caar l) (firsts (cdr l)))))

;; cars 
;; DEPENDENCIES: firsts
;; DEPENDED ON BY: ziplist
(defun cars (l) (firsts l))

;; reverse 
;; DEPENDENCIES: if, car, pair?, append, cons, cdr
;; DEPENDED ON BY: fibiter, reversestr, fibrec
(defun reverse (l) (if (pair? l) (append (reverse (cdr l)) (cons (car l) ())) l))

;; reversestr 
;; DEPENDENCIES: if, reverse, car, pair?, append, cdr
;; DEPENDED ON BY: 
(defun reversestr (l) (if (pair? l) (append (reverse (cdr l)) (car l)) l))

;; and 
;; DEPENDENCIES: if, quasiquote, unquote, null?, cond, splice, cdr, quote, car
;; DEPENDED ON BY: samesign, eq?, isdigit
(defmacro and (&rest vals) `(cond ((null? ',(cdr vals)) (if ,(car vals) 't)) (,(car vals) (and ,@(cdr vals))) ('t ())))

;; neg? 
;; DEPENDENCIES: <
;; DEPENDED ON BY: samesign, %
(defun neg? (x) (< x 0))

;; flatten 
;; DEPENDENCIES: if, atom?, append, cons, cdr, car
;; DEPENDED ON BY: 
(defun flatten (l) (if l (if (atom? (car l)) (cons (car l) (flatten (cdr l))) (append (flatten (car l)) (flatten (cdr l))))))

;; coerce 
;; DEPENDENCIES: if, car, pair?, cons, cdr
;; DEPENDED ON BY: quicksortatom, sortatom, readline
(defun coerce (l) (if (pair? l) (cons (car l) (coerce (cdr l))) (car l)))

;; pos? 
;; DEPENDENCIES: <
;; DEPENDED ON BY: samesign
(defun pos? (x) (< 0 x))

;; or 
;; DEPENDENCIES: if, quasiquote, unquote, null?, cond, splice, cdr, quote, car
;; DEPENDED ON BY: nievefib, samesign, eq?, /, eqz?
(defmacro or (&rest vals) `(cond ((null? ',(cdr vals)) (if ,(car vals) 't)) (,(car vals) 't) ('t (or ,@(cdr vals)))))

;; eq? 
;; DEPENDENCIES: atom?, <, null?, cond, not, and, cdr, quote, car, >, or
;; DEPENDED ON BY: even?, fibrec_r, remove, newline?, nievefib, in?, /, replaceall, nth
(defun eq? (a b) (cond ((and (atom? a) (atom? b)) (not (or (< a b) (> a b)))) ((and (null? a) (null? b)) 't) ((or (atom? a) (atom? b)) ()) ('t (and (eq? (car a) (car b)) (eq? (cdr a) (cdr b))))))

;; replaceall 
;; DEPENDENCIES: if, atom?, eq?, cons, cdr, car
;; DEPENDED ON BY: 
(defun replaceall (a b l) (if l (if (atom? l) (if (eq? l a) b l) (cons (replaceall a b (car l)) (replaceall a b (cdr l))))))

;; newline? 
;; DEPENDENCIES: eq?, quote
;; DEPENDED ON BY: readline
(defun newline? (char) (eq? char '\n))

;; samesign 
;; DEPENDENCIES: pos?, neg?, or, and
;; DEPENDED ON BY: /
(defun samesign (x y) (or (and (pos? y) (pos? x)) (and (neg? y) (neg? x))))

;; nth 
;; DEPENDENCIES: if, eq?, car, -, cdr
;; DEPENDED ON BY: 
(defun nth (l n) (if (eq? n 0) (car l) (nth (cdr l) (- n 1))))

;; reduce 
;; DEPENDENCIES: if, car, pair?, fn, cdr
;; DEPENDED ON BY: all?, any?
(defun reduce (fn l) (if (pair? l) (fn (car l) (reduce fn (cdr l))) (car l)))

;; + 
;; DEPENDENCIES: -
;; DEPENDED ON BY: fibrec_r, %, ++, nievefib, *, /, fibiter
(defun + (x y) (- x (- 0 y)))

;; ++ 
;; DEPENDENCIES: +
;; DEPENDED ON BY: /, fibiter
(defun ++ (x) (+ x 1))

;; % 
;; DEPENDENCIES: if, neg?, <, -, >, +
;; DEPENDED ON BY: even?, div
(defun % (x y) (if (neg? y) (if (> x y) (if (> x 0) (% (+ x y) y) x) (% (- x y) y)) (if (< x y) (if (< x 0) (% (+ x y) y) x) (% (- x y) y))))

;; even? 
;; DEPENDENCIES: eq?, %
;; DEPENDED ON BY: fast_exp
(defun even? (x) (eq? (% x 2) 0))

;; lambdachain 
;; DEPENDENCIES: if, quasiquote, unquote, lambda, splice, cdr, car
;; DEPENDED ON BY: 
(defmacro lambdachain (params vals &rest body) (if (cdr params) `((lambda (,(car params)) (lambdachain ,(cdr params) ,(cdr vals) ,body)) ,(car vals)) `((lambda (,(car params)) ,@body) ,(car vals))))

;; nievefib 
;; DEPENDENCIES: if, eq?, +, or, -
;; DEPENDED ON BY: 
(defun nievefib (n) (if (or (eq? n 0) (eq? n 1)) 1 (+ (nievefib (- n 1)) (nievefib (- n 2)))))

;; all? 
;; DEPENDENCIES: reduce
;; DEPENDED ON BY: ziplist
(defun all? (l) (reduce and l))

;; ziplist 
;; DEPENDENCIES: if, cdrs, cons, cars, all?
;; DEPENDED ON BY: 
(defun ziplist (ls) (if (all? (cars ls)) (cons (cars ls) (ziplist (cdrs ls)))))

;; eqz? 
;; DEPENDENCIES: <, not, or
;; DEPENDED ON BY: *, exp, fast_exp
(defun eqz? (a) (not (or (< a 0) (< 0 a))))

;; * 
;; DEPENDENCIES: if, --, <, not, -, eqz?, +
;; DEPENDED ON BY: double, exp, sq, fast_exp
(defun * (x y) (if (< x y) (* y x) (if (not (eqz? y)) (if (< 0 y) (+ x (* x (-- y))) (- (* x (+ y 1)) x)) 0)))

;; sq 
;; DEPENDENCIES: *
;; DEPENDED ON BY: 
(defun sq (x) (* x x))

;; exp 
;; DEPENDENCIES: if, eqz?, *, not, -
;; DEPENDED ON BY: **
(defun exp (x y) (if (not (eqz? y)) (* x (exp x (- y 1))) 1))

;; double 
;; DEPENDENCIES: *
;; DEPENDED ON BY: 
(defun double (x) (* 2 x))

;; map 
;; DEPENDENCIES: if, car, fn, cons, cdr
;; DEPENDED ON BY: in?
(defun map (fn l) (if l (cons (fn (car l)) (map fn (cdr l)))))

;; progn 
;; DEPENDENCIES: last, list, quasiquote, splice
;; DEPENDED ON BY: prompt, let, letrec, **, while, do, flet, printlist, print
(defmacro progn (&rest body) `(last (list ,@body)))

;; print 
;; DEPENDENCIES: if, quasiquote, unquote, printnewline, printsym, progn, splice, printspace, cdr, quote, car
;; DEPENDED ON BY: println, write
(defmacro print (newline? &rest params) `(if ',params (progn (printsym ,(car params)) (printspace) (print ,newline? ,@(cdr params))) (if ,newline? (printnewline))))

;; write 
;; DEPENDENCIES: quasiquote, splice, print
;; DEPENDED ON BY: prompt
(defmacro write (&rest params) `(print () ,@params))

;; println 
;; DEPENDENCIES: quote, quasiquote, splice, print
;; DEPENDED ON BY: **
(defmacro println (&rest params) `(print 't ,@params))

;; printlist 
;; DEPENDENCIES: if, car, printsym, progn, printspace, cdr
;; DEPENDED ON BY: 
(defun printlist (words) (if words (progn (printsym (car words)) (printspace) (printlist (cdr words)))))

;; flet 
;; DEPENDENCIES: if, quasiquote, unquote, defun, lambda, splice, progn, cdr, car
;; DEPENDED ON BY: 
(defmacro flet (bindings &rest body) (if bindings `((lambda () (progn (defun ,@(car bindings)) (flet ,(cdr bindings) ,@body)))) `(progn ,@body)))

;; while 
;; DEPENDENCIES: if, quasiquote, unquote, splice, progn
;; DEPENDED ON BY: 
(defmacro while (condition &rest body) `(if ,condition (progn ,@body (while ,condition ,@body))))

;; ** 
;; DEPENDENCIES: if, quote, <, exp, println, progn
;; DEPENDED ON BY: 
(defun ** (x y) (if (< y 0) (progn (println 'Exponent 'must 'not 'be 'less 'than 'zero) -1) (exp x y)))

;; letrec 
;; DEPENDENCIES: if, quasiquote, unquote, lambda, splice, cadar, progn, cdr, caar
;; DEPENDED ON BY: quicksort, do
(defmacro letrec (bindings &rest body) (if bindings `((lambda (,(caar bindings)) (letrec ,(cdr bindings) ,@body)) ,(cadar bindings)) `(progn ,@body)))

;; quicksort 
;; DEPENDENCIES: if, pair?, not, allwhich, secondlist, x, letrec, pivot, append, <, cons, lambda, cdr, car, firstlist
;; DEPENDED ON BY: quicksortatom
(defun quicksort (l) (if (pair? l) (letrec ((pivot (car l)) (firstlist (quicksort (allwhich (cdr l) (lambda (x) (< x pivot))))) (secondlist (quicksort (allwhich (cdr l) (lambda (x) (not (< x pivot))))))) (append firstlist (cons pivot secondlist))) l))

;; quicksortatom 
;; DEPENDENCIES: coerce, quicksort
;; DEPENDED ON BY: 
(defun quicksortatom (a) (coerce (quicksort a)))

;; let 
;; DEPENDENCIES: quasiquote, unquote, firsts, lambda, splice, progn, seconds
;; DEPENDED ON BY: maxlist, fibrec_r, sort, fast_exp, min, do, minlist, max, fibiter, readline, foreach
(defmacro let (bindings &rest body) `((lambda ,(firsts bindings) (progn ,@body)) ,@(seconds bindings)))

;; readline 
;; DEPENDENCIES: if, nextchar, cons, inputchar, newline?, coerce, let
;; DEPENDED ON BY: prompt
(defun readline () (let ((nextchar (inputchar))) (if (newline? nextchar) () (coerce (cons nextchar (readline))))))

;; max 
;; DEPENDENCIES: if, quasiquote, unquote, pair?, <, nextmax, splice, cdr, quote, car, let
;; DEPENDED ON BY: 
(defmacro max (&rest l) `(if ',(pair? l) (let ((nextmax (max ,@(cdr l)))) (if (< ,(car l) nextmax) nextmax ,(car l))) ,(car l)))

;; minlist 
;; DEPENDENCIES: if, pair?, this, <, cons, cddr, cdr, cadr, car, let
;; DEPENDED ON BY: sort
(defun minlist (l) (if (pair? l) (let ((this (car l))) (if (< this (cadr l)) (minlist (cons this (cddr l))) (minlist (cdr l)))) (car l)))

;; do 
;; DEPENDENCIES: result, if, quasiquote, letrec, unquote, splice, progn, let
;; DEPENDED ON BY: fibiter, foreach
(defmacro do (bindings test &rest body) `(letrec ,bindings (let ((result (progn ,@body))) (if ,test (do ,bindings ,test ,@body) result))))

;; foreach 
;; DEPENDENCIES: quasiquote, unquote, splice, do, cdr, alist, car, let
;; DEPENDED ON BY: 
(defmacro foreach (itemname alist bindings &rest body) `(let ((alist ,alist)) (do ((,itemname (car alist)) (alist (cdr alist)) ,@bindings) alist ,@body)))

;; fibiter 
;; DEPENDENCIES: --, f3, ++, do, quote, f1, nums, 1, x, <, n, cons, reverse, f2, let, +
;; DEPENDED ON BY: 
(defun fibiter (n) (reverse (let ((x 0) (f1 0) (f2 1) (nums '(1)) (n (-- n))) (do ((x (++ x)) (f3 (+ f1 f2)) (nums (cons f3 nums)) (f1 f2) (f2 f3)) (< x (++ n)) nums))))

;; min 
;; DEPENDENCIES: if, quasiquote, unquote, pair?, <, splice, nextmin, cdr, quote, car, let
;; DEPENDED ON BY: 
(defmacro min (&rest l) `(if ',(pair? l) (let ((nextmin (min ,@(cdr l)))) (if (< ,(car l) nextmin) nextmin ,(car l))) ,(car l)))

;; fibrec_r 
;; DEPENDENCIES: if, eq?, --, discovered, cons, quote, cadr, car, let, 1, +
;; DEPENDED ON BY: fib, fibrec
(defun fibrec_r (n) (if (eq? n 1) '(1 1) (let ((discovered (fibrec_r (-- n)))) (cons (+ (car discovered) (cadr discovered)) discovered))))

;; fibrec 
;; DEPENDENCIES: fibrec_r, reverse
;; DEPENDED ON BY: 
(defun fibrec (n) (reverse (fibrec_r n)))

;; maxlist 
;; DEPENDENCIES: if, pair?, this, cons, cddr, cdr, cadr, car, let, >
;; DEPENDED ON BY: 
(defun maxlist (l) (if (pair? l) (let ((this (car l))) (if (> this (cadr l)) (maxlist (cons this (cddr l))) (maxlist (cdr l)))) (car l)))

;; prompt 
;; DEPENDENCIES: readline, progn, write
;; DEPENDED ON BY: 
(defun prompt (prompt) (progn (write prompt) (readline)))

;; abs 
;; DEPENDENCIES: if, -, <
;; DEPENDED ON BY: /
(defun abs (x) (if (< x 0) (- 0 x) x))

;; / 
;; DEPENDENCIES: if, eq?, --, <, ++, abs, -, +, samesign, or
;; DEPENDED ON BY: div, fast_exp
(defun / (x y) (if (< (- (abs x) (abs y)) 0) (if (or (eq? x 0) (samesign x y)) 0 -1) (if (samesign x y) (++ (/ (- x y) y)) (-- (/ (+ x y) y)))))

;; fast_exp 
;; DEPENDENCIES: if, /, *, not, even?, eqz?, half_exp, let
;; DEPENDED ON BY: 
(defun fast_exp (x y) (if (not (eqz? y)) (let ((half_exp (fast_exp x (/ y 2)))) (if (even? y) (* half_exp half_exp) (* half_exp (* half_exp x)))) 1))

;; div 
;; DEPENDENCIES: /, list, %
;; DEPENDED ON BY: 
(defun div (x y) (list (/ x y) (% x y)))

;; any? 
;; DEPENDENCIES: reduce
;; DEPENDED ON BY: in?
(defun any? (l) (reduce or l))

;; in? 
;; DEPENDENCIES: any?, eq?, x, lambda, map
;; DEPENDED ON BY: isdigit
(defun in? (sym list) (any? (map (lambda (x) (eq? x sym)) list)))

;; isdigit 
;; DEPENDENCIES: if, 0, and, cdr, in?, quote, car
;; DEPENDED ON BY: 
(defun isdigit (sym) (if sym (and (in? (car sym) '(0 1 2 3 4 5 6 7 8 9)) (isdigit (cdr sym))) 't))

;; remove 
;; DEPENDENCIES: eq?, cons, null?, cond, cdr, quote, car
;; DEPENDED ON BY: sort, defunv
(defun remove (item l) (cond ((null? l) ()) ((eq? (car l) item) (cdr l)) ('t (cons (car l) (remove item (cdr l))))))

;; defunv 
;; DEPENDENCIES: quasiquote, unquote, remove, lambda, splice, quote, defmacro
;; DEPENDED ON BY: 
(defmacro defunv (name params &rest body) `(defmacro ,name ,params `((lambda ,',(remove '&rest params) ,',@body) ',,@(remove '&rest params))))

;; sort 
;; DEPENDENCIES: if, minlist, pair?, remove, cons, let, themin
;; DEPENDED ON BY: sortatom
(defun sort (l) (if (pair? l) (let ((themin (minlist l))) (cons themin (sort (remove themin l)))) l))

;; sortatom 
;; DEPENDENCIES: coerce, sort
;; DEPENDED ON BY: 
(defun sortatom (a) (coerce (sort a)))

;; cdadr 
;; DEPENDENCIES: car, cdr
;; DEPENDED ON BY: 
(defun cdadr (l) (cdr (car (cdr l))))

;; fib 
;; DEPENDENCIES: fibrec_r, car
;; DEPENDED ON BY: 
(defun fib (n) (car (fibrec_r n)))

