;; neg? 
;; DEPENDENCIES: <
;; DEPENDED ON BY: samesign, %
(defun neg? (x) (< x 0))

;; caadr 
;; DEPENDENCIES: car, cdr
;; DEPENDED ON BY: 
(defun caadr (l) (car (car (cdr l))))

;; pos? 
;; DEPENDENCIES: <
;; DEPENDED ON BY: samesign
(defun pos? (x) (< 0 x))

;; id 
;; DEPENDENCIES: 
;; DEPENDED ON BY: 
(defun id (x) x)

;; printnewline 
;; DEPENDENCIES: printsym, quote
;; DEPENDED ON BY: print
(defun printnewline () (printsym '\n))

;; caddr 
;; DEPENDENCIES: car, cdr
;; DEPENDED ON BY: 
(defun caddr (l) (car (cdr (cdr l))))

;; + 
;; DEPENDENCIES: -
;; DEPENDED ON BY: fibrec_r, ++, *, nievefib, %, fibiter, /
(defun + (x y) (- x (- 0 y)))

;; printspace 
;; DEPENDENCIES: printsym, quote
;; DEPENDED ON BY: printlist, print
(defun printspace () (printsym '\s))

;; cadar 
;; DEPENDENCIES: car, cdr
;; DEPENDED ON BY: cond, seconds, letrec
(defun cadar (l) (car (cdr (car l))))

;; seconds 
;; DEPENDENCIES: if, cdr, cadar, cons
;; DEPENDED ON BY: cadrs, let
(defun seconds (l) (if l (cons (cadar l) (seconds (cdr l)))))

;; allwhich 
;; DEPENDENCIES: fn, car, if, cdr, cons
;; DEPENDED ON BY: quicksort
(defun allwhich (l fn) (if l (if (fn (car l)) (cons (car l) (allwhich (cdr l) fn)) (allwhich (cdr l) fn))))

;; -- 
;; DEPENDENCIES: -
;; DEPENDED ON BY: fibrec_r, fibiter, /, *
(defun -- (x) (- x 1))

;; list 
;; DEPENDENCIES: cdr, cons, unquote, car, if, quasiquote, splice
;; DEPENDED ON BY: div, progn
(defmacro list (&rest alist) (if alist `(cons ,(car alist) (list ,@(cdr alist)))))

;; cddr 
;; DEPENDENCIES: cdr
;; DEPENDED ON BY: minlist, maxlist
(defun cddr (l) (cdr (cdr l)))

;; apply 
;; DEPENDENCIES: unquote, splice, quasiquote
;; DEPENDED ON BY: 
(defmacro apply (fn &rest params) `(,fn ,@params))

;; last 
;; DEPENDENCIES: if, cdr, car
;; DEPENDED ON BY: progn
(defun last (l) (if (cdr l) (last (cdr l)) (car l)))

;; ++ 
;; DEPENDENCIES: +
;; DEPENDED ON BY: fibiter, /
(defun ++ (x) (+ x 1))

;; append 
;; DEPENDENCIES: car, if, cdr, cons
;; DEPENDED ON BY: reversestr, inputnchars, flatten, quicksort, reverse
(defun append (lista listb) (if lista (cons (car lista) (append (cdr lista) listb)) listb))

;; flatten 
;; DEPENDENCIES: append, cdr, cons, atom?, car, if
;; DEPENDED ON BY: 
(defun flatten (l) (if l (if (atom? (car l)) (cons (car l) (flatten (cdr l))) (append (flatten (car l)) (flatten (cdr l))))))

;; inputnchars 
;; DEPENDENCIES: -, append, inputchar, if
;; DEPENDED ON BY: 
(defun inputnchars (n) (if (- n 1) (append (inputnchars (- n 1)) (inputchar)) (inputchar)))

;; caar 
;; DEPENDENCIES: car
;; DEPENDED ON BY: cond, firsts, letrec
(defun caar (l) (car (car l)))

;; firsts 
;; DEPENDENCIES: caar, if, cdr, cons
;; DEPENDED ON BY: let, cars
(defun firsts (l) (if l (cons (caar l) (firsts (cdr l)))))

;; cars 
;; DEPENDENCIES: firsts
;; DEPENDED ON BY: ziplist
(defun cars (l) (firsts l))

;; cond 
;; DEPENDENCIES: cdr, caar, cadar, unquote, car, if, splice, quasiquote
;; DEPENDED ON BY: remove, and, or, eq?
(defmacro cond (&rest options) (if (car options) `(if ,(caar options) ,(cadar options) (cond ,@(cdr options)))))

;; null? 
;; DEPENDENCIES: if, quote
;; DEPENDED ON BY: not, remove, and, or, eq?
(defun null? (x) (if x () 't))

;; or 
;; DEPENDENCIES: null?, cdr, cond, quote, unquote, car, quasiquote, if, splice
;; DEPENDED ON BY: samesign, eqz?, nievefib, /, eq?
(defmacro or (&rest vals) `(cond ((null? ',(cdr vals)) (if ,(car vals) 't)) (,(car vals) 't) ('t (or ,@(cdr vals)))))

;; cadrs 
;; DEPENDENCIES: seconds
;; DEPENDED ON BY: 
(defun cadrs (l) (seconds l))

;; not 
;; DEPENDENCIES: null?
;; DEPENDED ON BY: *, eqz?, exp, fast_exp, quicksort, eq?
(defun not (x) (null? x))

;; cdar 
;; DEPENDENCIES: car, cdr
;; DEPENDED ON BY: cdrs
(defun cdar (l) (cdr (car l)))

;; cdrs 
;; DEPENDENCIES: cdar, if, cdr, cons
;; DEPENDED ON BY: ziplist
(defun cdrs (l) (if l (cons (cdar l) (cdrs (cdr l)))))

;; lambdachain 
;; DEPENDENCIES: cdr, unquote, car, if, quasiquote, splice, lambda
;; DEPENDED ON BY: 
(defmacro lambdachain (params vals &rest body) (if (cdr params) `((lambda (,(car params)) (lambdachain ,(cdr params) ,(cdr vals) ,body)) ,(car vals)) `((lambda (,(car params)) ,@body) ,(car vals))))

;; map 
;; DEPENDENCIES: car, if, cdr, cons, fn
;; DEPENDED ON BY: in?
(defun map (fn l) (if l (cons (fn (car l)) (map fn (cdr l)))))

;; pair? 
;; DEPENDENCIES: cdr
;; DEPENDED ON BY: reverse, sort, coerce, reduce, min, minlist, quicksort, max, maxlist, reversestr
(defun pair? (x) (cdr x))

;; reduce 
;; DEPENDENCIES: pair?, car, if, cdr, fn
;; DEPENDED ON BY: any?, all?
(defun reduce (fn l) (if (pair? l) (fn (car l) (reduce fn (cdr l))) (car l)))

;; all? 
;; DEPENDENCIES: reduce
;; DEPENDED ON BY: ziplist
(defun all? (l) (reduce and l))

;; ziplist 
;; DEPENDENCIES: if, all?, cons, cars, cdrs
;; DEPENDED ON BY: 
(defun ziplist (ls) (if (all? (cars ls)) (cons (cars ls) (ziplist (cdrs ls)))))

;; coerce 
;; DEPENDENCIES: pair?, car, if, cdr, cons
;; DEPENDED ON BY: quicksortatom, sortatom, readline
(defun coerce (l) (if (pair? l) (cons (car l) (coerce (cdr l))) (car l)))

;; reverse 
;; DEPENDENCIES: pair?, append, car, if, cdr, cons
;; DEPENDED ON BY: fibrec, fibiter, reversestr
(defun reverse (l) (if (pair? l) (append (reverse (cdr l)) (cons (car l) ())) l))

;; reversestr 
;; DEPENDENCIES: reverse, pair?, append, car, if, cdr
;; DEPENDED ON BY: 
(defun reversestr (l) (if (pair? l) (append (reverse (cdr l)) (car l)) l))

;; cadr 
;; DEPENDENCIES: car, cdr
;; DEPENDED ON BY: minlist, fibrec_r, maxlist
(defun cadr (l) (car (cdr l)))

;; progn 
;; DEPENDENCIES: last, quasiquote, splice, list
;; DEPENDED ON BY: prompt, flet, do, letrec, print, **, let, while, printlist
(defmacro progn (&rest body) `(last (list ,@body)))

;; printlist 
;; DEPENDENCIES: progn, printspace, printsym, if, car, cdr
;; DEPENDED ON BY: 
(defun printlist (words) (if words (progn (printsym (car words)) (printspace) (printlist (cdr words)))))

;; while 
;; DEPENDENCIES: progn, unquote, splice, quasiquote, if
;; DEPENDED ON BY: 
(defmacro while (condition &rest body) `(if ,condition (progn ,@body (while ,condition ,@body))))

;; let 
;; DEPENDENCIES: progn, unquote, splice, quasiquote, lambda, firsts, seconds
;; DEPENDED ON BY: fibrec_r, foreach, sort, do, min, fast_exp, minlist, fibiter, max, maxlist, readline
(defmacro let (bindings &rest body) `((lambda ,(firsts bindings) (progn ,@body)) ,@(seconds bindings)))

;; max 
;; DEPENDENCIES: cdr, let, quote, nextmax, pair?, unquote, splice, quasiquote, if, car, <
;; DEPENDED ON BY: 
(defmacro max (&rest l) `(if ',(pair? l) (let ((nextmax (max ,@(cdr l)))) (if (< ,(car l) nextmax) nextmax ,(car l))) ,(car l)))

;; minlist 
;; DEPENDENCIES: cdr, cons, cddr, let, pair?, this, if, car, cadr, <
;; DEPENDED ON BY: sort
(defun minlist (l) (if (pair? l) (let ((this (car l))) (if (< this (cadr l)) (minlist (cons this (cddr l))) (minlist (cdr l)))) (car l)))

;; min 
;; DEPENDENCIES: cdr, let, quote, pair?, unquote, splice, quasiquote, if, car, nextmin, <
;; DEPENDED ON BY: 
(defmacro min (&rest l) `(if ',(pair? l) (let ((nextmin (min ,@(cdr l)))) (if (< ,(car l) nextmin) nextmin ,(car l))) ,(car l)))

;; print 
;; DEPENDENCIES: progn, printsym, cdr, quote, printspace, unquote, car, quasiquote, if, splice, printnewline
;; DEPENDED ON BY: write, println
(defmacro print (newline? &rest params) `(if ',params (progn (printsym ,(car params)) (printspace) (print ,newline? ,@(cdr params))) (if ,newline? (printnewline))))

;; println 
;; DEPENDENCIES: print, splice, quasiquote, quote
;; DEPENDED ON BY: **
(defmacro println (&rest params) `(print 't ,@params))

;; write 
;; DEPENDENCIES: print, splice, quasiquote
;; DEPENDED ON BY: prompt
(defmacro write (&rest params) `(print () ,@params))

;; letrec 
;; DEPENDENCIES: progn, cdr, caar, cadar, unquote, splice, if, quasiquote, lambda
;; DEPENDED ON BY: quicksort, do
(defmacro letrec (bindings &rest body) (if bindings `((lambda (,(caar bindings)) (letrec ,(cdr bindings) ,@body)) ,(cadar bindings)) `(progn ,@body)))

;; quicksort 
;; DEPENDENCIES: allwhich, append, cons, secondlist, firstlist, car, if, pivot, not, cdr, x, letrec, pair?, lambda, <
;; DEPENDED ON BY: quicksortatom
(defun quicksort (l) (if (pair? l) (letrec ((pivot (car l)) (firstlist (quicksort (allwhich (cdr l) (lambda (x) (< x pivot))))) (secondlist (quicksort (allwhich (cdr l) (lambda (x) (not (< x pivot))))))) (append firstlist (cons pivot secondlist))) l))

;; quicksortatom 
;; DEPENDENCIES: coerce, quicksort
;; DEPENDED ON BY: 
(defun quicksortatom (a) (coerce (quicksort a)))

;; do 
;; DEPENDENCIES: progn, letrec, let, unquote, splice, quasiquote, if, result
;; DEPENDED ON BY: fibiter, foreach
(defmacro do (bindings test &rest body) `(letrec ,bindings (let ((result (progn ,@body))) (if ,test (do ,bindings ,test ,@body) result))))

;; foreach 
;; DEPENDENCIES: do, cdr, alist, let, unquote, car, quasiquote, splice
;; DEPENDED ON BY: 
(defmacro foreach (itemname alist bindings &rest body) `(let ((alist ,alist)) (do ((,itemname (car alist)) (alist (cdr alist)) ,@bindings) alist ,@body)))

;; fibiter 
;; DEPENDENCIES: reverse, <, --, nums, cons, f1, let, quote, 1, +, ++, n, do, x, f2, f3
;; DEPENDED ON BY: 
(defun fibiter (n) (reverse (let ((x 0) (f1 0) (f2 1) (nums '(1)) (n (-- n))) (do ((x (++ x)) (f3 (+ f1 f2)) (nums (cons f3 nums)) (f1 f2) (f2 f3)) (< x (++ n)) nums))))

;; flet 
;; DEPENDENCIES: progn, cdr, defun, unquote, splice, if, quasiquote, car, lambda
;; DEPENDED ON BY: 
(defmacro flet (bindings &rest body) (if bindings `((lambda () (progn (defun ,@(car bindings)) (flet ,(cdr bindings) ,@body)))) `(progn ,@body)))

;; abs 
;; DEPENDENCIES: -, if, <
;; DEPENDED ON BY: /
(defun abs (x) (if (< x 0) (- 0 x) x))

;; any? 
;; DEPENDENCIES: reduce
;; DEPENDED ON BY: in?
(defun any? (l) (reduce or l))

;; > 
;; DEPENDENCIES: <
;; DEPENDED ON BY: maxlist, eq?, %
(defun > (a b) (< b a))

;; % 
;; DEPENDENCIES: >, neg?, if, +, -, <
;; DEPENDED ON BY: div, even?
(defun % (x y) (if (neg? y) (if (> x y) (if (> x 0) (% (+ x y) y) x) (% (- x y) y)) (if (< x y) (if (< x 0) (% (+ x y) y) x) (% (- x y) y))))

;; maxlist 
;; DEPENDENCIES: cdr, cons, >, cddr, let, pair?, this, if, car, cadr
;; DEPENDED ON BY: 
(defun maxlist (l) (if (pair? l) (let ((this (car l))) (if (> this (cadr l)) (maxlist (cons this (cddr l))) (maxlist (cdr l)))) (car l)))

;; cdadr 
;; DEPENDENCIES: car, cdr
;; DEPENDED ON BY: 
(defun cdadr (l) (cdr (car (cdr l))))

;; eqz? 
;; DEPENDENCIES: <, or, not
;; DEPENDED ON BY: exp, fast_exp, *
(defun eqz? (a) (not (or (< a 0) (< 0 a))))

;; * 
;; DEPENDENCIES: --, eqz?, if, not, +, -, <
;; DEPENDED ON BY: double, sq, exp, fast_exp
(defun * (x y) (if (< x y) (* y x) (if (not (eqz? y)) (if (< 0 y) (+ x (* x (-- y))) (- (* x (+ y 1)) x)) 0)))

;; sq 
;; DEPENDENCIES: *
;; DEPENDED ON BY: 
(defun sq (x) (* x x))

;; double 
;; DEPENDENCIES: *
;; DEPENDED ON BY: 
(defun double (x) (* 2 x))

;; exp 
;; DEPENDENCIES: *, if, not, -, eqz?
;; DEPENDED ON BY: **
(defun exp (x y) (if (not (eqz? y)) (* x (exp x (- y 1))) 1))

;; ** 
;; DEPENDENCIES: progn, if, exp, println, quote, <
;; DEPENDED ON BY: 
(defun ** (x y) (if (< y 0) (progn (println 'Exponent 'must 'not 'be 'less 'than 'zero) -1) (exp x y)))

;; and 
;; DEPENDENCIES: null?, cdr, cond, quote, unquote, car, quasiquote, if, splice
;; DEPENDED ON BY: samesign, isdigit, eq?
(defmacro and (&rest vals) `(cond ((null? ',(cdr vals)) (if ,(car vals) 't)) (,(car vals) (and ,@(cdr vals))) ('t ())))

;; eq? 
;; DEPENDENCIES: null?, or, and, cdr, >, cond, quote, atom?, car, not, <
;; DEPENDED ON BY: fibrec_r, nievefib, in?, newline?, even?, replaceall, remove, /, nth
(defun eq? (a b) (cond ((and (atom? a) (atom? b)) (not (or (< a b) (> a b)))) ((and (null? a) (null? b)) 't) ((or (atom? a) (atom? b)) ()) ('t (and (eq? (car a) (car b)) (eq? (cdr a) (cdr b))))))

;; nth 
;; DEPENDENCIES: eq?, car, if, cdr, -
;; DEPENDED ON BY: 
(defun nth (l n) (if (eq? n 0) (car l) (nth (cdr l) (- n 1))))

;; remove 
;; DEPENDENCIES: eq?, null?, cdr, cons, cond, quote, car
;; DEPENDED ON BY: sort, defunv
(defun remove (item l) (cond ((null? l) ()) ((eq? (car l) item) (cdr l)) ('t (cons (car l) (remove item (cdr l))))))

;; defunv 
;; DEPENDENCIES: quote, remove, defmacro, unquote, splice, quasiquote, lambda
;; DEPENDED ON BY: 
(defmacro defunv (name params &rest body) `(defmacro ,name ,params `((lambda ,',(remove '&rest params) ,',@body) ',,@(remove '&rest params))))

;; sort 
;; DEPENDENCIES: cons, let, remove, themin, pair?, minlist, if
;; DEPENDED ON BY: sortatom
(defun sort (l) (if (pair? l) (let ((themin (minlist l))) (cons themin (sort (remove themin l)))) l))

;; sortatom 
;; DEPENDENCIES: coerce, sort
;; DEPENDED ON BY: 
(defun sortatom (a) (coerce (sort a)))

;; replaceall 
;; DEPENDENCIES: eq?, cdr, cons, atom?, car, if
;; DEPENDED ON BY: 
(defun replaceall (a b l) (if l (if (atom? l) (if (eq? l a) b l) (cons (replaceall a b (car l)) (replaceall a b (cdr l))))))

;; even? 
;; DEPENDENCIES: eq?, %
;; DEPENDED ON BY: fast_exp
(defun even? (x) (eq? (% x 2) 0))

;; newline? 
;; DEPENDENCIES: eq?, quote
;; DEPENDED ON BY: readline
(defun newline? (char) (eq? char '\n))

;; readline 
;; DEPENDENCIES: cons, let, inputchar, if, nextchar, newline?, coerce
;; DEPENDED ON BY: prompt
(defun readline () (let ((nextchar (inputchar))) (if (newline? nextchar) () (coerce (cons nextchar (readline))))))

;; prompt 
;; DEPENDENCIES: progn, write, readline
;; DEPENDED ON BY: 
(defun prompt (prompt) (progn (write prompt) (readline)))

;; in? 
;; DEPENDENCIES: eq?, x, lambda, any?, map
;; DEPENDED ON BY: isdigit
(defun in? (sym list) (any? (map (lambda (x) (eq? x sym)) list)))

;; nievefib 
;; DEPENDENCIES: eq?, or, if, +, -
;; DEPENDED ON BY: 
(defun nievefib (n) (if (or (eq? n 0) (eq? n 1)) 1 (+ (nievefib (- n 1)) (nievefib (- n 2)))))

;; fibrec_r 
;; DEPENDENCIES: eq?, --, cons, let, quote, 1, car, if, +, cadr, discovered
;; DEPENDED ON BY: fibrec, fib
(defun fibrec_r (n) (if (eq? n 1) '(1 1) (let ((discovered (fibrec_r (-- n)))) (cons (+ (car discovered) (cadr discovered)) discovered))))

;; fib 
;; DEPENDENCIES: fibrec_r, car
;; DEPENDED ON BY: 
(defun fib (n) (car (fibrec_r n)))

;; fibrec 
;; DEPENDENCIES: reverse, fibrec_r
;; DEPENDED ON BY: 
(defun fibrec (n) (reverse (fibrec_r n)))

;; isdigit 
;; DEPENDENCIES: and, cdr, 0, quote, in?, car, if
;; DEPENDED ON BY: 
(defun isdigit (sym) (if sym (and (in? (car sym) '(0 1 2 3 4 5 6 7 8 9)) (isdigit (cdr sym))) 't))

;; samesign 
;; DEPENDENCIES: pos?, or, neg?, and
;; DEPENDED ON BY: /
(defun samesign (x y) (or (and (pos? y) (pos? x)) (and (neg? y) (neg? x))))

;; / 
;; DEPENDENCIES: eq?, or, --, abs, samesign, if, +, ++, -, <
;; DEPENDED ON BY: div, fast_exp
(defun / (x y) (if (< (- (abs x) (abs y)) 0) (if (or (eq? x 0) (samesign x y)) 0 -1) (if (samesign x y) (++ (/ (- x y) y)) (-- (/ (+ x y) y)))))

;; fast_exp 
;; DEPENDENCIES: *, eqz?, /, let, half_exp, if, not, even?
;; DEPENDED ON BY: 
(defun fast_exp (x y) (if (not (eqz? y)) (let ((half_exp (fast_exp x (/ y 2)))) (if (even? y) (* half_exp half_exp) (* half_exp (* half_exp x)))) 1))

;; div 
;; DEPENDENCIES: /, list, %
;; DEPENDED ON BY: 
(defun div (x y) (list (/ x y) (% x y)))

