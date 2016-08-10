;; -- 
;; DEPENDENCIES: -
;; DEPENDED ON BY: fibrec_r, *, /, fibiter
(defun -- (x) (- x 1))

;; printnewline 
;; DEPENDENCIES: printsym, quote
;; DEPENDED ON BY: print
(defun printnewline () (printsym '\n))

;; neg? 
;; DEPENDENCIES: <
;; DEPENDED ON BY: %, samesign
(defun neg? (x) (< x 0))

;; caadr 
;; DEPENDENCIES: cdr, car
;; DEPENDED ON BY: 
(defun caadr (l) (car (car (cdr l))))

;; list 
;; DEPENDENCIES: if, splice, quasiquote, cdr, car, cons, unquote
;; DEPENDED ON BY: progn, div
(defmacro list (&rest alist) (if alist `(cons ,(car alist) (list ,@(cdr alist)))))

;; printspace 
;; DEPENDENCIES: printsym, quote
;; DEPENDED ON BY: print, printlist
(defun printspace () (printsym '\s))

;; > 
;; DEPENDENCIES: <
;; DEPENDED ON BY: maxlist, %, eq?
(defun > (a b) (< b a))

;; cadr 
;; DEPENDENCIES: cdr, car
;; DEPENDED ON BY: maxlist, fibrec_r, minlist
(defun cadr (l) (car (cdr l)))

;; lambdachain 
;; DEPENDENCIES: if, lambda, cdr, quasiquote, splice, car, unquote
;; DEPENDED ON BY: 
(defmacro lambdachain (params vals &rest body) (if (cdr params) `((lambda (,(car params)) (lambdachain ,(cdr params) ,(cdr vals) ,body)) ,(car vals)) `((lambda (,(car params)) ,@body) ,(car vals))))

;; cdadr 
;; DEPENDENCIES: cdr, car
;; DEPENDED ON BY: 
(defun cdadr (l) (cdr (car (cdr l))))

;; apply 
;; DEPENDENCIES: splice, quasiquote, unquote
;; DEPENDED ON BY: 
(defmacro apply (fn &rest params) `(,fn ,@params))

;; cadar 
;; DEPENDENCIES: cdr, car
;; DEPENDED ON BY: letrec, cond, seconds
(defun cadar (l) (car (cdr (car l))))

;; last 
;; DEPENDENCIES: cdr, if, car
;; DEPENDED ON BY: progn
(defun last (l) (if (cdr l) (last (cdr l)) (car l)))

;; progn 
;; DEPENDENCIES: splice, quasiquote, last, list
;; DEPENDED ON BY: print, prompt, letrec, **, let, flet, while, do, printlist
(defmacro progn (&rest body) `(last (list ,@body)))

;; printlist 
;; DEPENDENCIES: if, printsym, printspace, progn, cdr, car
;; DEPENDED ON BY: 
(defun printlist (words) (if words (progn (printsym (car words)) (printspace) (printlist (cdr words)))))

;; while 
;; DEPENDENCIES: progn, if, splice, quasiquote, unquote
;; DEPENDED ON BY: 
(defmacro while (condition &rest body) `(if ,condition (progn ,@body (while ,condition ,@body))))

;; flet 
;; DEPENDENCIES: progn, if, splice, quasiquote, cdr, defun, car, unquote, lambda
;; DEPENDED ON BY: 
(defmacro flet (bindings &rest body) (if bindings `((lambda () (progn (defun ,@(car bindings)) (flet ,(cdr bindings) ,@body)))) `(progn ,@body)))

;; letrec 
;; DEPENDENCIES: if, progn, cdr, quasiquote, splice, cadar, unquote, lambda
;; DEPENDED ON BY: quicksort, do
(defmacro letrec (bindings &rest body) (if bindings `((lambda (,(caar bindings)) (letrec ,(cdr bindings) ,@body)) ,(cadar bindings)) `(progn ,@body)))

;; cdar 
;; DEPENDENCIES: cdr, car
;; DEPENDED ON BY: cdrs
(defun cdar (l) (cdr (car l)))

;; cdrs 
;; DEPENDENCIES: cdr, if, cdar, cons
;; DEPENDED ON BY: ziplist
(defun cdrs (l) (if l (cons (cdar l) (cdrs (cdr l)))))

;; seconds 
;; DEPENDENCIES: cdr, if, cadar, cons
;; DEPENDED ON BY: let, cadrs
(defun seconds (l) (if l (cons (cadar l) (seconds (cdr l)))))

;; cadrs 
;; DEPENDENCIES: seconds
;; DEPENDED ON BY: 
(defun cadrs (l) (seconds l))

;; let 
;; DEPENDENCIES: splice, quasiquote, lambda, seconds, progn
;; DEPENDED ON BY: max, fibiter, maxlist, sort, do, fibrec_r, minlist, foreach, min, fast_exp, readline
(defmacro let (bindings &rest body) `((lambda ,(firsts bindings) (progn ,@body)) ,@(seconds bindings)))

;; do 
;; DEPENDENCIES: progn, if, splice, quasiquote, let, result, letrec, unquote
;; DEPENDED ON BY: foreach, fibiter
(defmacro do (bindings test &rest body) `(letrec ,bindings (let ((result (progn ,@body))) (if ,test (do ,bindings ,test ,@body) result))))

;; allwhich 
;; DEPENDENCIES: cdr, if, cons, car
;; DEPENDED ON BY: quicksort
(defun allwhich (l fn) (if l (if (fn (car l)) (cons (car l) (allwhich (cdr l) fn)) (allwhich (cdr l) fn))))

;; caar 
;; DEPENDENCIES: car
;; DEPENDED ON BY: firsts, cond
(defun caar (l) (car (car l)))

;; map 
;; DEPENDENCIES: cdr, if, cons, car
;; DEPENDED ON BY: in?
(defun map (fn l) (if l (cons (fn (car l)) (map fn (cdr l)))))

;; null? 
;; DEPENDENCIES: if, quote
;; DEPENDED ON BY: and, not, eq?, remove, or
(defun null? (x) (if x () 't))

;; not 
;; DEPENDENCIES: null?
;; DEPENDED ON BY: exp, eq?, eqz?, *, quicksort, fast_exp
(defun not (x) (null? x))

;; + 
;; DEPENDENCIES: -
;; DEPENDED ON BY: fibiter, %, nievefib, ++, /, *, fibrec_r
(defun + (x y) (- x (- 0 y)))

;; ++ 
;; DEPENDENCIES: +
;; DEPENDED ON BY: /, fibiter
(defun ++ (x) (+ x 1))

;; % 
;; DEPENDENCIES: if, <, >, -, +, neg?
;; DEPENDED ON BY: even?, div
(defun % (x y) (if (neg? y) (if (> x y) (if (> x 0) (% (+ x y) y) x) (% (- x y) y)) (if (< x y) (if (< x 0) (% (+ x y) y) x) (% (- x y) y))))

;; abs 
;; DEPENDENCIES: if, -, <
;; DEPENDED ON BY: /
(defun abs (x) (if (< x 0) (- 0 x) x))

;; foreach 
;; DEPENDENCIES: cdr, quasiquote, let, splice, car, unquote, do
;; DEPENDED ON BY: 
(defmacro foreach (itemname alist bindings &rest body) `(let ((alist ,alist)) (do ((,itemname (car alist)) (alist (cdr alist)) ,@bindings) alist ,@body)))

;; print 
;; DEPENDENCIES: progn, printsym, printspace, if, splice, quasiquote, quote, cdr, car, unquote, printnewline
;; DEPENDED ON BY: write, println
(defmacro print (newline? &rest params) `(if ',params (progn (printsym ,(car params)) (printspace) (print ,newline? ,@(cdr params))) (if ,newline? (printnewline))))

;; println 
;; DEPENDENCIES: splice, quasiquote, print, quote
;; DEPENDED ON BY: **
(defmacro println (&rest params) `(print 't ,@params))

;; write 
;; DEPENDENCIES: splice, quasiquote, print
;; DEPENDED ON BY: prompt
(defmacro write (&rest params) `(print () ,@params))

;; cddr 
;; DEPENDENCIES: cdr
;; DEPENDED ON BY: maxlist, minlist
(defun cddr (l) (cdr (cdr l)))

;; firsts 
;; DEPENDENCIES: cdr, if, caar, cons
;; DEPENDED ON BY: cars
(defun firsts (l) (if l (cons (caar l) (firsts (cdr l)))))

;; cars 
;; DEPENDENCIES: firsts
;; DEPENDED ON BY: ziplist
(defun cars (l) (firsts l))

;; caddr 
;; DEPENDENCIES: cdr, car
;; DEPENDED ON BY: 
(defun caddr (l) (car (cdr (cdr l))))

;; pair? 
;; DEPENDENCIES: cdr
;; DEPENDED ON BY: maxlist, sort, min, max, reverse, reduce, minlist, reversestr, quicksort, coerce
(defun pair? (x) (cdr x))

;; coerce 
;; DEPENDENCIES: if, cdr, pair?, car, cons
;; DEPENDED ON BY: sortatom, readline, quicksortatom
(defun coerce (l) (if (pair? l) (cons (car l) (coerce (cdr l))) (car l)))

;; minlist 
;; DEPENDENCIES: if, <, cddr, cdr, let, pair?, this, car, cadr, cons
;; DEPENDED ON BY: sort
(defun minlist (l) (if (pair? l) (let ((this (car l))) (if (< this (cadr l)) (minlist (cons this (cddr l))) (minlist (cdr l)))) (car l)))

;; reduce 
;; DEPENDENCIES: cdr, if, car, pair?
;; DEPENDED ON BY: all?, any?
(defun reduce (fn l) (if (pair? l) (fn (car l) (reduce fn (cdr l))) (car l)))

;; any? 
;; DEPENDENCIES: reduce
;; DEPENDED ON BY: in?
(defun any? (l) (reduce or l))

;; all? 
;; DEPENDENCIES: reduce
;; DEPENDED ON BY: ziplist
(defun all? (l) (reduce and l))

;; ziplist 
;; DEPENDENCIES: if, all?, cdrs, cars, cons
;; DEPENDED ON BY: 
(defun ziplist (ls) (if (all? (cars ls)) (cons (cars ls) (ziplist (cdrs ls)))))

;; max 
;; DEPENDENCIES: if, <, splice, quasiquote, let, car, nextmax, pair?, cdr, quote, unquote
;; DEPENDED ON BY: 
(defmacro max (&rest l) `(if ',(pair? l) (let ((nextmax (max ,@(cdr l)))) (if (< ,(car l) nextmax) nextmax ,(car l))) ,(car l)))

;; min 
;; DEPENDENCIES: if, <, splice, quasiquote, let, quote, pair?, cdr, car, nextmin, unquote
;; DEPENDED ON BY: 
(defmacro min (&rest l) `(if ',(pair? l) (let ((nextmin (min ,@(cdr l)))) (if (< ,(car l) nextmin) nextmin ,(car l))) ,(car l)))

;; pos? 
;; DEPENDENCIES: <
;; DEPENDED ON BY: samesign
(defun pos? (x) (< 0 x))

;; cond 
;; DEPENDENCIES: if, caar, splice, quasiquote, cdr, cadar, car, unquote
;; DEPENDED ON BY: and, eq?, remove, or
(defmacro cond (&rest options) (if (car options) `(if ,(caar options) ,(cadar options) (cond ,@(cdr options)))))

;; or 
;; DEPENDENCIES: if, cdr, quasiquote, cond, car, splice, quote, null?, unquote
;; DEPENDED ON BY: eqz?, nievefib, eq?, /, samesign
(defmacro or (&rest vals) `(cond ((null? ',(cdr vals)) (if ,(car vals) 't)) (,(car vals) 't) ('t (or ,@(cdr vals)))))

;; eqz? 
;; DEPENDENCIES: or, <, not
;; DEPENDED ON BY: exp, fast_exp, *
(defun eqz? (a) (not (or (< a 0) (< 0 a))))

;; * 
;; DEPENDENCIES: if, <, eqz?, not, -, +, --
;; DEPENDED ON BY: exp, fast_exp, sq
(defun * (x y) (if (< x y) (* y x) (if (not (eqz? y)) (if (< 0 y) (+ x (* x (-- y))) (- (* x (+ y 1)) x)) 0)))

;; sq 
;; DEPENDENCIES: *
;; DEPENDED ON BY: 
(defun sq (x) (* x x))

;; exp 
;; DEPENDENCIES: if, eqz?, not, -, *
;; DEPENDED ON BY: **
(defun exp (x y) (if (not (eqz? y)) (* x (exp x (- y 1))) 1))

;; ** 
;; DEPENDENCIES: if, <, println, progn, exp, quote
;; DEPENDED ON BY: 
(defun ** (x y) (if (< y 0) (progn (println 'Exponent 'must 'not 'be 'less 'than 'zero) -1) (exp x y)))

;; and 
;; DEPENDENCIES: if, cdr, quasiquote, cond, car, splice, unquote, quote, null?
;; DEPENDED ON BY: isdigit, samesign, eq?
(defmacro and (&rest vals) `(cond ((null? ',(cdr vals)) (if ,(car vals) 't)) (,(car vals) (and ,@(cdr vals))) ('t ())))

;; eq? 
;; DEPENDENCIES: >, or, not, cdr, cond, quote, <, car, null?, atom?, and
;; DEPENDED ON BY: replaceall, nievefib, remove, fibrec_r, even?, in?, nth, newline?, /
(defun eq? (a b) (cond ((and (atom? a) (atom? b)) (not (or (< a b) (> a b)))) ((and (null? a) (null? b)) 't) ((or (atom? a) (atom? b)) ()) ('t (and (eq? (car a) (car b)) (eq? (cdr a) (cdr b))))))

;; newline? 
;; DEPENDENCIES: eq?, quote
;; DEPENDED ON BY: readline
(defun newline? (char) (eq? char '\n))

;; readline 
;; DEPENDENCIES: if, nextchar, let, newline?, coerce, inputchar, cons
;; DEPENDED ON BY: prompt
(defun readline () (let ((nextchar (inputchar))) (if (newline? nextchar) () (coerce (cons nextchar (readline))))))

;; prompt 
;; DEPENDENCIES: progn, readline, write
;; DEPENDED ON BY: 
(defun prompt (prompt) (progn (write prompt) (readline)))

;; nth 
;; DEPENDENCIES: if, cdr, -, car, eq?
;; DEPENDED ON BY: 
(defun nth (l n) (if (eq? n 0) (car l) (nth (cdr l) (- n 1))))

;; in? 
;; DEPENDENCIES: eq?, lambda, any?, map
;; DEPENDED ON BY: isdigit
(defun in? (sym list) (any? (map (lambda (x) (eq? x sym)) list)))

;; even? 
;; DEPENDENCIES: eq?, %
;; DEPENDED ON BY: fast_exp
(defun even? (x) (eq? (% x 2) 0))

;; fibrec_r 
;; DEPENDENCIES: if, let, quote, discovered, +, 1, --, car, eq?, cadr, cons
;; DEPENDED ON BY: fib, fibrec
(defun fibrec_r (n) (if (eq? n 1) '(1 1) (let ((discovered (fibrec_r (-- n)))) (cons (+ (car discovered) (cadr discovered)) discovered))))

;; fib 
;; DEPENDENCIES: fibrec_r, car
;; DEPENDED ON BY: 
(defun fib (n) (car (fibrec_r n)))

;; remove 
;; DEPENDENCIES: cdr, cond, quote, car, eq?, null?, cons
;; DEPENDED ON BY: defunv, sort
(defun remove (item l) (cond ((null? l) ()) ((eq? (car l) item) (cdr l)) ('t (cons (car l) (remove item (cdr l))))))

;; sort 
;; DEPENDENCIES: if, themin, let, remove, pair?, minlist, cons
;; DEPENDED ON BY: sortatom
(defun sort (l) (if (pair? l) (let ((themin (minlist l))) (cons themin (sort (remove themin l)))) l))

;; sortatom 
;; DEPENDENCIES: sort, coerce
;; DEPENDED ON BY: 
(defun sortatom (a) (coerce (sort a)))

;; defunv 
;; DEPENDENCIES: lambda, splice, quasiquote, remove, defmacro, quote
;; DEPENDED ON BY: 
(defmacro defunv (name params &rest body) `(defmacro ,name ,params `((lambda ,',(remove '&rest params) ,',@body) ',,@(remove '&rest params))))

;; nievefib 
;; DEPENDENCIES: if, or, -, +, eq?
;; DEPENDED ON BY: 
(defun nievefib (n) (if (or (eq? n 0) (eq? n 1)) 1 (+ (nievefib (- n 1)) (nievefib (- n 2)))))

;; replaceall 
;; DEPENDENCIES: if, cdr, atom?, car, eq?, cons
;; DEPENDED ON BY: 
(defun replaceall (a b l) (if l (if (atom? l) (if (eq? l a) b l) (cons (replaceall a b (car l)) (replaceall a b (cdr l))))))

;; samesign 
;; DEPENDENCIES: or, and, neg?, pos?
;; DEPENDED ON BY: /
(defun samesign (x y) (or (and (pos? y) (pos? x)) (and (neg? y) (neg? x))))

;; / 
;; DEPENDENCIES: if, <, ++, abs, -, or, +, --, eq?, samesign
;; DEPENDED ON BY: fast_exp, div
(defun / (x y) (if (< (- (abs x) (abs y)) 0) (if (or (eq? x 0) (samesign x y)) 0 -1) (if (samesign x y) (++ (/ (- x y) y)) (-- (/ (+ x y) y)))))

;; div 
;; DEPENDENCIES: /, list, %
;; DEPENDED ON BY: 
(defun div (x y) (list (/ x y) (% x y)))

;; fast_exp 
;; DEPENDENCIES: if, eqz?, not, even?, let, /, half_exp, *
;; DEPENDED ON BY: 
(defun fast_exp (x y) (if (not (eqz? y)) (let ((half_exp (fast_exp x (/ y 2)))) (if (even? y) (* half_exp half_exp) (* half_exp (* half_exp x)))) 1))

;; maxlist 
;; DEPENDENCIES: if, >, cdr, let, pair?, cddr, this, car, cadr, cons
;; DEPENDED ON BY: 
(defun maxlist (l) (if (pair? l) (let ((this (car l))) (if (> this (cadr l)) (maxlist (cons this (cddr l))) (maxlist (cdr l)))) (car l)))

;; isdigit 
;; DEPENDENCIES: if, cdr, quote, 0, car, in?, and
;; DEPENDED ON BY: 
(defun isdigit (sym) (if sym (and (in? (car sym) '(0 1 2 3 4 5 6 7 8 9)) (isdigit (cdr sym))) 't))

;; append 
;; DEPENDENCIES: if, cdr, cons, car
;; DEPENDED ON BY: quicksort, reversestr, reverse, flatten, inputnchars
(defun append (lista listb) (if lista (cons (car lista) (append (cdr lista) listb)) listb))

;; inputnchars 
;; DEPENDENCIES: append, if, -, inputchar
;; DEPENDED ON BY: 
(defun inputnchars (n) (if (- n 1) (append (inputnchars (- n 1)) (inputchar)) (inputchar)))

;; flatten 
;; DEPENDENCIES: append, if, cdr, cons, car, atom?
;; DEPENDED ON BY: 
(defun flatten (l) (if l (if (atom? (car l)) (cons (car l) (flatten (cdr l))) (append (flatten (car l)) (flatten (cdr l))))))

;; reverse 
;; DEPENDENCIES: append, if, cdr, pair?, car, cons
;; DEPENDED ON BY: reversestr, fibiter, fibrec
(defun reverse (l) (if (pair? l) (append (reverse (cdr l)) (cons (car l) ())) l))

;; fibrec 
;; DEPENDENCIES: fibrec_r, reverse
;; DEPENDED ON BY: 
(defun fibrec (n) (reverse (fibrec_r n)))

;; fibiter 
;; DEPENDENCIES: --, let, quote, f1, nums, f2, cons, do, <, x, 1, f3, +, ++, reverse
;; DEPENDED ON BY: 
(defun fibiter (n) (reverse (let ((x 0) (f1 0) (f2 1) (nums '(1)) (n (-- n))) (do ((x (++ x)) (f3 (+ f1 f2)) (nums (cons f3 nums)) (f1 f2) (f2 f3)) (< x (++ n)) nums))))

;; reversestr 
;; DEPENDENCIES: append, if, cdr, pair?, car, reverse
;; DEPENDED ON BY: 
(defun reversestr (l) (if (pair? l) (append (reverse (cdr l)) (car l)) l))

;; quicksort 
;; DEPENDENCIES: append, if, <, not, lambda, cdr, allwhich, pivot, pair?, secondlist, car, firstlist, letrec, cons
;; DEPENDED ON BY: quicksortatom
(defun quicksort (l) (if (pair? l) (letrec ((pivot (car l)) (firstlist (quicksort (allwhich (cdr l) (lambda (x) (< x pivot))))) (secondlist (quicksort (allwhich (cdr l) (lambda (x) (not (< x pivot))))))) (append firstlist (cons pivot secondlist))) l))

;; quicksortatom 
;; DEPENDENCIES: quicksort, coerce
;; DEPENDED ON BY: 
(defun quicksortatom (a) (coerce (quicksort a)))

