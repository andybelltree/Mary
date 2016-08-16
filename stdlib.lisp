;; cdadr 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: NONE
(defun cdadr (l) (cdr (car (cdr l))))

;; lambdachain 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: NONE
(defmacro lambdachain (params vals &rest body) (if (cdr params) `((lambda (,(car params)) (lambdachain ,(cdr params) ,(cdr vals) ,body)) ,(car vals)) `((lambda (,(car params)) ,@body) ,(car vals))))

;; printspace 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: printlist, print
(defun printspace () (printsym '\s))

;; caddr 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: NONE
(defun caddr (l) (car (cdr (cdr l))))

;; pos? 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: samesign
(defun pos? (x) (< 0 x))

;; cadr 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: minlist, fibrec_r, maxlist
(defun cadr (l) (car (cdr l)))

;; neg? 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: samesign, %
(defun neg? (x) (< x 0))

;; caadr 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: NONE
(defun caadr (l) (car (car (cdr l))))

;; pair? 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: sort, min, max, coerce, reversestr, reverse, reduce, maxlist, minlist, quicksort
(defun pair? (x) (cdr x))

;; reduce 
;; DEPENDENCIES: pair?
;; DEPENDED ON BY: all?, any?
(defun reduce (fn l) (if (pair? l) (fn (car l) (reduce fn (cdr l))) (car l)))

;; all? 
;; DEPENDENCIES: reduce
;; DEPENDED ON BY: ziplist
(defun all? (l) (reduce and l))

;; cddr 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: minlist, maxlist
(defun cddr (l) (cdr (cdr l)))

;; map 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: in?
(defun map (fn l) (if l (cons (fn (car l)) (map fn (cdr l)))))

;; caar 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: firsts, letrec, cond
(defun caar (l) (car (car l)))

;; printnewline 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: print
(defun printnewline () (printsym '\n))

;; -- 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: *, /, fibiter, fibrec_r
(defun -- (x) (- x 1))

;; firsts 
;; DEPENDENCIES: caar
;; DEPENDED ON BY: let, cars
(defun firsts (l) (if l (cons (caar l) (firsts (cdr l)))))

;; cars 
;; DEPENDENCIES: firsts
;; DEPENDED ON BY: ziplist
(defun cars (l) (firsts l))

;; list 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: div, progn
(defmacro list (&rest alist) (if alist `(cons ,(car alist) (list ,@(cdr alist)))))

;; coerce 
;; DEPENDENCIES: pair?
;; DEPENDED ON BY: quicksortatom, readline, sortatom
(defun coerce (l) (if (pair? l) (cons (car l) (coerce (cdr l))) (car l)))

;; > 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: eq?, maxlist, %
(defun > (a b) (< b a))

;; append 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: reversestr, reverse, flatten, inputnchars, quicksort
(defun append (lista listb) (if lista (cons (car lista) (append (cdr lista) listb)) listb))

;; inputnchars 
;; DEPENDENCIES: append
;; DEPENDED ON BY: NONE
(defun inputnchars (n) (if (- n 1) (append (inputnchars (- n 1)) (inputchar)) (inputchar)))

;; flatten 
;; DEPENDENCIES: append
;; DEPENDED ON BY: NONE
(defun flatten (l) (if l (if (atom? (car l)) (cons (car l) (flatten (cdr l))) (append (flatten (car l)) (flatten (cdr l))))))

;; reverse 
;; DEPENDENCIES: append, pair?
;; DEPENDED ON BY: reversestr, fibiter, fibrec
(defun reverse (l) (if (pair? l) (append (reverse (cdr l)) (cons (car l) ())) l))

;; reversestr 
;; DEPENDENCIES: append, reverse, pair?
;; DEPENDED ON BY: NONE
(defun reversestr (l) (if (pair? l) (append (reverse (cdr l)) (car l)) l))

;; apply 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: NONE
(defmacro apply (fn &rest params) `(,fn ,@params))

;; null? 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: eq?, not, or, and, remove
(defun null? (x) (if x () 't))

;; not 
;; DEPENDENCIES: null?
;; DEPENDED ON BY: fast_exp, eq?, exp, *, eqz?, quicksort
(defun not (x) (null? x))

;; cdar 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: cdrs
(defun cdar (l) (cdr (car l)))

;; cdrs 
;; DEPENDENCIES: cdar
;; DEPENDED ON BY: ziplist
(defun cdrs (l) (if l (cons (cdar l) (cdrs (cdr l)))))

;; ziplist 
;; DEPENDENCIES: cars, all?, cdrs
;; DEPENDED ON BY: NONE
(defun ziplist (ls) (if (all? (cars ls)) (cons (cars ls) (ziplist (cdrs ls)))))

;; + 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: fibrec_r, nievefib, ++, %, /, fibiter, *
(defun + (x y) (- x (- 0 y)))

;; % 
;; DEPENDENCIES: neg?, >, +
;; DEPENDED ON BY: div, even?
(defun % (x y) (if (neg? y) (if (> x y) (if (> x 0) (% (+ x y) y) x) (% (- x y) y)) (if (< x y) (if (< x 0) (% (+ x y) y) x) (% (- x y) y))))

;; ++ 
;; DEPENDENCIES: +
;; DEPENDED ON BY: /, fibiter
(defun ++ (x) (+ x 1))

;; cadar 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: letrec, seconds, cond
(defun cadar (l) (car (cdr (car l))))

;; cond 
;; DEPENDENCIES: cadar, caar
;; DEPENDED ON BY: eq?, or, and, remove
(defmacro cond (&rest options) (if (car options) `(if ,(caar options) ,(cadar options) (cond ,@(cdr options)))))

;; and 
;; DEPENDENCIES: cond, null?
;; DEPENDED ON BY: eq?, isdigit, samesign
(defmacro and (&rest vals) `(cond ((null? ',(cdr vals)) (if ,(car vals) 't)) (,(car vals) (and ,@(cdr vals))) ('t ())))

;; or 
;; DEPENDENCIES: cond, null?
;; DEPENDED ON BY: eq?, /, eqz?, nievefib, samesign
(defmacro or (&rest vals) `(cond ((null? ',(cdr vals)) (if ,(car vals) 't)) (,(car vals) 't) ('t (or ,@(cdr vals)))))

;; samesign 
;; DEPENDENCIES: pos?, or, neg?, and
;; DEPENDED ON BY: /
(defun samesign (x y) (or (and (pos? y) (pos? x)) (and (neg? y) (neg? x))))

;; eqz? 
;; DEPENDENCIES: or, not
;; DEPENDED ON BY: *, exp, fast_exp
(defun eqz? (a) (not (or (< a 0) (< 0 a))))

;; seconds 
;; DEPENDENCIES: cadar
;; DEPENDED ON BY: let, cadrs
(defun seconds (l) (if l (cons (cadar l) (seconds (cdr l)))))

;; cadrs 
;; DEPENDENCIES: seconds
;; DEPENDED ON BY: NONE
(defun cadrs (l) (seconds l))

;; * 
;; DEPENDENCIES: --, not, +, eqz?
;; DEPENDED ON BY: exp, fast_exp, sq
(defun * (x y) (if (< x y) (* y x) (if (not (eqz? y)) (if (< 0 y) (+ x (* x (-- y))) (- (* x (+ y 1)) x)) 0)))

;; sq 
;; DEPENDENCIES: *
;; DEPENDED ON BY: NONE
(defun sq (x) (* x x))

;; exp 
;; DEPENDENCIES: not, eqz?, *
;; DEPENDED ON BY: **
(defun exp (x y) (if (not (eqz? y)) (* x (exp x (- y 1))) 1))

;; any? 
;; DEPENDENCIES: reduce
;; DEPENDED ON BY: in?
(defun any? (l) (reduce or l))

;; allwhich 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: quicksort
(defun allwhich (l fn) (if l (if (fn (car l)) (cons (car l) (allwhich (cdr l) fn)) (allwhich (cdr l) fn))))

;; eq? 
;; DEPENDENCIES: or, cond, and, not, >, null?
;; DEPENDED ON BY: in?, fibrec_r, nievefib, /, newline?, even?, remove, nth, replaceall
(defun eq? (a b) (cond ((and (atom? a) (atom? b)) (not (or (< a b) (> a b)))) ((and (null? a) (null? b)) 't) ((or (atom? a) (atom? b)) ()) ('t (and (eq? (car a) (car b)) (eq? (cdr a) (cdr b))))))

;; replaceall 
;; DEPENDENCIES: eq?
;; DEPENDED ON BY: NONE
(defun replaceall (a b l) (if l (if (atom? l) (if (eq? l a) b l) (cons (replaceall a b (car l)) (replaceall a b (cdr l))))))

;; nth 
;; DEPENDENCIES: eq?
;; DEPENDED ON BY: NONE
(defun nth (l n) (if (eq? n 0) (car l) (nth (cdr l) (- n 1))))

;; remove 
;; DEPENDENCIES: eq?, cond, null?
;; DEPENDED ON BY: sort, defunv
(defun remove (item l) (cond ((null? l) ()) ((eq? (car l) item) (cdr l)) ('t (cons (car l) (remove item (cdr l))))))

;; defunv 
;; DEPENDENCIES: remove
;; DEPENDED ON BY: NONE
(defmacro defunv (name params &rest body) `(defmacro ,name ,params `((lambda ,',(remove '&rest params) ,',@body) ',,@(remove '&rest params))))

;; even? 
;; DEPENDENCIES: %, eq?
;; DEPENDED ON BY: fast_exp
(defun even? (x) (eq? (% x 2) 0))

;; newline? 
;; DEPENDENCIES: eq?
;; DEPENDED ON BY: readline
(defun newline? (char) (eq? char '\n))

;; nievefib 
;; DEPENDENCIES: eq?, or, +
;; DEPENDED ON BY: NONE
(defun nievefib (n) (if (or (eq? n 0) (eq? n 1)) 1 (+ (nievefib (- n 1)) (nievefib (- n 2)))))

;; in? 
;; DEPENDENCIES: eq?, any?, map
;; DEPENDED ON BY: isdigit
(defun in? (sym list) (any? (map (lambda (x) (eq? x sym)) list)))

;; isdigit 
;; DEPENDENCIES: and, in?
;; DEPENDED ON BY: NONE
(defun isdigit (sym) (if sym (and (in? (car sym) '(0 1 2 3 4 5 6 7 8 9)) (isdigit (cdr sym))) 't))

;; abs 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: /
(defun abs (x) (if (< x 0) (- 0 x) x))

;; / 
;; DEPENDENCIES: eq?, or, abs, --, ++, samesign, +
;; DEPENDED ON BY: div, fast_exp
(defun / (x y) (if (< (- (abs x) (abs y)) 0) (if (or (eq? x 0) (samesign x y)) 0 -1) (if (samesign x y) (++ (/ (- x y) y)) (-- (/ (+ x y) y)))))

;; div 
;; DEPENDENCIES: %, list, /
;; DEPENDED ON BY: NONE
(defun div (x y) (list (/ x y) (% x y)))

;; last 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: progn
(defun last (l) (if (cdr l) (last (cdr l)) (car l)))

;; progn 
;; DEPENDENCIES: last, list
;; DEPENDED ON BY: let, flet, print, prompt, letrec, **, do, printlist, while
(defmacro progn (&rest body) `(last (list ,@body)))

;; while 
;; DEPENDENCIES: progn
;; DEPENDED ON BY: NONE
(defmacro while (condition &rest body) `(if ,condition (progn ,@body (while ,condition ,@body))))

;; printlist 
;; DEPENDENCIES: progn, printspace
;; DEPENDED ON BY: NONE
(defun printlist (words) (if words (progn (printsym (car words)) (printspace) (printlist (cdr words)))))

;; letrec 
;; DEPENDENCIES: cadar, progn, caar
;; DEPENDED ON BY: do
(defmacro letrec (bindings &rest body) (if bindings `((lambda (,(caar bindings)) (letrec ,(cdr bindings) ,@body)) ,(cadar bindings)) `(progn ,@body)))

;; print 
;; DEPENDENCIES: progn, printnewline, printspace
;; DEPENDED ON BY: write, println
(defmacro print (newline? &rest params) `(if ',params (progn (printsym ,(car params)) (printspace) (print ,newline? ,@(cdr params))) (if ,newline? (printnewline))))

;; println 
;; DEPENDENCIES: print
;; DEPENDED ON BY: **
(defmacro println (&rest params) `(print 't ,@params))

;; ** 
;; DEPENDENCIES: progn, exp, println
;; DEPENDED ON BY: NONE
(defun ** (x y) (if (< y 0) (progn (println 'Exponent 'must 'not 'be 'less 'than 'zero) -1) (exp x y)))

;; write 
;; DEPENDENCIES: print
;; DEPENDED ON BY: prompt
(defmacro write (&rest params) `(print () ,@params))

;; flet 
;; DEPENDENCIES: progn
;; DEPENDED ON BY: NONE
(defmacro flet (bindings &rest body) (if bindings `((lambda () (progn (defun ,@(car bindings)) (flet ,(cdr bindings) ,@body)))) `(progn ,@body)))

;; let 
;; DEPENDENCIES: progn, firsts, seconds
;; DEPENDED ON BY: fibrec_r, sort, min, fast_exp, max, foreach, do, readline, fibiter, maxlist, minlist, quicksort
(defmacro let (bindings &rest body) `((lambda ,(firsts bindings) (progn ,@body)) ,@(seconds bindings)))

;; quicksort 
;; DEPENDENCIES: append, allwhich, let, not, pair?
;; DEPENDED ON BY: quicksortatom
(defun quicksort (l) (if (pair? l) (let ((pivot (car l))) (append (quicksort (allwhich (cdr l) (lambda (x) (< x pivot)))) (cons pivot (quicksort (allwhich (cdr l) (lambda (x) (not (< x pivot)))))))) l))

;; quicksortatom 
;; DEPENDENCIES: quicksort, coerce
;; DEPENDED ON BY: NONE
(defun quicksortatom (a) (coerce (quicksort a)))

;; maxlist 
;; DEPENDENCIES: cadr, let, >, cddr, pair?
;; DEPENDED ON BY: NONE
(defun maxlist (l) (if (pair? l) (let ((this (car l))) (if (> this (cadr l)) (maxlist (cons this (cddr l))) (maxlist (cdr l)))) (car l)))

;; readline 
;; DEPENDENCIES: let, newline?, coerce
;; DEPENDED ON BY: prompt
(defun readline () (let ((nextchar (inputchar))) (if (newline? nextchar) () (coerce (cons nextchar (readline))))))

;; prompt 
;; DEPENDENCIES: write, readline, progn
;; DEPENDED ON BY: NONE
(defun prompt (prompt) (progn (write prompt) (readline)))

;; do 
;; DEPENDENCIES: progn, let, letrec
;; DEPENDED ON BY: fibiter, foreach
(defmacro do (bindings test &rest body) `(letrec ,bindings (let ((result (progn ,@body))) (if ,test (do ,bindings ,test ,@body) result))))

;; fibiter 
;; DEPENDENCIES: let, reverse, do, --, ++, +
;; DEPENDED ON BY: NONE
(defun fibiter (n) (reverse (let ((x 0) (f1 0) (f2 1) (nums '(1)) (n (-- n))) (do ((x (++ x)) (f3 (+ f1 f2)) (nums (cons f3 nums)) (f1 f2) (f2 f3)) (< x (++ n)) nums))))

;; foreach 
;; DEPENDENCIES: do, let
;; DEPENDED ON BY: NONE
(defmacro foreach (itemname alist bindings &rest body) `(let ((alist ,alist)) (do ((,itemname (car alist)) (alist (cdr alist)) ,@bindings) alist ,@body)))

;; max 
;; DEPENDENCIES: let, pair?
;; DEPENDED ON BY: NONE
(defmacro max (&rest l) `(if ',(pair? l) (let ((nextmax (max ,@(cdr l)))) (if (< ,(car l) nextmax) nextmax ,(car l))) ,(car l)))

;; fast_exp 
;; DEPENDENCIES: even?, let, not, eqz?, /, *
;; DEPENDED ON BY: NONE
(defun fast_exp (x y) (if (not (eqz? y)) (let ((half_exp (fast_exp x (/ y 2)))) (if (even? y) (* half_exp half_exp) (* half_exp (* half_exp x)))) 1))

;; min 
;; DEPENDENCIES: let, pair?
;; DEPENDED ON BY: NONE
(defmacro min (&rest l) `(if ',(pair? l) (let ((nextmin (min ,@(cdr l)))) (if (< ,(car l) nextmin) nextmin ,(car l))) ,(car l)))

;; fibrec_r 
;; DEPENDENCIES: eq?, cadr, let, --, +
;; DEPENDED ON BY: fib, fibrec
(defun fibrec_r (n) (if (eq? n 1) '(1 1) (let ((discovered (fibrec_r (-- n)))) (cons (+ (car discovered) (cadr discovered)) discovered))))

;; fibrec 
;; DEPENDENCIES: fibrec_r, reverse
;; DEPENDED ON BY: NONE
(defun fibrec (n) (reverse (fibrec_r n)))

;; fib 
;; DEPENDENCIES: fibrec_r
;; DEPENDED ON BY: NONE
(defun fib (n) (car (fibrec_r n)))

;; minlist 
;; DEPENDENCIES: cadr, let, cddr, pair?
;; DEPENDED ON BY: sort
(defun minlist (l) (if (pair? l) (let ((this (car l))) (if (< this (cadr l)) (minlist (cons this (cddr l))) (minlist (cdr l)))) (car l)))

;; sort 
;; DEPENDENCIES: minlist, remove, let, pair?
;; DEPENDED ON BY: sortatom
(defun sort (l) (if (pair? l) (let ((themin (minlist l))) (cons themin (sort (remove themin l)))) l))

;; sortatom 
;; DEPENDENCIES: coerce, sort
;; DEPENDED ON BY: NONE
(defun sortatom (a) (coerce (sort a)))

