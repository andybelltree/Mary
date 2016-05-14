;; (defmacro defun (name params &rest body) ; defun macro
;;   `(defmacro ,name (&rest params)
;;      (cons '(lambda ,params ,@body) params)
;;      )
;;   )

(defmacro list (&rest alist)
  (if alist
      `(cons ,(car alist) (list ,@(cdr alist)))
      )
  )

(defmacro defun (name params &rest body) ; defun macro
  `(defmacro ,name ,params
     `((lambda ,',params ,',@body) ,,@params)
     )
  )


(defun cadr (l)
  (car (cdr l))
  )

(defun cdar (l)
  (cdr (car l))
  )

(defun caar (l)
  (car (car l))
  )

(defun cddr (l)
  (cdr (cdr l))
  )

(defun cadar (l)
  (car (cdr (car l)))
  )

(defun caadr (l)
  (car (car (cdr l)))
  )

(defun cdadr (l)
  (cdr (car (cdr l)))
  )



(defmacro cond (&rest options)
  (if (car options)
      `(if ,(caar options)
	   ,(cadar options)
	   (cond ,@(cdr options))))
  )


(defmacro and (&rest vals)
  `(cond
    ((null ',(cdr vals)) (if ,(car vals) 't)) ; at the end of the list, return the car value
    (,(car vals) (and ,@(cdr vals))) ; or if the first value is true
    ('t ())			; call recursively on the rest. Otherwise return false
    )
  )

(defmacro or (&rest vals)
  `(cond
    ((null ',(cdr vals)) (if ,(car vals) 't)) ; at the end of the list, return the car value
    (,(car vals) 't) ; or if the first value is true return true,
    ('t (or ,@(cdr vals))) ; otherwise call recursively on the rest.
    )
  )

(defun map (fn l)
  (if l
      (cons (fn (car l)) (map fn (cdr l)))
  )
  )

(defmacro do (bindings test &rest body)
  `(letrec ,bindings
     (let ((result (progn ,@body)))
     (if ,test (do ,bindings ,test ,@body) result))
     )
 )

;; From Graham
(defmacro with-gensyms (syms &rest body)
  `(let ,(map (lambda (s) `(,s (gensym))) syms)
     ,@body))

(defmacro withgensyms (gensyms &rest body)
  (if gensyms
      (replaceall `(car ,gensyms) '(gensym) `(withgensyms ,(cdr gensyms) ,@body)
      )
   '(progn body)
   )
  )

(defun replaceall (a b l) ;; replaces all occurences of a with b in l
  (if l
  (if (atom? l) (if (eq? l a) b l)
	(cons (replaceall a b (car l)) (replaceall a b (cdr l)))
	)
  )
  )

(defun getgensymslist (l)
  (if l
      (cons (cons (car l) (list '(gensym))) (getgensymslist (cdr l)))
      )
)

(defmacro let (bindings &rest body)
      `((lambda ,(firsts bindings) (progn ,@body)) ,@(seconds bindings))
      )

(defmacro letrec (bindings &rest body)
  (if bindings
      `((lambda (,(caar bindings)) (letrec ,(cdr bindings) ,@body)) ,(cadar bindings))
      `(progn ,@body)
      )
  )



(defmacro defunv (name params &rest body) ; defun macro allows &rest keyword. WIP
  `(defmacro ,name ,params
     `((lambda ,',(remove '&rest params) ,',@body) ',,@(remove '&rest params))
     )
    )


(defun > (a b)
  (< b a)
  )

(defun eq? (a b)
  (not (or (< a b) (> a b)))
  )

(defun null (x)
  (if x () 't)
  )

(defun not (x)
  (null x))


(defun pair? (x)
  (if (cdr x) 't)
  )

(defmacro apply (fn &rest params)
`(,fn ,@params)
)

(defmacro lambdachain (params vals &rest body)
  (if (cdr params)
      `((lambda (,(car params)) (lambdachain ,(cdr params) ,(cdr vals) ,body)) ,(car vals))
      `((lambda (,(car params)) ,@body) ,(car vals))
      )
  )

(defun append (lista listb)
  (if lista
      (cons (car lista) (append (cdr lista) listb))
      listb
      )
  )

(defun cars (l)
  (firsts l)
)

(defun cdrs (l)
  (if l
      (cons (cdar l) (cdrs (cdr l))))
  )

(defun cadrs (l)
  (seconds l)
  )


(defun firsts (l)
  (if l
      (cons (caar l) (firsts (cdr l)))
      )
 )

(defun seconds (l)
  (if l
      (cons (cadar l) (seconds (cdr l)))
      )
  )


(defmacro progn (&rest body)
  `(last (list ,@body))
  )

(defmacro apply-to-list (fn args)
  (flatten (cons fn args))
)

(defun flatten (l)
  (if l
      (if (atom? (car l)) (cons (car l) (flatten (cdr l)))
	(append (flatten (car l)) (flatten (cdr l)))))
)

(defun mapcarl (fn ls)
  (if (cars ls)
      (cons (apply-to-l fn (cars ls)) (mapcarl fn (cdrs ls))))
)

;; Not working yet
;(defmacro mapcar (fn &rest ls)
;  `(if (cars ,ls)
;      (cons (apply-to-l ,fn (cars ,ls))) (apply-to-l mapcar (apply-to-l ,fn (cdrs ,ls))))
;  )

(defmacro mapcar (fn &rest ls)
  `((lambda (fn ls) (if (car ls) (cons (fn (cars ls)) (mapcar fn (cdrs ls))))) ,fn ,',ls)
  )



(defmacro zip (&rest ls)
  `((lambda (ls) (if (car ls) (cons (cars ls) (zip (cdrs ls))))) ,ls)
  )


(defun cons_to_2 (x) (cons x '(2)))
(defun double (x) (* 2 x))
(defun id (x) x)
(defun first (x) (id x))
(defun second (x y) (last (l x y)))
(defun third (x y z) (last (l x y z)))
(defun fourth (x y z a) (last (l x y z a)))

(defun reverse (l)
  (if (pair? l)
      (append (reverse (cdr l)) (cons (car l) ()))
      l
      )
  )


(defun reversestr (l)
  (if (pair? l)
      (append (reverse (cdr l)) (car l))
      l
      )
  )

(defun last (l)
  (if (cdr l)
      (last (cdr l))
      (car l)
  )
)

(defun + (x y)
  (- x (- 0 y))
  )

(defun * (x y)
      (if y
	  (if (< 0 y)
	      (+ x (* x (-- y)))
	      (- (* x (+ y 1)) x)
	      )
	  0)
      )

(defun / (x y)
  (if (< (- (abs x) (abs y)) 0)
      (if (samesign x y) 0 -1)
      (if (samesign x y)	    
	  (++ (/ (- x y) y))
	  (-- (/ (+ x y) y))
      )
  )
)
(defun % (x y)
  (if (neg y)
      ;; Special case
      (if (> x y)
	  (if (> x 0) (% (+ x y) y) x)
	  (% (- x y) y)
	  )
      (if (< x y)
	  (if (< x 0) (% (+ x y) y) x)
	  (% (- x y) y)
	  )
      )
  )

;; True iff positive
(defun pos (x)
  (< 0 x)
  )

;; True iff negative
(defun neg (x)
  (< x 0)
  )

;; Absolute value of x
(defun abs (x)
  (if (< x 0) (- 0 x) x)
  )

;; True iff x and y have the same sign
(defun samesign (x y)
  (or
   (and (pos y) (pos x))
   (and (neg y) (neg x))
   )
  )

;; Returns a list of the quotient and remainder
(defun div (x y)
  (list (/ x y) (% x y))
  )

(defun ++ (x) (+ x 1))
(defun -- (x) (- x 1))
(defun sq (x) (* x x))

(defun printspace ()
  (printsym '\s)
  )

(defun printnewline ()
  (printsym '\n)
  )

;; Print symbols. Newline is a boolean for whether to print a newline at the end
(defmacro print (newline? &rest params)
  `(if ',params
       (progn
	 (printsym ,(car params))
	 (printspace)
	 (print ,newline? ,@(cdr params))
	 )
   (if ,newline? (printnewline))
   )
  )

;; Print symbols with a newline at the end
(defmacro println (&rest params)
 `(print 't ,@params)
 )

;; Print symbols without a newline at the end
(defmacro write (&rest params)
`(print () ,@params)
)

;; Collects n characters of input
(defun inputnchars (n)
  (if (- n 1)
      (append (inputnchars (- n 1)) (inputchar))
      (inputchar)
  )
  )

(defun readline ()
  (let ((nextchar (inputchar)))
    (if
     (newline? nextchar)
     ()
     (coerce (cons nextchar (readline)))
   )
  )
  )

(defun newline? (char)
  (eq? char '\n)
  )

;; Turns a l of symbols into a string
(defun coerce (l)
  (if (pair? l)
      (cons (car l) (coerce (cdr l)))
      (car l)
      )
  )

(defun prompt (prompt)
  (progn
    (write prompt)
    (readline)
    )
  )

(defun min (l)
  (if (pair? l)
      (let ((this (car l)))
	(if
	 (< this (cadr l))
	 (min (cons this (cddr l)))
	 (min (cdr l))
	 )
	)
      (car l)
   )
  )


;; Remove first instance of a given item from the l
(defun remove (item l)
  (cond
    ;; If the list is empty, item wasn't in it. Return
    ((null l) ())
    ;; If you've found the item, return the rest
    ((eq? (car l) item)
     (cdr l))
    ;; Otherwise keep looking
    ('t (cons (car l) (remove item (cdr l))))
    )
  )

;; Selection sort.
;; Could probably be more efficient, but it works for now
(defun sort (l)
  (if (pair? l)
      (let ((themin (min l)))
	(cons themin (sort (remove themin l)))
      )
      l
      )
  )


;; Nieve approach to fibonacci. Slow but doesn't hit max recursion depth till over 15. Too slow to bother after that anyway.

(defun nieve_fib (n)
  (if (or (eq? n 0) (eq? n 1)) 1
      (+ (nieve_fib (- n 1)) (nieve_fib (- n 2)))
      )
  )

;; Finds the fibonacci sequence up to n iteratively. Goes up to 11
(defun fibiter (n)
  (reverse
  (let ((x 0) (f1 0) (f2 1) (nums '(1)) (n (-- n)))
    (do ((x (++ x)) (f3 (+ f1 f2)) (nums (cons f3 nums)) (f1 f2) (f2 f3)) (< x n) nums)
    )
  )
  )

;; More recursive way of finding the nth fibonacci number. Can go up to 54
(defun fibrec (n)
  (reverse (fibrec_r n))
  )

(defun fibrec_r (n)
  (if (eq? n 1) '(1 1)
      (let ((discovered (fibrec_r (-- n))))
	(cons (+ (car discovered) (cadr discovered)) discovered)
	 ))
  )

;; Just the last value in fibonacci series found recursively. Same limitations as above
(defun fibrec_1 (n)
  (car (fibrec_r n))
  )

(let ((let '`(let ((let ',let)),let)))`(let ((let ',let)),let)) ; Evaluates to itself
;; credit - Mike McMahon

;;(println 'Loaded 'std 'library)
