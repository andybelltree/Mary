'a ;; a
(car '(a b c)) ;; a
(cdr '(a b c)) ;; (b c)
(cons 'a '(b c)) ;; (a b c)
(if 'a 'b 'c) ;; b
(if () 'b 'c) ;; c
(if 'a ()) ;; ()
(if () 'a) ;; ()
(if 0 'a 'b) ;; a
(- 5 3) ;; 2
(< 2 9) ;; 2
(< 'hello 'bart) ;;
(atom? 'a) ;; a
(atom? '(a b c)) ;; ()
((lambda (x) x) 3) ;; 3
((lambda (x y) y) 1 2) ;; 2
(defmacro cadr (x) `(car (cdr ,x))) ;; cadr
(cadr '(a b c)) ;; b
(defmacro cdar (x) `(cdr (car ,x))) ;; cdar
(cdar '((a d) b c)) ;; (d)
