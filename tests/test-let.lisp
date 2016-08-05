(let ((a 2) (b 5)) (- b a)) ;; 3
(let ((x 'hello) (y 'world)) (list x y)) ;; (hello world)
(let ((x 'hello) (y '_world)) (append x y)) ;; hello_world
(letrec ((x 'hello) (y x)) y) ;; hello
(letrec ((a 3) (y (- a 2))) (- y 1)) ;; 0
(flet ((sqr (x) (* x x))) (sqr (sqr 2))) ;; 16
(flet ((sqr (x) (* x x)) (half (x) (/ x 2))) (half (sqr 2))) ;; 2
