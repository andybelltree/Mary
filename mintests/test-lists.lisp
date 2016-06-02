(car (cons 'a ())) ;; a
(car (cons 'a (cons 'b ()))) ;; a
(cadr (cons 'a (cons 'b ()))) ;; b
(cadr (cons 'a (cons 'b (cons 'c ())))) ;; b
(car (reverse (cons 'a (cons 'b (cons 'c ()))))) ;; c
