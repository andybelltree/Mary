(map (lambda (x) (** x 2)) '(1 2 3 4 5 6)) ;; (1 4 9 16 25 36)
(map (lambda (x) (div x 3)) '(3 6 9)) ;; ((1 0) (2 0) (3 0))
(map ++ (map ++ '(1 2 3 4))) ;; (3 4 5 6)
(map ++ '(1 2 3)) ;; (2 3 4)
(map sq '(1 2 3)) ;; (1 4 9)
(map (lambda (x) (+ 3 x)) '(1 2 3)) ;; (4 5 6)
(reduce + '(3 4 5)) ;; 12
(reduce (lambda (x y) (append y (cons x ()))) '(a b c)) ;; (c b a)
(reduce ** '(2 3 2)) ;; 512
(in? 'a '(a b c)) ;; t
(in? 'c '(a b d)) ;; ()
