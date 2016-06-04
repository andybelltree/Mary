(cons 1 '(2 3 4)) ;; (1 2 3 4)
(car '(4 3 2 1)) ;; 4
(cdr '(a b c d)) ;; (b c d)
(reverse '(list goes back)) ;; (back goes list)
(minlist '(5 33 2 100)) ;; 2
(minlist '(c d h q)) ;; c
(maxlist '(c d h q)) ;; q
(maxlist '(5 33 2 100)) ;; 100
(sort '(4 55 2 1)) ;; (1 2 4 55)
(nth '(2 3 4) 1) ;; 3
(nth '(2 3 4 5) 2) ;; 4
(nth (nth '(2 3 (4 hello) 5) 2) 1) ;; hello
