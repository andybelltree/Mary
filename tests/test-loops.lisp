(let ((x 0) (nums ()) ) (do ((x (++ x)) (nums (cons x nums))) (< x 10) nums)) ;; (10 9 8 7 6 5 4 3 2 1)
(let ((x 2) (nums ()) ) (do ((x (* x x)) (nums (cons x nums))) (< x 17) nums)) ;; (256 16 4)
(let ((x 0) (nums ()) ) (do ((x (++ x)) (nums (cons (* x x) nums))) (< x 5) nums)) ;; (25 16 9 4 1)
