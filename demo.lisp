;; Function Definition

(defun foo (x)
  (if (< x 5)
      (+ x 1)
      (* x 2))
  )

(defun bar (x y)
  (if (< x y)
      x
      y
  )
)

(defun fizzbuzz (x y)
  (cond ((> x y) ())
	((and (eqz? (% x 5))
	      (eqz? (% x 3)))
	 (cons 'fizzbuzz
		(fizzbuzz (+ x 1) y)))
	((eqz? (% x 5))
	 (cons 'fizz
		(fizzbuzz (+ x 1) y)))
	((eqz? (% x 3))
	 (cons 'buzz
		(fizzbuzz (+ x 1) y)))
	('t
	 (cons x (fizzbuzz (+ x 1) y)))
	)
)

;; Hello World!

(defun hello_x ()
  (progn
    (println 'Hello 'World!)
   (println
    (cons 'Hello\s
   	 (prompt 'What\sis\syour\sname?)))
  )
  )
