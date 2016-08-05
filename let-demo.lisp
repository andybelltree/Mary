
(let ((foo 'hello) (bar 'world))
  (println foo bar)
  )


(defun circumference (r)
  (let ((pi 3.14159))
    (* 2 (* pi r))
    )
  )

(flet ((myfunc (x y) (+ x y)))
  (myfunc 1 2)
  )

(flet ((myfunc (x y) (if (> x y) () (cons x (myfunc (+ x 1) y)))))
  (let ((x 1) (y 10))
    (myfunc x y)
    )
  )

; (circumference 3)


(let ((let '`(let ((let ',let)),let)))`(let ((let ',let)),let)) ; Evaluates to itself

