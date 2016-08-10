;; Actually can create changing states with macros:

(defun next () 0)

(defmacro count ()
  `(progn (defun next () (+ ,(next) 1))
	  (next))
  )

(defmacro resetcount ()
  '(progn (defun next () 0)
    0)
  )

(reverse
(let ((the_next ()))
(do ((the_next (cons (count) the_next)))
    (< (car the_next) 20)
  (println (car the_next))
  the_next
  )
)
)
