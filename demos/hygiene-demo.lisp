;; Unhygienic macros:

(defmacro my-unless (condition &rest body)
  `(if (not ,condition)
     (progn
       ,@body)))

(my-unless 't
   (println 'This\sshould\snot\sbe\sprinted!))

(flet ((not (x) x))
   (my-unless 't
	      (println 'This\sshould\snot\sbe\sprinted!)))
