(defmacro defun (name params &rest body) ; defun macro
  `(defmacro ,name ,params
     `((lambda ,',params ,',@body) ,,@params)
     )
  )
