(defmacro defun (name params &rest body) ; defun macro
  `(defmacro ,name ,params
     `((lambda ,',params ,',@body) ,,@params)
     )
  ) ;; defun
(defun cadr (l)
  (car (cdr (l)))) ;; cadr
(cadr '(1 2 3)) ;; 2
