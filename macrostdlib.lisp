;; defun 
;; DEPENDENCIES: splice, quasiquote, defmacro, quote, lambda
;; DEPENDED ON BY: 
(defmacro defun (name params &rest body) `(defmacro ,name ,params `((lambda ,',params ,',@body) ,,@params)))

