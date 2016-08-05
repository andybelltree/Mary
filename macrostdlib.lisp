;; defun 
;; DEPENDENCIES: quasiquote, splice, unquote, lambda, defmacro, quote
;; DEPENDED ON BY: 
(defmacro defun (name params &rest body) `(defmacro ,name ,params `((lambda ,',params ,',@body) ,,@params)))

