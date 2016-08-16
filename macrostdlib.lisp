;; defun 
;; DEPENDENCIES: NONE
;; DEPENDED ON BY: NONE
(defmacro defun (name params &rest body) `(defmacro ,name ,params `((lambda ,',params ,',@body) ,,@params)))

