
;; Lexical scope vs. dynamic

(defun get-temp-func ()
  (let ((temp 1))
  (lambda () temp)
  )
  )

(let ((temp-func (get-temp-func)))
  (let ((temp 2))
    (temp-func)
  )
)
