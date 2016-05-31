(defun or (x y) (if x y x))

(defun and (x y) (if x x y))

(defun cons (x y) (lambda (f) (f x y)))

(defun car (c) (c (lambda (x y) x)))

(defun cdr (c) (c (lambda (x y) y)))

(defun cadr (l) (car (cdr l)))

(defun cadr (l)
  (car (cdr l))
  )

(defun cdar (l)
  (cdr (car l))
  )

(defun caar (l)
  (car (car l))
  )

(defun cddr (l)
  (cdr (cdr l))
  )

(defun cadar (l)
  (car (cdr (car l)))
  )

(defun caadr (l)
  (car (car (cdr l)))
  )

(defun cdadr (l)
  (cdr (car (cdr l)))
  )

(defun cond (options)
  (if (car options)
      (if (caar options)
	   (cadar options)
	   (cond (cdr options))))
  )

(defun set (label val)
  (defun 'label () val)
  )

