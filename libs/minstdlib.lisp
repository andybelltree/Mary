(defun or (x y) (if x x y))

(defun and (x y) (if x y x))

(defun cons (x y) (lambda (f) (f x y)))

(defun car (c) (c (lambda (x y) x)))

(defun cdr (c) (c (lambda (x y) y)))

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

(defun cddar (l)
  (cdr (cdr (car l)))
  )

(defun caddr (l)
  (car (cdr (cdr l)))
  )

(defun cdddr (l)
  (cdr (cdr (cdr l)))
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

(defun null? (x)
  (if x () x)
  )

(defun pair? (x)
  (cdr x)
  )

(defun append (lista listb)
  (if lista
      (cons (car lista) (append (cdr lista) listb))
      listb
      )
  )

(defun reverse (l)
  (if (pair? l)
      (append (reverse (cdr l)) (cons (car l) ()))
      l
      )
  )

(defun firsts (l)
  (if l
      (cons (caar l) (firsts (cdr l)))
      )
 )

(defun seconds (l)
  (if l
      (cons (cadar l) (seconds (cdr l)))
      )
  )

(defun reduce (fn l)
  (if (pair? l)
      (fn (car l) (reduce fn (cdr l)))
      (car l)
      )
  )

(defun map (fn l)
  (if l
      (cons (fn (car l)) (map fn (cdr l)))
  )
  )
