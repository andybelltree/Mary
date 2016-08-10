
(defun dfs (node tree)
  (if tree
      (if (eq? node (car tree))
	  node
	  (if (dfs node (cadr tree))
	      node
	      (dfs node (caddr tree))
	  )
      )
  )
)


;; (defun dfs (node tree)
;;   (if tree
;;        (or (eq? node (car tree))
;; 	   (dfs node (cadr tree))
;; 	   (dfs node (caddr tree))
;; 	  )
;;       )
;;   )



  (let ((the_tree
  	 '(a
  	   (b (c) (d))
  	   (e (f) (g (h))))
  	 ))
    (dfs 'z the_tree)
    )

  ;; (let ((the_tree
  ;; 	 '(a
  ;; 	   (b)
  ;; 	   (c)
  ;; 	 )))
  ;;   (dfs 'c the_tree)
  ;;   )


  ;; (let ((the_tree
  ;; 	 '(a
  ;; 	   (b (d (p (q (r (s (t (u (v) (w)) (x))) (y))))) (e (m (n) (o))))
  ;; 	   (c (f (h (i (j (k (l)))))) (g))
  ;; 	 )))
  ;;   (dfs 'g the_tree)
  ;;   )
