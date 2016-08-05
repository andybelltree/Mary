

;; In Lisp Let over Lambda provides OO like structure using Lexical Closures

;; Anonymous object

((cadr
  ((lambda ()
   (let ((height 2) (width 3) (breadth 2))
     (list
     (lambda () (* height (* breadth width)))
     (lambda () (* width breadth))))
))))

;; Classes

(defun CubeClass (height width breadth)
     (let ((_height height) (_width width) (_breadth breadth))
     (list
      (lambda () (* _height (* _breadth _width)))
      (lambda () (* _width _breadth))
      (lambda (x) (* _width (* _breadth (* _height x)))))
     )
   )

(let ((cube (CubeClass 3 4 5)))
  (list
   ((car cube)) ;; Volume
   ((cadr cube)) ;; Area
   ((caddr cube) 2) ;; 2 * Volume
   )
  )


;; Problem is with a functional you can't really have state. Or can you?
