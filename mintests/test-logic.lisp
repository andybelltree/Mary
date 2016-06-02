(or 'a 'b) ;; a
(and 'a 'b) ;; b
(or () 'b) ;; b
(or 'a ()) ;; a
(and () 'b) ;; ()
(and 'a ()) ;; ()
(or () ()) ;; ()
(and () ()) ;; ()
