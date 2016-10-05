(defun display_row (row)
  (println (car row) '| (cadr row) '| (caddr row))
  )

(defun display_board (board)
  (progn
    (display_row (car board))
    (println '----------)
    (display_row (cadr board))
    (println '----------)
    (display_row (caddr board))
    )
  )  

(defun cell (board coord)
  (nth (nth board (car coord)) (cadr coord))
  )

(defun get_digit (prompt_str)
  (do ((digit (prompt prompt_str)))
      (or (not digit) (and (not (isdigit digit)) (not (eq? 'q digit))))
    (if (not (or (isdigit digit) (eq? 'q digit))) (println 'Not 'a 'digit))
   digit)
  )

(defun get_cell (player)
  (progn (println 'player player)
	 (cons (get_digit 'row:) (cons (get_digit 'column:) ())))
  )

(defun new_board ()
  '((- - -) (- - -) (- - -))
  )

(defun update_cell (board coord val)
  (let ((row (car coord)) (col (cadr coord)))
    (if (eq? row 0)
	(cons (update_row (car board) col val) (cdr board))
	(cons (car board) (update_cell (cdr board) (cons (- row 1) (list col)) val))
	)
  )
  )

(defun update_row (row col val)
  (if (eq? col 0)
      (if (or (eq? 'O (car row)) (eq? 'X (car row)))
	  (progn (println 'cell 'already 'taken) row)
	  (cons val (cdr row)))
      (cons (car row) (update_row (cdr row) (- col 1) val))
      )
  )

(defun good_row (row player)
  (all? (map (lambda (x) (eq? x player)) row))
  )

(defun horizontal? (board player)
  (let ((wins ()))
    (any? 
     (foreach row board ((wins (cons (good_row row player) wins)))
	      wins
       )
    )
   )
  )

(defun vertical? (board player)
  (horizontal? (ziplist board) player)
  )

(defun diagonal? (board player)
  (horizontal? (list
		 (list
		  (car (car board))
		  (cadr (cadr board))
		  (caddr (caddr board)))
		 (list
		  (car (caddr board))
		  (cadr (cadr board))
		  (caddr (car board))
		 )
		 )
	       player)
  )

(defun full_board? (board)
  (all?
   (map (lambda (row)
	  (all? (map
		 (lambda (square) (not (eq? square '-)))
		 row)))
	board))
  )

(defun complete? (board player)
  (cond
   ((or
    (horizontal? board player)
    (vertical? board player)
    (diagonal? board player))
    (progn (println player 'wins!) 't))
   ((full_board? board)
    (progn (println 'Draw!) 't)
    )
   )
)

(defun main ()
(let ((board (new_board)) (player 'O))
  (display_board board)
  (do
   ((last_board board)
    (last_player player)
    (coords (get_cell player))
   (board (if (not (in? 'q coords)) (update_cell board coords player) board))
    (player (cond
	      ((eq? last_board board) player)
	      ((eq? player 'O) 'X)
	      ('t 'O))))
     (and (not (in? 'q coords)) (not (complete? board last_player)))
      (printnewline)
      (display_board board)
    )
  )
)

(main)


