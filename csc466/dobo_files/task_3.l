;;;; Task 3 -- Possible Moves List, Modelling a game and filtering possible moves
;

;; Task 3.1 -- making the possible moves list
;            - a move is a list of points 
;  ;Algorithm to generate all possible moves for an nxn dimesional board:
;     ;Generating all horizontal moves
;     -loop controlling row
;       -loop controlling line size
;         -loop controlling start position
;          -conditional to check if col-size is greater than or equal to line size - start position
;          -generate move
;     ;Generating all vertical Moves 
;     -loop controlling col
;       -loop controlling line size
;         -loop controlling start position
;           -conditional to check if row-size is greater than or equal to line size - start position
;            -generate move
;
( defmethod set-pm-list ()
  ;Generating all Horizontal Moves 
  ( setf h-possible-moves '() )
  ( let ( ( row 0 ) ( line-size size ) ( start-pos 0 ) ( move '() ) ( i 0 ) ) 
    ( loop while ( < row size ) do
      ( setf line-size 1 )
      ( loop while ( not ( > line-size size ) ) do 
        ( setf start-pos 0 )
        ( loop while ( < start-pos size ) do
          ( if ( < size ( + line-size start-pos ) )
            ( return )
          )
          ;add this move to the list 
          ( setf col start-pos )
          ( setf i line-size )
          ( setf move '() )
          ( loop while ( > i 0 ) do
            ( setf move ( snoc ( list row col ) move ) )
            ( setf i ( - i 1 ) )
            ( setf col ( + col 1 ) )
          )
          ( setf h-possible-moves ( snoc move h-possible-moves ) )
          ( setf start-pos ( + start-pos 1 ) )
        )
        ( setf line-size ( + line-size 1 ) )
      )
      ( setf row ( + row 1 ) )
    )
  )
  ;Generating all Vertical Moves 
  ( setf v-possible-moves '() )
  ( let ( ( col 0 ) ( line-size size ) ( start-pos 0 ) ( move '() ) ( i 0 ) ) 
    ( loop while ( < col size ) do
      ( setf line-size 1 )
      ( loop while ( not ( > line-size size ) ) do 
        ( setf start-pos 0 )
        ( loop while ( < start-pos size ) do
          ( if ( < size ( + line-size start-pos ) )
            ( return )
          )
          ;add this move to the list 
          ( setf row start-pos )
          ( setf i line-size )
          ( setf move '() )
          ( loop while ( > i 0 ) do
            ( setf move ( snoc ( list row col ) move ) )
            ( setf i ( - i 1 ) )
            ( setf row ( + row 1 ) )
          )
          ( setf v-possible-moves ( snoc move v-possible-moves ) )
          ( setf start-pos ( + start-pos 1 ) )
        )
        ( setf line-size ( + line-size 1 ) )
      )
      ( setf col ( + col 1 ) )
    )
  )
  ; Removing all duplicates and combining lists
  ( dolist ( i h-possible-moves )
    ( dolist ( j v-possible-moves )
      ( if ( equal i j ) ( setf v-possible-moves ( remove j v-possible-moves ) ) )
    )
  )
  ( setf pm-list ( append h-possible-moves v-possible-moves ) )
)

;; function to dipslay the possible moves list
;  one move per line, and numbering to help with
;   move selection 
;
( defun display-pm-list ()
  ( format t "~%~%---------------------------------------------------------~%" )
  ( let ( ( count 1 ) )
    ( dolist ( x pm-list )
      ( format t "~A: ~A~%" count x )
      ( setf count ( + count 1 ) )
    )
  )
  ( format t "---------------------------------------------------------~%~%" )
)
 
( defun dobo-demo-3.1 ()
  ( set-pm-list )
  ( format t "~%possible Moves List for ~A x ~A board: ~%" size size )
  ( display-pm-list )
  ( setf size 4 )
  ( set-pm-list )
  ( format t "Possible Moves List for ~A x ~A board: ~%~%" size size )
  ( display-pm-list )
  ( setf size 5 )
  ( set-pm-list )
  ( format t "Possible Moves List for ~A x ~A board: ~%" size size )
  ( display-pm-list )
)

;; Task 3.2 -- defining a game 
;
( defclass game ()
  ( ( state-list :accessor game-state-list :initarg :state-list :initform '() ) )
)

( defmethod display ( ( g game ) )
  ( terpri )
  ( dolist ( state ( game-state-list g ) )
    ( display state )
  )
)

( defun dobo-demo-3.2 ( &aux game state )
  ( format t "Making an instance of a game ...~%" )
  ( setf game ( make-instance 'game ) )
  ( format t "Manually adding a few states to the game ...~%" )
  ( setf state ( make-instance 'state ) )
  ( setf ( game-state-list game ) ( list state state state state ) )
  ( format t "Displaying the game ...~%" )
  ( display game )
)


;; Task 3.3 -- filtering the possible moves list after a move 
;
( defun filter-possible-moves ( move )
  ( dolist ( i move )
    ( dolist ( j pm-list )
      ( dolist ( x j )
        ( if ( equal i x ) ( setf pm-list ( remove j pm-list ) ) )
      )
    )
  )
  nil
)

( defun dobo-demo-3.3 (&aux state move)
  ( set-pm-list )
  ( display-pm-list )
  ( setf state ( make-instance 'state ) )
  ( dotimes ( i 5 )
    ( display state )
    ( setf move ( nth ( random ( length pm-list ) ) pm-list ) ) 
    ( format t "The move that will be applied is ~A~%" move )
    ( setf state ( apply-move move state ) )
    ( display state )
    ( format t "Eliminating now impossible moves from the possbile moves list ...~%" )
    ( filter-possible-moves move )
    ( display-pm-list )
  )
)

;; Task 3 all inclusive demo
;
( defun dobo-demo-3 () 
  ( format t "dobo-demo-3.1 returned ~A~%" ( dobo-demo-3.1 ) )
  ( format t "dobo-demo-3.2 returned ~A~%" ( dobo-demo-3.2 ) )
  ( format t "dobo-demo-3.3 returned ~A~%" ( dobo-demo-3.3 ) )
)

