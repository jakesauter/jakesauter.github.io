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
