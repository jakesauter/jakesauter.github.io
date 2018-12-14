;; Task 1.1 -- Modelling the state class
;
( defclass state ()
  ( ( board :accessor state-board :initarg :board :initform ( make-full-board ) ) )
)

( defmethod make-full-board ( &aux board )
  ( setf board ( make-list 2 :initial-element size ) )  
  ( setf board ( make-array board :initial-element 1 ) )
)

( defmethod display ( ( s state ) &aux board )
  ( terpri )
  ( setf board ( state-board s ) )
  ( let ( ( i 0 ) ( j 0 ) )
    ( loop while ( not ( = i ( array-dimension board 0 ) ) ) do 
      ( setf j 0 )
      ( loop while ( not ( = j ( array-dimension board 1 ) ) ) do 
         ( format t "  ~A" ( aref board i j ) )
         ( setf j ( + j 1 ) )
      )   
      ( terpri )
      ( setf i ( + i 1 ) )
    )
  )
  ( terpri )
)

;; A demo of making and displaying a state
;
( defun dobo-demo-1.1 (&aux state)
  ( format t "Making a game state ...~%" )
  ( setf state ( make-instance 'state ) )
  ( format t "Displaying the game state ...~%" )
  ( display state ) 
  state
)
