;; Task 4.3 -- Defining a Human Player
;

( defclass human-player ( player ) () )

( defmethod make-move ( ( p human-player ) ( s state ) &rest report &aux move ) 
  ( setf report ( car report ) ) 
  ( if report ( format t "Begin human player move ...~%" ) )
  ( setf invalid-move t )
  ( loop while invalid-move do 
    ( if report ( display s ) )
    ( if *show-pm-list* ( display-pm-list ) )
    ( format t "Please select a move: " )
    ( setf move ( read ) )
    ;if move is an integer, grab that location in pm-list
    ; otherwise we can apply the move 
    ( if ( integerp move )
        ( setf move ( nth ( - move 1 ) pm-list ) )
    )
    ( if ( not ( valid-move-p move ) )
      ( format t "Invalid move!~%" )
      ( setf invalid-move nil ) 
    )
  )
  ( setf after-state ( apply-move move s ) )
  ( if report ( format t "End human player move~%" ) )
  after-state
)

( defun valid-move-p ( move &aux found )
  ( setf found nil )
  ( dolist ( x pm-list ) 
    ( if ( equal-move-p move x )
      ( return-from valid-move-p t )
    )
  )
  nil
) 

( defun equal-move-p ( l1 l2 )
  ( if ( not ( equal ( length l1 ) ( length l2 ) ) )
    ( return-from equal-move-p nil )
  )
  ( dolist ( i l1 )
    ( member i l2 :test 'equal )
  )
  t
)

( defun dobo-demo-4.3 ( &aux player1 player2 )
  ( setf player1 ( make-instance 'human-player ) )
  ( setf player2 ( make-instance 'random-machine-player ) )
  ( setf input-choice-valid nil )
  ( loop while ( not input-choice-valid ) do
    ( format t "Please enter n for 3<=n<=10 for n x n board size: " )
    ( setf *size* ( read ) )
    ( if ( integerp *size* ) 
      ( if ( and ( < *size* 11 ) ( > *size* 2 ) )
        ( setf input-choice-valid t )  
        ( format t "Error: Please enter n for 3<=n<=10~%" )
      )
      ( format t "Error: Please enter an integer~%" )
    )
  ) 
  ( setf input-choice-valid nil )
  ( loop while ( not input-choice-valid ) do
    ( format t "Would you like to see the possible moves list?(y/n): " )
    ( setf *show-pm-list* ( read ) )
    ( if ( or ( equal *show-pm-list* 'y ) ( equal *show-pm-list* 'n ) ) 
      ( setf input-choice-valid t )  
      ( format t "Error: Please enter y or n~%" )
    )
  )
  ( if ( equal *show-pm-list* 'y )
    ( setf *show-pm-list* t )
    ( setf *show-pm-list* nil )
  )
  ( generic-play player1 player2 t )
)
