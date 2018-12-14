;;;; Task 4 -- Defining a Random Player, Implementing Game Functionality, and Defining a Human Player
;

;; Task 4.1 -- Defining a Random Player
( defclass random-machine-player ( player ) 
  ( 
    ( name :initform 'random-machine ) 
  ) 
)

( defmethod make-move ( ( p random-machine-player ) ( s state ) &rest report &aux move )
  ( setf report ( car report ) )
  ( if report ( format t "Begin ~A move ...~%" ( player-name p ) ) )
  ( if report ( display s ) )
  ( setf move ( nth ( random ( length pm-list ) ) pm-list ) )
  ( if report ( format t "radomly selecting ~A for my move ~%" move ) )
  ( apply-move move s )  
)

( defun dobo-demo-4.1 ( &aux player state )
  ( set-pm-list )
  ( format t "Making a random machine player ...~%" )
  ( setf player ( make-instance 'random-machine-player ) )
  ( format t "Making a starting game state ...~%" )
  ( display player )
  ( setf state ( make-instance 'state ) )
  ( dotimes ( i 3 )
    ( display state )
    ( setf state ( make-move player state t ) )
    ( display state ) 
  )
)

;; Task 4.2 -- Implementing Game Functionality
;
( defmethod generic-play ( ( p1 player ) ( p2 player ) &rest report &aux move game player )
  ( setf report ( car report ) ) 
  ( set-pm-list ) 
  ( setf game '() )
  ( if report ( format t "making initial state ...~%" ) ) 
  ( push ( make-instance 'state ) game )
  ( setf player p1 )
  ( loop while ( not ( empty-state-p ( car game ) ) ) do
    ( push ( make-move player ( car game ) report ) game ) 
    ( if ( equal player p1 ) ( setf player p2 ) ( setf player p1 ) ) 
  )
  ( if report ( display ( car game ) ) ) 
  ;;incriments wins and losses of players
  ( if ( equal player p1 )
    ( let ();player 1 won 
      ( setf ( player-wins p1 ) ( + 1 ( player-wins p1 ) ) ) 
      ( setf ( player-losses p2 ) ( + 1 ( player-losses p2 ) ) ) 
    )   
    ( let ();player 2 won 
      ( setf ( player-wins p2 ) ( + 1 ( player-wins p2 ) ) ) 
      ( setf ( player-losses p1 ) ( + 1 ( player-losses p1 ) ) )
    )
  )
  ( format t "The winner is . . . ~A ~%" ( if ( equal player p1 ) ( player-name p1 ) ( player-name p2 ) ) )
  ( make-instance 'game :state-list game )
)

( defun dobo-demo-4.2 (&aux player1 player2)
  ( format t "Making 2 random players ...~%" )
  ( setf player1 ( make-instance 'random-machine-player :name 'player1 ) )
  ( setf player2 ( make-instance 'random-machine-player :name 'player2 ) )
  ( format t "Pitting the players against eachother ...~%" )
  ( generic-play player1 player2 t )
)


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
  ( format t "~%Moves may be entered as a list of (row col) pairs or as an index in the pm-list~%~%" )
  ( generic-play player1 player2 t )
)

;; Task 4 all inclusive demo
( defun dobo-demo-4 () 
  ( format t "dobo-demo-4.1 returned ~A~%" ( dobo-demo-4.1 ) )
  ( format t "dobo-demo-4.2 returned ~A~%" ( dobo-demo-4.2 ) )
  ( format t "dobo-demo-4.3 returned ~A~%" ( dobo-demo-4.3 ) )
)

