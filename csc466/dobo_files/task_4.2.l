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

