;; Task 11: Modeling a Neural Network Player 
;

( defclass neural-net-player ( player )
  (
    ( name :initform 'neural-net-player )
    ( nn :accessor neural-net-player-neural-net :initarg :neural-net :initform nil )
  )
)

( defmethod make-move ( ( p neural-net-player ) ( s state ) &optional demo &aux move )
  ( setf nn ( neural-net-player-neural-net p ) )
  ( if demo ( format t "Begin ~A move ...~%" ( player-name p ) ) )
  ( if demo ( format t "searching database for best move ...~%" ) )
  ;three lists will be used in correlation, lists with index correlating to a move,
    ;the state after the application of that move and the probability found in the
    ;gst for that state
  ( setf possible-moves () )
  ( setf possible-states () )
  ( setf state-probability-list () )
  ;generate all probabilies of all possible states from possible moves
  ( setf pm-backup ( copy-list pm-list ) )
  ( dolist ( move pm-list )
    ( setf possible-moves ( push move possible-moves ) )
    ( setf possible-states ( push ( apply-move move s ) possible-states ) )
    ( setf state-probability-list
      ( push ( forward-prop nn ( car possible-states ) ) 
        state-probability-list )
    )
  ) 
  ;reset pm-list because we had to try all possible moves
  ( setf pm-list pm-backup )
  ;find the index of the state with the highest probability, reference the move to remove all now
    ;impossible moves and return the chosen state
  ( setf max ( reduce #'max state-probability-list ) )
  ( if demo ( format t "probability-list: ~A~%" state-probability-list ) )
  ( setf max-p ( position max state-probability-list ) )
  ( filter-possible-moves ( nth max-p possible-moves ) )
  ( if demo ( format t "selecting ~A for my move ~%" ( nth max-p possible-moves ) ) )
  ( if demo ( format t "max-p: ~A, max: ~A~%" max-p max ) )
  ( nth max-p possible-states )
) 

( defun dobo-demo-11 ( &aux nn player state )
  ( set-pm-list )
  ( format t "Making a neural net player ...~%" )
  ( setf nn ( init-neural-net '( 9 15 7 1 ) "nn_9-15-7-1.txt" ) )
  ( setf player ( make-instance 'neural-net-player :neural-net nn ) )
  ( format t "Making a starting game state ...~%" )
  ( display player )
  ( setf state ( make-instance 'state ) )
  ( dotimes ( i 3 )
    ( display state )
    ( setf state ( make-move player state t ) )
    ( display state ) 
  )
)
