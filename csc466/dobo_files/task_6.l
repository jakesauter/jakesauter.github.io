;;;; Task 6 -- modelling a table-lookup-player
;

( defclass table-lookup-player ( player )
  (
    ( name :initform 'table-lookup-player )
    ( gst :accessor table-lookup-player-game-state-table :initarg :game-state-table :initform ( converge-gst 1000 ) )
  )
)

( defmethod make-move ( ( p table-lookup-player ) ( s state ) &rest report &aux move )
  ( setf report ( car report ) ) 
  ( setf gst ( table-lookup-player-game-state-table p ) )
  ( if report ( format t "Begin ~A move ...~%" ( player-name p ) ) )
  ( if report ( format t "searching database for best move ...~%" ) )
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
      ( push ( table-probability-lookup ( table-lookup-player-game-state-table p ) ( car possible-states ) gst ) 
        state-probability-list )
    )
  ) 
  ;reset pm-list because we had to try all possible moves
  ( setf pm-list pm-backup )
  ;find the index of the state with the highest probability, reference the move to remove all now
    ;impossible moves and return the chosen state
  ( setf max ( reduce #'max state-probability-list ) )
  ( if report ( format t "probability-list: ~A~%" state-probability-list ) )
  ( setf max-p ( position max state-probability-list ) )
  ( filter-possible-moves ( nth max-p possible-moves ) )
  ( if report ( format t "selecting ~A for my move ~%" ( nth max-p possible-moves ) ) )
  ( if report ( format t "max-p: ~A, max: ~A~%" max-p max ) )
  ( nth max-p possible-states )
)

( defun dobo-demo-6 ()
  ( set-pm-list )
  ( format t "Making a table lookup player ...~%" )
  ( setf gst ( make-instance 'game-state-table ) )
  ( setf player ( make-instance 'table-lookup-player :game-state-table gst ) )
  ( format t "Making a starting game state ...~%" )
  ( display player )
  ( setf state ( make-instance 'state ) )
  ( dotimes ( i 3 )
    ( display state )
    ( setf state ( make-move player state t ) )
    ( display state ) 
  )
)
