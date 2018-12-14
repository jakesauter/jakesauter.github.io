;; Task 5.6 -- Retrieval 

( defmethod table-entry-lookup ( ( gst game-state-table ) ( s state ) &rest training )
  ( dolist ( entry ( game-state-table-state-probability-tuple-list gst ) ) 
    ( if ( similar s entry ) ( return-from table-entry-lookup entry ) )
  )
  ;if the move is not in the table, its initial learning rate is .25
  nil
)

( defmethod table-probability-lookup ( ( gst game-state-table ) ( s state ) &rest training )
  ( if ( setf found ( table-entry-lookup gst s ) )
    ( return-from table-probability-lookup ( state-probability-tuple-probability found ) )
  )
  ;The game state table player will explore new states will training but now while playing
  ( if training 1.1 0 ) 
)

( defmethod dobo-demo-5.6 ( &aux gst )
  ( setf gst ( dobo-demo-5.5 ) )
  ( setf s ( make-instance 'state ) )
  ( set-row 0 '( 1 1 1 ) ( state-board s ) ) 
  ( set-row 1 '( 1 1 1 ) ( state-board s ) ) 
  ( set-row 2 '( 1 1 1 ) ( state-board s ) ) 
  ( format t "~%~%Searching for probability of:~%" )
  ( display s ) 
  ( format t "~%found probability: ~A~%" ( table-probability-lookup gst s ) )
)
