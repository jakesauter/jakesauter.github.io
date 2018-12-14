;; Task 5.2 -- Modelling and adding entries to the game state table
;
( defclass game-state-table ()
  (
    ( state-probability-tuple-list 
      :accessor game-state-table-state-probability-tuple-list 
      :initarg :state-probability-tuple-list 
      :initform ()
    )
  )
)

( defmethod display ( ( gst game-state-table ) )
  ( dolist ( entry ( game-state-table-state-probability-tuple-list gst ) )
    ( format t "_____________________________~%" )    
    ( display entry )
  )
  ( if ( not ( null ( game-state-table-state-probability-tuple-list gst ) ) )
    ( format t "_____________________________~%" )
  )
  gst
)

;; Adding an entry to the game state table
;
( defmethod add ( ( gst game-state-table ) ( s state ) ( wins integer ) ( hits integer ) )
  ( setf spt ( make-instance 'state-probability-tuple :game-state s :wins wins :hits hits ) ) 
  ( setf ( state-probability-tuple-probability spt )
    ( float ( / ( state-probability-tuple-wins spt ) ( state-probability-tuple-hits spt ) ) )
  )
  ( setf ( game-state-table-state-probability-tuple-list gst )
         ( push spt ( game-state-table-state-probability-tuple-list gst ) ) 
  ) 
  T
)

( defun dobo-demo-5.2 ()
  ( format t "Empty game state table: ~%" )
  ( setf gst ( make-instance 'game-state-table ) )
  ( display gst )
  ( setf state ( make-instance 'state ) ) 
  ( format t "Adding the entry to the table ...~%" )
  ( add gst state 1 1 )
  ( format t "Updated game state table: ~%" )
  ( display gst ) 
)

