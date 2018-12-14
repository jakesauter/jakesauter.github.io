;; Task 5.5 -- the insert method, which tests each entry for simularity before adding
;
( defmethod insert ( ( gst game-state-table ) ( s state ) ( wins integer ) ( hits integer ) )
  ;special condition if the table is empty
  ( if ( eq ( length ( game-state-table-state-probability-tuple-list gst ) ) 0 ) 
       ( let ()  
         ( add gst s wins hits )
         ( return-from insert T )
       )
  )   
  ;check if the state matches any current state in the board
  ( setf found ( table-entry-lookup gst s ) )
  ( if found 
    ( let () 
      ;if found edit the entry 
      ( setf ( state-probability-tuple-hits found ) ( + ( state-probability-tuple-hits found ) hits ) )    
      ( setf ( state-probability-tuple-wins found ) ( + ( state-probability-tuple-wins found ) wins ) )    
      ( setf ( state-probability-tuple-probability found )
        ( float ( / ( state-probability-tuple-wins found ) ( state-probability-tuple-hits found ) ) )
      )
      ;;if its really low or high, bump it towards 1 or 0
      ( setf p ( state-probability-tuple-probability found ) )
      ( if ( not ( or ( = p 1 ) ( = p 0 ) ) )
        ( cond 
          ( ( > p .90 ) 
            ( setf ( state-probability-tuple-probability found ) 1 ) 
          )
          ( ( < p .1 )
            ( setf ( state-probability-tuple-probability found ) 0 ) 
          )
        )
      )
      ( return-from insert T )
    )
    ;if not found add the entry  
    ( let ()
      ( add gst s wins hits )
      ( return-from insert T ) 
    )
  )
) 

( defun dobo-demo-5.5 ( &aux gst )
  ( setf gst ( make-instance 'game-state-table ) )
  ( dotimes ( x 10 ) 
    ( setf ( game-state-table-state-probability-tuple-list gst )    
            ( push ( make-instance 'state-probability-tuple ) 
            ( game-state-table-state-probability-tuple-list gst ) )
    )
  ) 
  ( display gst )
  ( format t "~%~%calling insert ...~%~%" )
  ( setf s ( make-instance 'state ) )
  ( insert gst s 1 1 )
  ( display gst )
  gst 
)
