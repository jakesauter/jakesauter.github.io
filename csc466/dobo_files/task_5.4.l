;; Task 5.4 -- The similar function
;
( defmethod similar ( ( s state ) ( entry state-probability-tuple ) )
  ( setf entry ( state-probability-tuple-game-state entry ) ) 
  ( or  
    ( equal-state entry s )
    ( equal-state entry ( horizontalReflection s ) )   
    ( equal-state entry ( verticalReflection s ) ) 
    ( equal-state entry ( horizontalReflection ( verticalReflection s ) ) )  
    ( equal-state entry ( rotation s 1 ) ) 
    ( equal-state entry ( rotation s 2 ) )  
    ( equal-state entry ( rotation s 3 ) ) 
  )   
)

;; helper function
;
( defmethod equal-state ( ( s1 state ) ( s2 state ) )
  ( setf board1 ( state-board s1 ) )
  ( setf board2 ( state-board s2 ) )
  ( let ( ( i 0 ) ( j 0 ) )
    ( loop while ( < j *SIZE* ) do
      ( setf i 0 )
      ( loop while ( < i *SIZE* ) do
        ( if ( not ( equal ( aref board1 i j ) ( aref board2 i j ) ) ) ( return-from equal-state nil ) )
        ( setf i ( + i 1 ) )
      )
      ( setf j ( + j 1 ) )
    )
  )
  T
)

( defun dobo-demo-5.4 ( &aux s1 s2 temp ) 
  ( setf s1 ( make-instance 'state ) )
  ( setf s2 ( make-instance 'state ) ) 
  ;Initializing s1
  ( set-row 0 '( 1 0 0 ) ( state-board s1 ) )
  ( set-row 1 '( 0 0 0 ) ( state-board s1 ) )
  ( set-row 2 '( 0 0 0 ) ( state-board s1 ) )
  ;Initializing s2
  ( set-row 0 '( 0 0 0 ) ( state-board s2 ) )
  ( set-row 1 '( 0 0 0 ) ( state-board s2 ) )
  ( set-row 2 '( 0 0 1 ) ( state-board s2 ) )
  ( format t "state 1: ~%" )
  ( display s1 )
  ( format t "state 2: ~%" )
  ( display s2 )
  ( setf spt ( make-instance 'state-probability-tuple :game-state s2 ) )
  ( format t "similar s1 s2: ~A~%~%" ( similar s1 spt ) )
  ( setf temp s1 )
  ( setf s1 s2 ) 
  ( setf s2 temp ) 
  ( format t "state 1: ~%" )
  ( display s1 )
  ( format t "state 2: ~%" )
  ( display s2 )
  ( setf spt ( make-instance 'state-probability-tuple :game-state s2 ) )
  ( format t "similar s1 s2: ~A " ( similar s1 spt ) )
)
