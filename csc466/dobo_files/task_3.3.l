;; Task 3.3 -- filtering the possible moves list after a move 
;
( defun filter-possible-moves ( move )
  ( dolist ( i move )
    ( dolist ( j pm-list )
      ( dolist ( x j )
        ( if ( equal i x ) ( setf pm-list ( remove j pm-list ) ) )
      )
    )
  )
  nil
)

( defun dobo-demo-3.3 (&aux state move)
  ( set-pm-list )
  ( display-pm-list )
  ( setf state ( make-instance 'state ) )
  ( dotimes ( i 5 )
    ( display state )
    ( setf move ( nth ( random ( length pm-list ) ) pm-list ) ) 
    ( format t "The move that will be applied is ~A~%" move )
    ( setf state ( apply-move move state ) )
    ( display state )
    ( format t "Eliminating now impossible moves from the possbile moves list ...~%" )
    ( filter-possible-moves move )
    ( display-pm-list )
  )
)

