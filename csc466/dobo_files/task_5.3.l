;; Task 5.3 -- Performing transformations on game states 
;

( defmethod horizontalReflection ( ( s state ) &aux board )
  ( setf board ( state-board ( copy-state s ) ) )
  ( setf board ( state-board ( copy-state s ) ) )
  ( dotimes ( j *SIZE* )
    ( set-row j ( reverse ( get-row j board ) ) board ) 
  )
  ( make-instance 'state :board board ) 
)

( defmethod verticalReflection ( ( s state ) &aux board )
  ( setf board ( state-board ( copy-state s ) ) )
  ( dotimes ( j *SIZE* )
    ( set-col j ( reverse ( get-col j board ) ) board ) 
  )
  ( make-instance 'state :board board ) 
)

( defmethod rotation ( ( s state ) ( x integer ) &aux board nboard )
  ( setf board ( state-board s ) )
  ( setf nboard ( make-full-board ) )
  ( dotimes ( i x )
    ( dotimes ( j *SIZE* )
      ( set-row j ( reverse ( get-col j board ) ) nboard ) 
    )
    ( setf board ( copy-board nboard ) )
  )
  ( make-instance 'state :board nboard )
)

;; helper functions for 2D array functionality
;
( defmethod get-row ( ( r integer ) ( x array ) &aux row )
  ( setf row '() )
  ( let ( ( i 0 ) )
    ( loop while ( < i ( array-dimension x 1 ) ) do
      ( setf row ( snoc ( aref x r i ) row ) )
      ( setf i ( + i 1 ) )
    )
  )
  row 
)

( defmethod set-row ( ( r integer ) ( l list ) ( x array ) &aux row )
  ( let ( ( i 0 ) )
    ( loop while ( < i ( array-dimension x 1 ) ) do
      ( setf ( aref x r i ) ( nth i l ) ) 
      ( setf i ( + i 1 ) )
    )
  )
  x
)

( defmethod get-col ( ( c integer ) ( x array ) &aux col )
  ( setf col '() )
  ( let ( ( i 0 ) )
    ( loop while ( < i ( array-dimension x 0 ) ) do
      ( setf col ( snoc ( aref x i c ) col ) )
      ( setf i ( + i 1 ) )
    )
  )
  col
)

( defmethod set-col ( ( c integer ) ( l list ) ( x array ) &aux col )
  ( let ( ( i 0 ) )
    ( loop while ( < i ( array-dimension x 0 ) ) do
      ( setf ( aref x i c ) ( nth i l ) ) 
      ( setf i ( + i 1 ) )
    )
  )
  x 
)

( defun dobo-demo-5.3 ( &aux s )
  ( setf *size* 3 )
  ( setf s ( make-instance 'state ) )
  ( set-row 0 '( 1 0 0 ) ( state-board s ) )
  ( set-row 1 '( 0 0 0 ) ( state-board s ) )
  ( set-row 2 '( 0 0 1 ) ( state-board s ) )
  ( format t "Original State: ~%" )
  ( display s ) 
  ( format t "Horizontal reflection of the state: ~%")
  ( display ( horizontalReflection s ) )
  ( format t "Vertical reflection of the state: ~%" )
  ( display ( verticalReflection s ) )
  ( format t "Single rotation of the state: ~%" ) 
  ( display ( rotation s 1 ) )
  ( format t "Double rotation of the state: ~%" )
  ( display ( rotation s 2 ) )
  ( format t "Triple rotation of the state: ~%" )
  ( display ( rotation s 3 ) )
)

