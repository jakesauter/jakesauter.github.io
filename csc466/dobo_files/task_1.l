;;;; Task 1 -- Model a State
;

;; Task 1.1 -- Modelling the state class
;
( defclass state ()
  ( ( board :accessor state-board :initarg :board :initform ( make-full-board ) ) )
)

( defmethod make-full-board ( &aux board )
  ( setf board ( make-list 2 :initial-element size ) )  
  ( setf board ( make-array board :initial-element 1 ) )
)

( defmethod display ( ( s state ) &aux board )
  ( terpri )
  ( setf board ( state-board s ) )
  ( let ( ( i 0 ) ( j 0 ) )
    ( loop while ( not ( = i ( array-dimension board 0 ) ) ) do 
      ( setf j 0 )
      ( loop while ( not ( = j ( array-dimension board 1 ) ) ) do 
         ( format t "  ~A" ( aref board i j ) )
         ( setf j ( + j 1 ) )
      )   
      ( terpri )
      ( setf i ( + i 1 ) )
    )
  )
  ( terpri )
)

;; A demo of making and displaying a state
;
( defun dobo-demo-1.1 (&aux state)
  ( format t "Making a game state ...~%" )
  ( setf state ( make-instance 'state ) )
  ( format t "Displaying the game state ...~%" )
  ( display state ) 
  state
)

;; Task 1.2 -- applying a move to a state
;           -- a move is defined as a list of points 
;
( defmethod apply-move ( ( move list ) ( s state ) &rest demo &aux board return-state )
  ( setf return-state ( copy-state s ) )
  ( setf board ( state-board return-state ) )
  ( dolist ( i move )
    ( setf ( aref board ( nth 0 i ) ( nth 1 i ) ) 0 ) 
  )
  ( if ( not demo ) ( filter-possible-moves move ) )
  return-state 
)

;; A demo of apply-move
;
( defun dobo-demo-1.2 ( &aux state move after-state )
  ( setf state ( dobo-demo-1.1 ) )
  ( setf move '( ( 1 1 ) ) )
  ( format t "Move ~A will be applied ...~%" move )
  ( setf after-state ( apply-move move state t ) )
  ( format t "Now displaying the modified game state ...~%" )
  ( display after-state ) 
  ( setf state ( dobo-demo-1.1 ) )
  ( setf move '( ( 0 0 ) ( 0 1 ) ( 0 2 ) ) )
  ( format t "Move ~A will be applied ...~%" move )
  ( setf after-state ( apply-move move state t ) )
  ( format t "Now displaying the modified game state ...~%" )
  ( display after-state ) 
  ( setf state ( dobo-demo-1.1 ) )
  ( setf move '( ( 0 0 ) ( 1 0 ) ( 2 0 ) ) )
  ( format t "Move ~A will be applied ...~%" move )
  ( setf after-state ( apply-move move state t ) )
  ( format t "Now displaying the modified game state ...~%" )
  ( display after-state ) 
  ( setf state ( dobo-demo-1.1 ) )
  ( setf move '( ( 1 1 ) ( 1 2 ) ) )
  ( format t "Move ~A will be applied ...~%" move )
  ( setf after-state ( apply-move move state t ) )
  ( format t "Now displaying the modified game state ...~%" )
  ( display after-state )  
)

;; A helper method to copy a state
;
( defmethod copy-state ( ( s state ) )
  ( make-instance 'state :board ( copy-board ( state-board s ) ) )
)

;; A helper method to copy a board
;
( defmethod copy-board ( ( board array ) &aux return-board )
  ( setf return-board ( make-array ( array-dimensions board ) ) )
  ( let ( ( i 0 ) ( j 0 ) )
    ( loop while ( not ( = i ( array-dimension board 0 ) ) ) do
      ( setf j 0 )
      ( loop while ( not ( = j ( array-dimension board 1 ) ) ) do
        ( setf ( aref return-board i j ) ( aref board i j ) )
        ( setf j ( + j 1 ) )
      )
      ( setf i ( + i 1 ) )
    )
  )
  return-board
)

;; A helper method to determine if a state is empty,
;  meaning no stones are present and no moves can 
;  be applied to the state
;
( defmethod empty-state-p ( ( s state ) )
  ( setf board ( state-board s ) )
  ( let ( ( i 0 ) ( j 0 ) )
    ( loop while ( not ( = i ( array-dimension board 0 ) ) ) do 
      ( setf j 0 )
      ( loop while ( not ( = j ( array-dimension board 1 ) ) ) do 
        ( if ( not ( equal ( aref board i j ) 0 ) ) ( return-from empty-state-p nil ) )
        ( setf j ( + j 1 ) )
      )    
      ( setf i ( + i 1 ) )
    )    
  )
  t
)

;; Task 1 all inclusive demo 
;
( defun dobo-demo-1 () 
  ( format t "dobo-demo-1.1 returned ~A~%" ( dobo-demo-1.1 ) )
  ( format t "dobo-demo-1.2 returned ~A~%" ( dobo-demo-1.2 ) )
)


