;;;; Task 5 -- Modelling the Game State Table
;

;; Task 5.1 -- Modelling a state-probability tuple, which
;              will be an entry in the game state table
;
( defclass state-probability-tuple ()
  (
    ( game-state :accessor state-probability-tuple-game-state :initarg :game-state :initform ( make-instance 'state ) )
    ( probability :accessor state-probability-tuple-probability :initarg :probability :initform 0 )  
    ( wins :accessor state-probability-tuple-wins :initarg :wins :initform 0 )
    ( hits :accessor state-probability-tuple-hits :initarg :hits :initform 0 )
  )
)

( defmethod display ( ( spt state-probability-tuple ) )  
  ( format t "state: " )
  ( display ( state-probability-tuple-game-state spt ) )  
  ( format t "wins: ~A~%" ( state-probability-tuple-wins spt ) )
  ( format t "hits: ~A~%" ( state-probability-tuple-hits spt ) )
  ( format t "probability: ~A~%" ( state-probability-tuple-probability spt ) )
  spt  
)

( defun dobo-demo-5.1 ()
  ( format t "Making an instance of a state-probability tuple ...~%" )
  ( display ( make-instance 'state-probability-tuple ) )

  ( setf s ( make-instance 'state ) )

  ( format t "~%Making an instance of a state-probability tuple ...~%" )
  ( set-col 0 '( 0 0 0 ) ( state-board s ) )
  ( display ( make-instance 'state-probability-tuple :game-state s ) )

  ( format t "~%Making an instance of a state-probability tuple ...~%" )
  ( set-row 2 '( 1 1 0 ) ( state-board s ) )
  ( display ( make-instance 'state-probability-tuple :game-state s :wins 1 :hits 1) )

  ( format t "~%Making an instance of a state-probability tuple ...~%" )
  ( set-row 1 '( 0 0 0 ) ( state-board s ) )
  ( display ( make-instance 'state-probability-tuple :game-state s :wins 5 :hits 15 :probability ( float ( / 5 15 ) ) ) )

  ( format t "~%Making an instance of a state-probability tuple ...~%" :wins 1 :hits 200 :probability ( float ( / 1 200 ) ) )
  ( set-col 2 '( 1 1 1 ) ( state-board s ) )
  ( display ( make-instance 'state-probability-tuple :game-state s :wins 4 :hits 7 :probability ( float ( / 4 7 ) ) ) )
)

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

;; Task 5 all inclusive demo
( defun dobo-demo-5 () 
  ( format t "dobo-demo-5.1 returned ~A~%" ( dobo-demo-5.1 ) )
  ( format t "dobo-demo-5.2 returned ~A~%" ( dobo-demo-5.2 ) )
  ( format t "dobo-demo-5.3 returned ~A~%" ( dobo-demo-5.3 ) )
  ( format t "dobo-demo-5.4 returned ~A~%" ( dobo-demo-5.4 ) )
  ( format t "dobo-demo-5.5 returned ~A~%" ( dobo-demo-5.5 ) )
  ( format t "dobo-demo-5.6 returned ~A~%" ( dobo-demo-5.6 ) )
)

