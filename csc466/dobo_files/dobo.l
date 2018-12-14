( load "lp.l" )
( setf *FILE* "gst_3x3.txt" )

;-------------------------------------------
;;The size of each dimention of a 2D board
;
( setf *SIZE* 3 )
;-------------------------------------------

;;;; Task 1 -- Model a State
;

;; Task 1.1 -- Modelling the state class
;
( defclass state ()
  ( ( board :accessor state-board :initarg :board :initform ( make-full-board ) ) )
)

( defmethod make-full-board ( &aux board )
  ( setf board ( make-list 2 :initial-element *SIZE* ) )  
  ( setf board ( make-array board :initial-element 1 ) )
)

( defmethod display ( ( s state ) &aux board )
  ( terpri )
  ( setf board ( state-board s ) )
  ( dotimes ( i ( array-dimension board 0 ) ) 
    ( dotimes ( j ( array-dimension board 1 ) ) 
       ( format t "  ~A" ( aref board i j ) )
    )   
    ( terpri )
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

;;;; Task 2 -- Model a Player
; The root of the player hierarchy

( defclass player ()
  ( 
    ( name :accessor player-name :initarg :name :initform 'john-doe ) 
    ( wins :accessor player-wins :initarg :wins :initform 0 )
    ( losses :accessor player-losses :initarg :losses :initform 0 )
  )
)

( defmethod display ( ( p player  ) )
  ( terpri )
  ( format t "Name: ~A~%" ( player-name p ) )
  ( format t "Wins: ~A~%" ( player-wins p ) )
  ( format t "Losses: ~A~%" ( player-losses p ) )
  ( if ( > ( + ( player-wins p ) ( player-losses p ) ) 0 )
    ( let () 
      ( format t "Win Ratio: ~A~%~%" 
        ( float ( / ( player-wins p ) ( + ( player-wins p ) ( player-losses p ) ) ) ) 
      )
    )
    ( format t "Win Ratio: Undetermined~%~%" )
  )
)

( defun dobo-demo-2 (&aux gen-player) 
  ( format t "Making a generic player ...~%" )
  ( setf gen-player ( make-instance 'player ) )
  ( format t "Displaying the generic player ...~%" )
  ( display gen-player )
  ( format t "Changing generic player's name ...~%" )
  ( setf ( player-name gen-player ) 'Billy-Joe ) 
  ( format t "Displaying the generic player ...~%" )
  ( display gen-player )
  ( format t "The player won 5 out of 5 games played and has a perfect win ratio ...~%" )
  ( setf ( player-wins gen-player ) 5 )
  ( format t "Displaying the generic player ...~%" )
  ( display gen-player )
  ( format t "The player lost the next 3 games ...~%" )
  ( setf ( player-losses gen-player ) 3 )
  ( format t "Displaying the generic player ...~%" )
  ( display gen-player )
  gen-player
)

;;;; Task 3 -- Possible Moves List, Modelling a game and filtering possible moves
;

;; Task 3.1 -- making the possible moves list
;            - a move is a list of points 
;  ;Algorithm to generate all possible moves for an nxn dimesional board:
;     ;Generating all horizontal moves
;     -loop controlling row
;       -loop controlling line size
;         -loop controlling start position
;          -conditional to check if col-size is greater than or equal to line size - start position
;          -generate move
;     ;Generating all vertical Moves 
;     -loop controlling col
;       -loop controlling line size
;         -loop controlling start position
;           -conditional to check if row-size is greater than or equal to line size - start position
;            -generate move
;
( defmethod set-pm-list ()
  ;Generating all Horizontal Moves 
  ( setf h-possible-moves '() )
  ( let ( ( row 0 ) ( line-size *SIZE* ) ( start-pos 0 ) ( move '() ) ( i 0 ) ) 
    ( loop while ( < row *SIZE* ) do
      ( setf line-size 1 )
      ( loop while ( not ( > line-size *SIZE* ) ) do 
        ( setf start-pos 0 )
        ( loop while ( < start-pos *SIZE* ) do
          ( if ( < *SIZE* ( + line-size start-pos ) )
            ( return )
          )
          ;add this move to the list 
          ( setf col start-pos )
          ( setf i line-size )
          ( setf move '() )
          ( loop while ( > i 0 ) do
            ( setf move ( snoc ( list row col ) move ) )
            ( setf i ( - i 1 ) )
            ( setf col ( + col 1 ) )
          )
          ( setf h-possible-moves ( snoc move h-possible-moves ) )
          ( setf start-pos ( + start-pos 1 ) )
        )
        ( setf line-size ( + line-size 1 ) )
      )
      ( setf row ( + row 1 ) )
    )
  )
  ;Generating all Vertical Moves 
  ( setf v-possible-moves '() )
  ( let ( ( col 0 ) ( line-size *SIZE* ) ( start-pos 0 ) ( move '() ) ( i 0 ) ) 
    ( loop while ( < col *SIZE* ) do
      ( setf line-size 1 )
      ( loop while ( not ( > line-size *SIZE* ) ) do 
        ( setf start-pos 0 )
        ( loop while ( < start-pos *SIZE* ) do
          ( if ( < *SIZE* ( + line-size start-pos ) )
            ( return )
          )
          ;add this move to the list 
          ( setf row start-pos )
          ( setf i line-size )
          ( setf move '() )
          ( loop while ( > i 0 ) do
            ( setf move ( snoc ( list row col ) move ) )
            ( setf i ( - i 1 ) )
            ( setf row ( + row 1 ) )
          )
          ( setf v-possible-moves ( snoc move v-possible-moves ) )
          ( setf start-pos ( + start-pos 1 ) )
        )
        ( setf line-size ( + line-size 1 ) )
      )
      ( setf col ( + col 1 ) )
    )
  )
  ; Removing all duplicates and combining lists
  ( dolist ( i h-possible-moves )
    ( dolist ( j v-possible-moves )
      ( if ( equal i j ) ( setf v-possible-moves ( remove j v-possible-moves ) ) )
    )
  )
  ( setf pm-list ( append h-possible-moves v-possible-moves ) )
)

;; function to dipslay the possible moves list
;  one move per line, and numbering to help with
;   move selection 
;
( defun display-pm-list ()
  ( format t "~%~%------------------------PM-LIST--------------------------~%" )
  ( let ( ( count 1 ) )
    ( dolist ( x pm-list )
      ( format t "~A: ~A~%" count x )
      ( setf count ( + count 1 ) )
    )
  )
  ( format t "---------------------------------------------------------~%~%" )
)
 
( defun dobo-demo-3.1 ()
  ( set-pm-list )
  ( format t "~%possible Moves List for ~A x ~A board: ~%" *SIZE* *SIZE* )
  ( display-pm-list )
  ( setf *SIZE* 4 )
  ( set-pm-list )
  ( format t "Possible Moves List for ~A x ~A board: ~%~%" *SIZE* *SIZE* )
  ( display-pm-list )
  ( setf *SIZE* 5 )
  ( set-pm-list )
  ( format t "Possible Moves List for ~A x ~A board: ~%" *SIZE* *SIZE* )
  ( display-pm-list )
  ( setf *SIZE* 3 )
)

;; Task 3.2 -- defining a game 
;
( defclass game ()
  ( ( state-list :accessor game-state-list :initarg :state-list :initform '() ) )
)

( defmethod display ( ( g game ) )
  ( terpri )
  ( dolist ( state ( game-state-list g ) )
    ( display state )
  )
)

( defun dobo-demo-3.2 ( &aux game state )
  ( format t "Making an instance of a game ...~%" )
  ( setf game ( make-instance 'game ) )
  ( format t "Manually adding a few states to the game ...~%" )
  ( setf state ( make-instance 'state ) )
  ( setf ( game-state-list game ) ( list state state state state ) )
  ( format t "Displaying the game ...~%" )
  ( display game )
)


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

;; Task 3 all inclusive demo
;
( defun dobo-demo-3 () 
  ( format t "dobo-demo-3.1 returned ~A~%" ( dobo-demo-3.1 ) )
  ( format t "dobo-demo-3.2 returned ~A~%" ( dobo-demo-3.2 ) )
  ( format t "dobo-demo-3.3 returned ~A~%" ( dobo-demo-3.3 ) )
)

;;;; Task 4 -- Defining a Random Player, Implementing Game Functionality, and Defining a Human Player
;

;; Task 4.1 -- Defining a Random Player
( defclass random-machine-player ( player ) 
  ( 
    ( name :initform 'random-machine ) 
  ) 
)

( defmethod make-move ( ( p random-machine-player ) ( s state ) &optional demo &aux move )
  ( if demo ( format t "Begin ~A move ...~%" ( player-name p ) ) )
  ( if demo ( display s ) )
  ( setf move ( nth ( random ( length pm-list ) ) pm-list ) )
  ( if demo ( format t "radomly selecting ~A for my move ~%" move ) )
  ( if demo ( format t "End random machine move~%~%" ) )
  ( apply-move move s )  
)

( defun dobo-demo-4.1 ( &aux player state )
  ( set-pm-list )
  ( format t "Making a random machine player ...~%" )
  ( setf player ( make-instance 'random-machine-player ) )
  ( format t "Making a starting game state ...~%" )
  ( display player )
  ( setf state ( make-instance 'state ) )
  ( dotimes ( i 3 )
    ( display state )
    ( setf state ( make-move player state t ) )
    ( display state ) 
  )
)

;; Task 4.2 -- Implementing Game Functionality
;
( defmethod generic-play ( ( p1 player ) ( p2 player ) &optional report &aux move game player )
  ( set-pm-list ) 
  ( setf game '() )
  ( if report ( format t "making initial state ...~%" ) ) 
  ( push ( make-instance 'state ) game )
  ( setf player p1 )
  ( loop while ( not ( empty-state-p ( car game ) ) ) do
    ( push ( make-move player ( car game ) report ) game ) 
    ( if ( equal player p1 ) ( setf player p2 ) ( setf player p1 ) ) 
  )
  ( if report ( display ( car game ) ) ) 
  ;;incriments wins and losses of players
  ( if ( equal player p1 )
    ( let ();player 1 won 
      ( setf ( player-wins p1 ) ( + 1 ( player-wins p1 ) ) ) 
      ( setf ( player-losses p2 ) ( + 1 ( player-losses p2 ) ) ) 
    )   
    ( let ();player 2 won 
      ( setf ( player-wins p2 ) ( + 1 ( player-wins p2 ) ) ) 
      ( setf ( player-losses p1 ) ( + 1 ( player-losses p1 ) ) )
    )
  )
  ( format t "The winner is . . . ~A ~%" ( if ( equal player p1 ) ( player-name p1 ) ( player-name p2 ) ) )
  ( make-instance 'game :state-list game )
)

( defun dobo-demo-4.2 (&aux player1 player2)
  ( format t "Making 2 random players ...~%" )
  ( setf player1 ( make-instance 'random-machine-player :name 'player1 ) )
  ( setf player2 ( make-instance 'random-machine-player :name 'player2 ) )
  ( format t "Pitting the players against eachother ...~%" )
  ( generic-play player1 player2 t )
)

( setf *show-pm-list* nil )

;; Task 4.3 -- Defining a Human Player
;

( defclass human-player ( player ) () )

( defmethod make-move ( ( p human-player ) ( s state ) &optional demo &aux move ) 
  ( if demo ( format t "Begin human player move ...~%" ) )
  ( setf invalid-move t )
  ( loop while invalid-move do 
    ( if demo ( display s ) )
    ( if *show-pm-list* ( display-pm-list ) )
    ( format t "Please select a move: " )
    ( setf move ( read ) )
    ;if move is an integer, grab that location in pm-list
    ; otherwise we can apply the move 
    ( if ( integerp move )
        ( setf move ( nth ( - move 1 ) pm-list ) )
    )
    ( if ( listp move )
      ( if ( not ( valid-move-p move ) ) 
        ( format t "Invalid move!~%" )
        ( setf invalid-move nil ) 
      )
      ( format t "~%Please enter a list of points or an index of the pm-list!~%~%" )
    )
  )
  ( setf after-state ( apply-move move s ) )
  ( if demo ( format t "End human player move~%~%" ) )
  after-state
)

( defun valid-move-p ( move &aux found )
  ( setf found nil )
  ( dolist ( x pm-list ) 
    ( if ( equal-move-p move x )
      ( return-from valid-move-p t )
    )
  )
  nil
) 

( defun equal-move-p ( l1 l2 )
  ( if ( not ( equal ( length l1 ) ( length l2 ) ) )
    ( return-from equal-move-p nil )
  )
  ( dolist ( i l1 )
    ( member i l2 :test 'equal )
  )
  t
)

( defun dobo-demo-4.3 ( &aux player1 player2 )
  ( setf player1 ( make-instance 'human-player ) )
  ( setf player2 ( make-instance 'random-machine-player ) )
  ( setf input-choice-valid nil )
  ( loop while ( not input-choice-valid ) do
    ( format t "Please enter n for 3<=n<=10 for n x n board size: " )
    ( setf *size* ( read ) )
    ( if ( integerp *size* ) 
      ( if ( and ( < *size* 11 ) ( > *size* 2 ) )
        ( setf input-choice-valid t )  
        ( format t "Error: Please enter n for 3<=n<=10~%" )
      )
      ( format t "Error: Please enter an integer~%" )
    )
  ) 
  ( setf input-choice-valid nil )
  ( loop while ( not input-choice-valid ) do
    ( format t "Would you like to see the possible moves list?(y/n): " )
    ( setf *show-pm-list* ( read ) )
    ( if ( or ( equal *show-pm-list* 'y ) ( equal *show-pm-list* 'n ) ) 
      ( setf input-choice-valid t )  
      ( format t "Error: Please enter y or n~%" )
    )
  )
  ( if ( equal *show-pm-list* 'y )
    ( setf *show-pm-list* t )
    ( setf *show-pm-list* nil )
  )
  ( format t "~%Moves may be entered as a list of (row col) pairs or as an index in the pm-list~%~%" )
  ( generic-play player1 player2 t )
)

;; Task 4 all inclusive demo
( defun dobo-demo-4 () 
  ( format t "dobo-demo-4.1 returned ~A~%" ( dobo-demo-4.1 ) )
  ( format t "dobo-demo-4.2 returned ~A~%" ( dobo-demo-4.2 ) )
  ( format t "dobo-demo-4.3 returned ~A~%" ( dobo-demo-4.3 ) )
)

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

( defun dobo-demo-5.6 ( &aux gst )
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

;;;; Task 6 -- modelling a table-lookup-player
;

( defclass table-lookup-player ( player )
  (
    ( name :initform 'table-lookup-player )
    ( gst :accessor table-lookup-player-game-state-table :initarg :game-state-table :initform ( converge-gst 1000 ) )
  )
)

( defmethod make-move ( ( p table-lookup-player ) ( s state ) &optional demo &aux move )
  ( setf gst ( table-lookup-player-game-state-table p ) )
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
      ( push ( table-probability-lookup ( table-lookup-player-game-state-table p ) ( car possible-states ) gst ) 
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
  )
)
;;;; Task 7 -- Converge the probability values of the game table
;;

( defmethod converge-random-vs-lookup ( ( gst game-state-table ) ( n integer ) )
  ( setf player1 ( make-instance 'random-machine-player ) )
  ( setf player2 ( make-instance 'table-lookup-player :game-state-table gst ) )
  ( converge player1 player2 gst n )
)

( defmethod converge ( ( player1 player ) ( player2 player ) ( gst game-state-table ) ( n integer ) )
  ( setf analyze nil )
  ( dotimes ( i n ) 
    ( write-gst-training-stats-vs-time player1 player2 i )
    ( format t  "game ~A: " i )
    ( setf game ( generic-play player1 player2 nil gst ) ) 
    ( if ( equal ( player-name player1 ) 'table-lookup-player ) 
      ( if ( = ( mod ( length ( game-state-list game ) ) 2 ) 0 )
        ( setf analyze t ) 
      )
      ( if ( = ( mod ( length ( game-state-list game ) ) 2 ) 1 )
        ( setf analyze t ) 
      )
    ) 
    ;now to analyze the game
    ( setf training-n 1 )
    ( if ( not ( equal ( game-state-table-state-probability-tuple-list gst ) '() ) )
      ( setf training-n ( ceiling ( / ( state-probability-tuple-hits 
        ( rac ( game-state-table-state-probability-tuple-list gst ) ) ) 100 ) )
      )
    )
    ( if analyze
      ( let ( ( win 0 ) )
        ( dotimes ( j ( - ( length ( game-state-list game ) ) 1 ) )
          ( setf s ( nth j ( game-state-list game ) ) )
          ( insert gst s ( * training-n win ) training-n ) 
          ( if ( = win 1 ) ( setf win 0 ) ( setf win 1 ) )
        )
        ( setf analyze nil )
      )
    )
    ( if ( = ( mod i 10 ) 0 )
      ( let ()
        ( format t "Player Switch~%" )
        ( setf temp player1 )
        ( setf player1 player2 )
        ( setf player2 temp )
        ( write-gst-to-file gst )
      )
    )
  ) 
  ( write-gst-to-file gst )
  ( format t "Final GST: ~%" )
  ( display gst ) 
  gst 
)

( defun dobo-demo-7 ( )
  ( setf gst ( make-instance 'game-state-table ) )
  ( format t "GST is empty~%" )
  ( display gst )
  ( format t " converging with 100 games, displaying every 10 games ...~%" )
  ( setf gst ( converge-random-vs-lookup gst 10 ) )
  ( format t "GST after 10 training games ...~%" )
  ( display gst )
  ( setf gst ( converge-random-vs-lookup gst 10 ) )
  ( format t "GST after 20 training games ...~%" )
  ( display gst )
  ( setf gst ( converge-random-vs-lookup gst 10 ) )
  ( format t "GST after 30 training games ...~%" )
  ( display gst )
  ( setf gst ( converge-random-vs-lookup gst 10 ) )
  ( format t "GST after 40 training games ...~%" )
  ( display gst )
  ( setf gst ( converge-random-vs-lookup gst 10 ) )
  ( format t "GST after 50 training games ...~%" )
  ( display gst )
  ( setf gst ( converge-random-vs-lookup gst 10 ) )
  ( format t "GST after 60 training games ...~%" )
  ( display gst )
  ( setf gst ( converge-random-vs-lookup gst 10 ) )
  ( format t "GST after 70 training games ...~%" )
  ( display gst )
  ( setf gst ( converge-random-vs-lookup gst 10 ) )
  ( format t "GST after 80 training games ...~%" )
  ( display gst )
  ( setf gst ( converge-random-vs-lookup gst 10 ) )
  ( format t "GST after 90 training games ...~%" )
  ( display gst )
  ( setf gst ( converge-random-vs-lookup gst 10 ) )
  ( format t "GST after 100 training games ...~%" )
  ( display gst )
)

;; Method to write stats to a file to see steady state 
;
( defmethod write-gst-training-stats-vs-time ( ( p1 player ) ( p2 player ) ( n integer ) )
  ( with-open-file ( stream "stats.csv" :direction :output :if-exists :supersede :if-does-not-exist :create )
    ( if ( not ( equal ( player-name p1 ) 'table-lookup-player ) )
      ( setf p1 p2 )  
    )
    ;writing stats to file   
    ( handler-case 
      ( setf ratio ( float ( / ( player-wins p1 ) ( + ( player-losses p1 ) ( player-wins p1 ) ) ) ) )
      ( division-by-zero () 
        ( setf ratio 0 ) 
      )
    )
    ( format stream "~A , ~A~%" n ratio ) 
  )
)


;; Method to save the converged gst to a file
;
( defmethod write-gst-to-file ( ( gst game-state-table ) &aux board state ) 
  ( format t "Writing gst to ~A~%" *FILE* )
  ( with-open-file ( stream *FILE* :direction :output )
    ( dolist ( entry ( game-state-table-state-probability-tuple-list gst ) )
      ( setf board '() )
      ( setf state ( state-probability-tuple-game-state entry ) )
      ( dotimes ( i *SIZE* ) 
        ( setf board ( snoc ( get-row i ( state-board state ) ) board ) )
      ) 
      ( format stream "~A ~A ~A ~A ~A" *SIZE* board ( state-probability-tuple-wins entry )
            ( state-probability-tuple-hits entry ) ( state-probability-tuple-probability entry )
      )
      (terpri stream)
    )
  )
)

;; Method to read the gst in from a file
;
( defmethod read-gst-from-file ( &aux gst entry-list )
  ( setf entry-list '() )
  ( let ( ( in (open *FILE* :if-does-not-exist nil ) ) )
     ( when in
        ( loop for line = ( read-line in nil ) 
          while line do 
            ( with-input-from-string ( s line ) 
              ;fist in line will be size of the board
              ( setf *SIZE* ( read s ) )
              ( setf board ( read s ) )
              ( setf entry ( list board ( read s ) ( read s ) ( read s ) ) )
              ( setf entry-list ( snoc entry entry-list ) )
            )
        )
        (close in)
     )
     ( setf e entry )
  )
  ( setf gst ( make-instance 'game-state-table ) )
  ( dolist ( e entry-list )
    ;now to parse the entry-list into a game-state-table
    ( setf state ( make-instance 'state ) )
    ( setf start-pos 0 )
    ( setf board ( car e ) )
    ( dotimes ( i *SIZE* ) 
      ( set-row i ( nth i board ) ( state-board state ) ) 
      ( setf start-pos ( + start-pos *SIZE* ) )
    )
    ( setf entry ( make-instance 'state-probability-tuple ) )
    ( setf ( state-probability-tuple-game-state entry ) state )
    ( setf ( state-probability-tuple-wins entry ) ( nth 1 e ) ) 
    ( setf ( state-probability-tuple-hits entry ) ( nth 2 e ) ) 
    ( setf ( state-probability-tuple-probability entry ) ( nth 3 e ) ) 
    ( setf ( game-state-table-state-probability-tuple-list gst ) 
           ( snoc entry ( game-state-table-state-probability-tuple-list gst ) )
    )
  )
  gst
)


;; Method to provide statistics on how well a player using
;   a game state table can fair against a random machine player
;
( defun table-vs-random-player-stats ( gst n )
  ;( format t "table player going first ... ~%" )
  ( setf player1 ( make-instance 'random-machine-player ) )
  ( setf player2 ( make-instance 'table-lookup-player :game-state-table gst ) )
  ( dotimes ( i n )
    ( setf game ( generic-play player1 player2 nil nil ) )
    ;display the game if the table player lost    
    ( if ( equal ( player-name player1 ) 'table-lookup-player ) 
      ( if ( = ( mod ( length ( game-state-list game ) ) 2 ) 0 )
        ( display game )
      )
      ( if ( = ( mod ( length ( game-state-list game ) ) 2 ) 1 )
        ( display game )
      )
    ) 
    ( if ( = ( mod i ( / n 2 ) ) 0 )
      ( let ()
        ( format t "Player switch ~%" )
        ( setf temp player1 ) 
        ( setf player1 player2 )
        ( setf player2 temp )
      )
    )
  )
  ( display player1 )
  ( display player2 )
)


;;;; Task 8 -- Implementing Necessary Matrix/Vector Functionality
; matrix transpose
; matrix multiplication
; input parameterize
; init-neural-net that makes a dynamic layered net
;

;; Method to copy an array 
;
( defmethod copy-array ( ( x array ) )
  ( setf return-array ( make-array ( array-dimensions x ) ) )
  ( dotimes ( i ( array-dimension x 0 ) )
    ( dotimes ( j ( array-dimension x 1 ) )
      ( setf ( aref return-array i j ) ( aref x i j ) )
    )
  )
  return-array
)

;; Method to apply the sigmoid function to 
; a single floating point number
;
( defmethod sigmoid ( ( x float ) )
  ( handler-case 
    ( / 1 ( + 1 ( exp ( - x ) ) ) ) 
    ( floating-point-underflow () 
      0
    )
    ( floating-point-overflow ()
      1
    )
  )
)

;; Method to apply the sigmoid function
; to every element in an array 
;
( defmethod sigmoid ( ( x array ) )
  ( setf nx ( copy-array x ) )
  ( dotimes ( i ( array-dimension x 0 ) )
    ( dotimes ( j ( array-dimension x 1 ) )
      ( setf input ( float ( aref x i j ) ) )   
      ( setf ( aref nx i j ) ( sigmoid input ) )  
    )    
  )
  nx
)

;; Method to dipslay an array
;
( defmethod display ( ( x array ) )
  ( terpri )
  ( dotimes ( i ( array-dimension x 0 ) )
    ( dotimes ( j ( array-dimension x 1 ) )
      ( format t "  ~A" ( aref x i j ) )
    )
    ( terpri )
  )
  ( terpri )
)

;; Method to apply a matrix transpose
;
( defmethod transpose ( ( x array ) )
  ( setf nx ( make-array ( list ( array-dimension x 1 ) ( array-dimension x 0 ) ) ) )
  ( dotimes ( i ( array-dimension x 1 ) )
    ( set-row i ( reverse ( get-col i x ) ) nx )
  ) 
  nx
)

;; Method to sum all elements in a list
;
( defmethod sum-list ( ( l list ) )
  ( cond
    ( ( null l )
      0
    )
    ( t 
      ( + ( car l ) ( sum-list ( cdr l ) ) )
    )
  )
)

;; Method to calculate the dot product of two lists
;
( defmethod dot ( ( l1 list ) ( l2 list ) &aux ret )
  ( setf ret 0 )
  ( if ( not ( = ( length l1 ) ( length l2 ) ) )
    ( let ()
      ( format t "Cannot dot vectors of different lengths!~%" )
      ( return-from dot nil )
    )
  )
  ( cond 
    ( ( null l1 )
      0      
    )
    ( t
      ( setf ret ( + ( dot ( cdr l1 ) ( cdr l2 ) ) ( * ( car l1 ) ( car l2 ) ) ) )
    )
  )
  ret
)

;; Method to perform elemtwise addtion of to matrices
;
( defmethod v-add ( ( x1 array ) ( x2 array ) )
  ( setf nx ( copy-array x1 ) ) 
  ( if ( not ( = ( array-dimension x1 0 ) ( array-dimension x2 0 ) ) )
    ( let ()
      ( format t "Cannot add vectors of size ~A x ~A and ~A x ~A~%"
                  ( array-dimension x1 0 ) ( array-dimension x1 1 ) 
                  ( array-dimension x2 0 ) ( array-dimension x2 1 ) 
      )
      ( return-from v-add nil ) 
    )
  )
  ;all good to go 
  ( dotimes ( i ( array-dimension x1 0 ) )
    ( dotimes ( j ( array-dimension x2 1 ) ) 
      ( setf ( aref nx i j ) ( + ( aref x1 i j ) ( aref x2 i j ) ) ) 
    )
  )
  nx
)

;; Method to perform elemetwise multiplication of
; a constant to every matrix entry
;
( defmethod multiply ( ( c float ) ( x array ) )
  ( setf nx ( copy-array x ) )
  ( dotimes ( i ( array-dimension x 0 ) )
    ( dotimes ( j ( array-dimension x 1 ) )
      ( setf ( aref nx i j ) ( * c ( aref x i j ) ) )
    )
  )
  nx
)

;; Method to calculate the dot product of two matrices
;
( defmethod dot ( ( x1 array ) ( x2 array ) )
  ( if ( not ( equal ( array-dimension x1 1 ) ( array-dimension x2 0 ) ) )
    ( let () 
      ( format t "Cannot multiply a ~A x ~A by a ~A by ~A!~%" 
        ( array-dimension x1 0 ) ( array-dimension x1 1 ) 
        ( array-dimension x2 0 ) ( array-dimension x2 1 ) 
      )
      ( return-from dot nil )
    )
  ) 
  ;all good to go
  ( setf nx ( make-array ( list ( array-dimension x1 0 ) ( array-dimension x2 1 ) ) ) )
  ( dotimes ( i ( array-dimension nx 0 ) )
    ( dotimes ( j ( array-dimension nx 1 ) )
      ( setf ( aref nx i j )  
          ( dot ( get-row i x1 ) ( get-col j x2 ) ) 
      )
    ) 
  )
  nx
)

( defun dobo-demo-8 ()
  ( setf matrix ( make-array '( 4 4 ) :initial-element 1 ) )
  ( format t "matrix: ~%" )
  ( display matrix )
  ( format t "matrix multiplied by 5: ~%" )
  ( display ( multiply 5.0 matrix ) )
  ( format t "matrix 1 dotted with its product with 5: ~%" )
  ( display ( dot matrix ( multiply 5.0 matrix ) ) )
  ( format t "matrix 1 summed with itself: ~%" )
  ( display ( v-add matrix matrix ) )
  ( format t "column 0 of matrix 1 summed: ~A ~%~%" ( sum-list ( get-col 0 matrix ) ) )
  ( set-row 0 '( 0 0 0 0 ) matrix )
  ( format t "matrix: ~%" )
  ( display matrix )
  ( format t "transpose of the matrix: ~%" )
  ( display ( transpose matrix ) )
  ( format t "sigmoid of matrix: ~%" )
  ( display ( sigmoid matrix ) )
)

;;;; Task 9 Defining A Neural Net and Forward Propogation
;

( defclass neural-net ()
  (
    ( weights :accessor neural-net-weights :initarg :weights :initform '() )    
    ( biases :accessor neural-net-biases :initarg :biases :initform '() )
    ( params :accessor neural-net-params :initarg :params :initform '() )
  )
)

( defmethod display ( ( nn neural-net ) )
  ( format t "--------Weights--------" )
  ( dolist ( x ( reverse ( neural-net-weights nn ) ) )
    ( display x ) 
  )
  ( format t "--------Biases---------" )
  ( dolist ( x ( reverse ( neural-net-biases nn ) ) )
    ( display x ) 
  )
)

;; Method to write a neural net to a file
;
( defmethod write-nn-to-file ( ( nn neural-net ) ( nn-file string ) )
  ( setf nn ( net-to-list nn ) )
  ( with-open-file ( stream nn-file :direction :output )
    ( format stream "( " )
    ( dolist ( e nn )
      ( format stream "~A " e )
    )
    ( format stream ")~%~%" ) 
  )
)

;; Method to copy a neural net
;
( defmethod copy ( ( nn neural-net ) )
  ( list-to-net ( net-to-list nn ) nn ) 
)

;; Method to initialize a neural network with a list of 
; the nodes contained in each layer. An additional optional 
; variable can be provided to load the neural net from a file
;
( defun init-neural-net ( l &optional ind-file &aux ind nn weights biases params )
  ( if ind-file 
    ( let ( ( in ( open ind-file :if-does-not-exist nil ) ) )
       ( when in
          ( loop for line = ( read-line in nil ) 
            while line do 
              ( with-input-from-string ( s line ) 
                ( setf ind ( read s ) )
                ( return )
              )
          )
          (close in)
       )
    )
  ) 
  ( setf weights '() )
  ( setf biases '() )
  ( setf params l )
  ( loop while ( not ( = ( length l ) 1 ) ) do
    ( setf weights ( append weights 
      ( list ( make-array ( list ( nth 0 l ) ( nth 1 l ) ) :initial-element 1 ) ) ) 
    )
    ( setf l ( cdr l ) )
    ( setf biases ( append biases
      ( list ( make-array ( list 1 ( nth 0 l ) ) :initial-element 0 ) ) ) 
    )
  )
  ( setf nn ( make-instance 'neural-net :weights weights :biases biases :params params ) )
  ( if ind-file
    ( setf nn ( list-to-net ind ( make-instance 'neural-net :weights weights :biases biases :params params ) ) )
  )
  nn
)

;; Method to forward propogate an input vector through the neural network
;
( defmethod forward-prop ( ( nn neural-net ) ( x array ) &optional notTraining demo &aux activation output )
  ( if demo 
    ( let () 
      ( format t "input: " )
      ( display x ) 
    )
  )
  ( setf activation ( v-add 
                        ( dot ( transpose x ) ( car ( neural-net-weights nn ) ) ) 
                        ( car ( neural-net-biases nn ) ) ) 
  )
  ( if demo
    ( let ()
      ( format t "first layer activation: ~%" ) 
      ( display activation )
    )
  ) 
  ( loop for i from 1 to ( - ( length ( neural-net-weights nn ) ) 1 ) do
    ( setf w ( nth i ( neural-net-weights nn ) ) )
    ( setf b ( nth i ( neural-net-biases nn ) ) )
    ( setf activation ( v-add ( dot ( sigmoid activation ) w ) b ) )
    ( if demo
      ( let ()  
        ( format t "activation of layer ~A: ~%" i )
        ( display activation )
      )
    )
  )
  ( setf output ( sigmoid ( float ( sum-list ( get-col 0 activation ) ) ) ) )
  ( if notTraining
    ( if ( > output .5 )
      ( setf output 1 )
      ( setf output 0 ) 
    ) 
  )
  ( if demo ( format t "net output: ~A~%~%" output ) )
  output
)

;; Wrapper for froward prop that input vectorizes a state before
; forward propogation is applied
;
( defmethod forward-prop ( ( nn neural-net ) ( s state ) &optional notTraining demo )
  ( forward-prop nn ( input-vector s ) notTraining demo )
)

;; Method to turn a state into an input vector
 ; for a neural network
( defmethod input-vector ( ( s state ) )
  ( input-vector ( state-board s ) )
)

;; Helper method to 
( defmethod input-vector ( ( x array ) )
  ( setf vector ( make-array ( list ( expt ( array-dimension x 0 ) 2 ) 1 ) ) )
  ( setf pos 0 )
  ( dotimes ( i ( array-dimension x 0 ) )
    ( dotimes ( j ( array-dimension x 1 ) )
      ( setf ( aref vector pos 0 ) ( aref x i j ) ) 
      ( setf pos ( + pos 1 ) )
    ) 
  ) 
  vector
)

( defun dobo-demo-9 ()
  ( format t "initializing a neural net of size 9-3-1~%" )
  ( setf nn ( init-neural-net '( 9 3 1 ) ) )
  ( display nn ) 
  ( setf x ( make-array '( 9 1 ) :initial-element 1 ) )
  ( forward-prop nn x nil t ) 
  nn
)

;;;; Task 10 -- Training the Neural Network
;**********Demos**************
( setf *ga-demo* nil )
;********Learning Rate*********
( setf *learning-rate* .1 )  
( setf *decreasing-learning-rate* nil )
;**********Sizes**************
( setf *population-size* 10 ) 
( setf *selection-size* 10 )
;********Constants************
( setf *pc-x* 70 ) ; These must total to 100% 
( setf *pc-c* 30 ) ; to mainatin population size
( setf *pc-m* 100 )
( setf *nr-generations* 300 )
( setf *mut-agg* ( / 1 10 ) )
;*********Stats****************
( setf *recording-stats* nil )
( setf *recording-file* nil )
;******Initialization**********
( setf *stoch-init* t )
;***********Bias***************
( setf *bias* t )
;*********normalized***********
( setf *normalized-data-set* t )
;********weight-limits*********
( setf *weight-limits* '(-1 1) )
;

;; Task 10.1 helper methods for the genetic algorithm 
;

( setf *demo-cost* nil )

;; Method to provide the cost of a neural network over
; every entry in the game state table
;
( defmethod cost ( ( nn neural-net ) ( gst game-state-table ) &optional notTraining demo &aux n cost )
  ( setf cost 0 )
  ( if *demo-cost*
    ( setf n 1 )
    ( setf n ( length ( game-state-table-state-probability-tuple-list gst ) ) )
  )
  ( dolist ( entry ( subseq ( game-state-table-state-probability-tuple-list gst ) 0 n ) )
    ( setf state ( state-probability-tuple-game-state entry ) )
    ( setf i-cost 
      ( abs ( - ( table-probability-lookup gst state ) ( forward-prop nn state notTraining demo ) ) )
    ) 
    ( if *normalized-data-set*
      ( if ( < i-cost .5 ) ( setf i-cost 0 ) )
    )
    ( setf cost ( + cost i-cost ) )
  )
  cost
)

;; Method to convert a neural network to just the list of its weights
;
( defmethod net-to-list ( ( nn neural-net ) &aux net-list )
  ( setf net-list '() )
  ;loop through each weight matrix and put rows into the list
  ( dolist ( x ( neural-net-weights nn ) )
    ( loop for i from 0 to ( - ( array-dimension x 0 ) 1 ) do
      ( setf net-list ( append net-list ( copy-list ( get-row i x ) ) ) )
    )
  )
  ( dolist ( x ( neural-net-biases nn ) )
    ( loop for i from 0 to ( - ( array-dimension x 0 ) 1 ) do
      ( setf net-list ( append net-list ( copy-list ( get-row i x ) ) ) )
    )
  )
  net-list
)

;; Method to convert a list representing the weights and biases of 
; a neural network into a neural network  
;
( defmethod list-to-net ( ( n-list list ) ( nn neural-net ) )
  ( setf nn ( init-neural-net ( neural-net-params nn ) ) )
  ( dolist ( w ( neural-net-weights nn ) )
    ( loop for i from 0 to ( - ( array-dimension w 0 ) 1 ) do 
      ( set-row i ( subseq n-list 0 ( array-dimension w 1 ) ) w )
      ( setf n-list ( remove-seq 0 ( array-dimension w 1 ) n-list ) )
    )    
  )
  ( dolist ( b ( neural-net-biases nn ) )
    ( loop for i from 0 to ( - ( array-dimension b 0 ) 1 ) do 
      ( set-row i ( subseq n-list 0 ( array-dimension b 1 ) ) b )
      ( setf n-list ( remove-seq 0 ( array-dimension b 1 ) n-list ) )
    )    
  )
  nn
)

( defun dobo-demo-10.1 () 
  ( format t "Initializing a 9-3-1 neural network ...~%" )
  ( setf nn ( init-neural-net '( 9 3 1 ) ) )
  ( format t "The neural network: ~%" )
  ( display nn )
  ( format t "The list representation of the neural network: ~%" )
  ( setf l ( net-to-list nn ) )
  ( format t "~A~%" l )
  ( setf ( car l ) 0 )
  ( format t "maninpulated list: ~%" )
  ( format t "~A~%" l )
  ( format t "The maninpulated list transformed to the weights of a neural network: ~%" )
  ( display ( list-to-net l nn ) ) 
)

;; Task 10.2; Modeling and Manipulating and Individual
;

( defclass individual ()
  (
    ( neural-net :accessor individual-neural-net :initarg :neural-net :initform nil )
    ( cost :accessor individual-cost :initarg :cost :initform 0 )
  ) 
)

( defmethod copy ( ( i individual ) )
  ( make-instance 'individual :neural-net ( copy ( individual-neural-net i ) )
                              :cost ( individual-cost i )
  )
)

( defmethod display ( ( i individual) )
  ( format t "----------------Individual-------------------~%" )
  ( display ( individual-neural-net i ) ) 
  ( format t "cost: ~A~%~%" ( individual-cost i ) )
  ( format t "---------------------------------------------" )
)

;; Method to determine if an individual should be mutated or not
 ; based on the percent mutation possibility
 ;
( defmethod maybe-mutate ( ( i individual ) )
  ( if ( <= ( + 1 ( random 100 ) ) *pc-m* ) 
    ( mutate i )
    i
  )
)

;; Method to mutate and individual by radnomly selecting 
	; a certain amount of positions in the list representation of 
	; the neural network's weights and incrimenting or decrememting
	; them by the learning rate
  ;
( defmethod mutate ( ( i individual ) &aux m-list nn len )
  ( setf m-list ( net-to-list ( individual-neural-net i ) ) )
  ( setf nn ( individual-neural-net i ) )
  ( setf len ( length m-list ) ) 
  ( dotimes ( j ( + 1 ( floor ( random ( float ( * *mut-agg* len ) ) ) ) ) )  
    ( setf pos ( random ( length m-list ) ) )  
    ( setf inc-or-dec ( random 2 ) )
    ( if ( > inc-or-dec 0 ) 
      ( setf ( nth pos m-list ) ( + ( nth pos m-list ) *learning-rate* ) ) 
      ( setf ( nth pos m-list ) ( - ( nth pos m-list ) *learning-rate* ) ) 
    )
    ( if *weight-limits* 
      ( let () 
        ( if ( < ( nth pos m-list ) ( nth 0 *weight-limits* ) )
          ( setf ( nth pos m-list ) ( nth 0 *weight-limits* ) )
        )
        ( if ( > ( nth pos m-list ) ( nth 1 *weight-limits* ) )
          ( setf ( nth pos m-list ) ( nth 1 *weight-limits* ) )
        )
      )
    )
  )
  ( setf i ( make-instance 'individual :neural-net ( list-to-net m-list nn ) ) )
)

( defun dobo-demo-10.2.1 ()
  ( setf nn ( init-neural-net '( 9 3 1 ) ) )
  ( setf i ( make-instance 'individual :neural-net nn ) )
  ( format t "~%~%initial-individual: ~%~%" )
  ( display i )
  ( dotimes ( j 5 )
    ( format t "~%~%mutated-individual: ~%~%" )
    ( display ( mutate i ) )
  )
)

;; Method to crossover two nets
;
( defmethod crossover ( ( mother individual ) ( father individual ) &aux m f )
  ( setf m ( net-to-list ( individual-neural-net mother ) ) )
  ( setf f ( net-to-list ( individual-neural-net father ) ) )
  ( setf pos ( + 1 ( random ( length m ) ) ) )
  ( setf m
    ( list-to-net
      ( append ( first-n m pos ) ( rest-n f pos ( length m ) ) )
      ( individual-neural-net mother )
    )
  )
  ( make-instance 'individual :neural-net m )
)

( defun dobo-demo-10.2.2 ()
  ( setf nn1 ( init-neural-net '( 9 3 1 ) ) )
  ( setf nn2 ( copy nn1 ) )
  ( setf nn1 ( list-to-net ( gen-stoch-init ( net-to-list nn1 ) ) nn1 ) )
  ( setf i1 ( make-instance 'individual :neural-net nn1 ) )
  ( setf i2 ( make-instance 'individual :neural-net nn1 ) )
  ( format t "~%~%initial-individual2: ~%~%" )
  ( display i1 )
  ( display i2 )
  ( format t "~%~%crossed-individual: ~%~%" )
  ( display ( crossover i1 i2 ) )
)

( defun dobo-demo-10.2 () 
  ( dobo-demo-10.2.1 )
  ( dobo-demo-10.2.2 )
)

;; Task 10.3 Modeling and Maninpulating a population
;

( defclass population ()
  (
    ( individuals :accessor population-individuals :initarg :individuals :initform () )
    ( generation :accessor population-generation :initform 0 )
    ( game-state-table :accessor population-gst :initarg :gst :initform '() )
  )
)

( defmethod display ( ( p population ) )
  ( loop for i from 0 to ( - ( length ( population-individuals p ) ) 1 ) do
    ( format t "~A: ~A, cost: ~A~%" 
      ( + i 1 )
      ( net-to-list ( individual-neural-net ( nth i ( population-individuals p ) ) ) )
      ( individual-cost ( nth i ( population-individuals p ) ) )
    )
  ) 
)

;; Method to "seed" a population with an individual, pushing the population in 
 ; the direction of a previously found favorable individual
 ;
( defmethod seed-population ( ( p population ) ( ind-file string ) ( nn neural-net ) ( gst game-state-table ) )
  ( let ( ( in (open ind-file :if-does-not-exist nil ) ) )
     ( when in
        ( loop for line = ( read-line in nil ) 
          while line do 
            ( with-input-from-string ( s line ) 
              ( setf ind ( read s ) )
              ( return )
            )
        )
        (close in)
     )
  )
  ( setf nn ( list-to-net ind nn ) ) 
  ( setf cost ( cost nn gst ) ) 
  ( setf ind ( make-instance 'individual :neural-net nn :cost cost ) )
  ( setf individuals ( reverse ( select-individuals p ) ) )
  ( dotimes ( i ( / ( - ( length individuals ) 1 ) 2 ) )
    ( setf ( nth i individuals ) ( copy ind ) )
  )
  ( setf ( population-individuals p ) individuals )
)

;; Method to initialize a population with a neural net and a game state table
 ;
( defmethod init-population ( ( nn neural-net ) ( gst game-state-table ) &aux individuals l )
  ( setf individuals '() )
  ( dotimes ( i *population-size* ) 
    ( setf nn ( init-neural-net ( neural-net-params nn ) ) )
    ;stochastic initialization
    ( if *stoch-init*
      ( let ()
        ( setf l ( gen-stoch-init ( net-to-list nn ) ) )
        ( setf nn ( list-to-net l nn ) )
      )
    )
    ( setf cost ( cost nn gst ) )
    ( push ( make-instance 'individual :neural-net nn :cost cost ) individuals )
  )
  ( make-instance 'population :individuals individuals :gst gst )
)

;; Method to create an empty population based on the
	; characteristics of a predefined population
 ;
( defmethod empty-population ( ( cp population ) &aux np )
  ( setf np ( make-instance 'population ) )
  ( setf ( population-generation np ) ( + 1 ( population-generation cp ) ) )
  np 
)

;; Method to update the costs of the individuals of a population
 ;
( defmethod update-costs ( ( p population ) ( gst game-state-table ) )
  ( dolist ( i ( population-individuals p ) )
    ( setf ( individual-cost i ) ( cost ( individual-neural-net i ) gst ) ) 
  )
)

;; Method to find the average cost of a population
 ;
( defmethod average ( ( p population ) &aux ( sum 0 ) )
  ( setf individuals ( population-individuals p ) )
  ( dotimes ( i *population-size* )
    ( setf sum ( + sum ( individual-cost ( nth i individuals ) ) ) )
  )
  ( handler-case 
    ( / sum *population-size* )
    ( division-by-zero () 
      ( setf ratio "high" ) 
    )
  )
)

( defmethod select-mother-father ( ( p population ) &aux mfi candidates )
  ( setf candidates ( select-individuals p ) )
  ( setf m ( most-fit-individual candidates ) )
  ( remove m candidates )
  ( setf f ( most-fit-individual candidates ) )
  ( list m f ) 
)

( defmethod select-individuals ( ( p population ) &aux individuals candidates rn )
  ( setf individuals ( population-individuals p ) )
  ( setf candidates '() )
  ;want to make sure the best individual caries over
  ( setf candidates ( population-individuals p ) ) 
  ;sorting candidates
  ( setf ordered-candidates '() )
  ( loop while ( not ( null candidates ) ) do 
    ( setf vals ( mapcar #'individual-cost candidates ) )
    ( setf min ( loop for x in vals minimize x ) )
    ( setf min ( nth ( position min vals ) candidates ) ) 
    ( setf candidates ( remove min candidates :count 1 ) )
    ( setf min ( copy min ) )
    ( setf ordered-candidates ( append ordered-candidates ( list min ) ) )  
  ) 
  ordered-candidates
)

;; Method to select the most fit individual from a list of individuals 
 ;
( defmethod most-fit-individual ( ( l list ) &aux max-value max-individual )
  ( setf min-individual ( car l ) )
  ( setf min-value ( individual-cost min-individual ) )
  ( dotimes ( i ( length l ) )
    ( if ( < ( individual-cost ( nth i l ) ) min-value )
      ( let ()
        ( setf min-individual ( nth i l ) )
        ( setf min-value ( individual-cost min-individual ) )
      )
    )
  )
  min-individual
) 

( defmethod perform-crossovers ( ( cp population ) ( np population ) )
  ( dotimes ( i ( nr-crossovers ) )
    ( perform-one-crossover cp np )
  )
)

( defmethod nr-crossovers ()
  ( * ( / *pc-x* 100 ) *population-size* )
)

( defmethod perform-one-crossover ( ( cp population ) ( np population ) )
  ( let ( x m mother father new-i )
    ( setf mf ( select-mother-father cp ) )
    ( setf mother ( nth 0 mf ) )
    ( setf father ( nth 1 mf ) )
    ( setf m ( crossover mother father ) )
    ( setf m ( maybe-mutate m ) )
    ( setf 
      ( population-individuals np ) 
      ( append ( population-individuals np ) ( list m ) )
    )
  )
)

( defmethod perform-copies ( ( cp population ) ( np population ) &aux individuals )
  ( setf individuals ( first-n ( select-individuals cp ) ( nr-copies ) ) )
  ( setf 
    ( population-individuals np )
    ( append ( population-individuals np ) ( list ( car individuals ) ) )
  )
  ( setf individuals ( cdr individuals ) )
  ( dolist ( i individuals )
    ( setf i ( maybe-mutate i ) )
    ( setf 
      ( population-individuals np )
      ( append ( population-individuals np ) ( list i ) )
    )
  )
  np
)

( defmethod nr-copies () 
  ( * ( / *pc-c* 100 ) *population-size* )
)

;;providing information on progress of generations
;
( defmethod summarize ( ( p population ) )
  ( display p )
  ( check-average p ) 
  ( terpri ) 
)

( defmethod check-average ( ( p population ) )
  ( format t "average fitness of population ~A = ~f~%"
    ( population-generation p )
    ( average p )
  )
)

;; Method to produce the next generation of a population
;
( defmethod next-gen ( ( cp population ) ( gst game-state-table ) &aux np )
  ( setf np ( empty-population cp ) )
  ( perform-copies cp np ) 
  ( perform-crossovers cp np ) 
  ( update-costs np gst )
  np
)

( defun dobo-demo-10.3 () 
  ( setf *demo-cost* t )
  ( format t "Initial population: ~%" )
  ( setf nn ( init-neural-net '( 9 3 1 ) ) )
  ( setf gst ( read-gst-from-file ) )
  ( setf p ( init-population nn gst ) )
  ( display p )
  ( format t "~%~% Performing copies from initial population to new population: ~%~%" )
  ( setf np ( empty-population p ) )
  ( perform-copies p np )
  ( display np )
  ( format t "~%~% Performing crossovers from initial population to new population: ~%~%" ) 
  ( perform-crossovers p np )
  ( display np ) 
  ( setf *demo-cost* nil )
)

;; Implenting the genetic algorithm with all of Task 10
;

;; The Genetic Algorithm 
;
( defmethod train-neural-network ( &aux p )
  ( if *normalized-data-set*
    ( setf *file* "normalized_gst_3x3.txt" )
  )  
  ( setf gst ( read-gst-from-file ) )
  ( setf seed nil )
  ( format t "Neural Netrwork params?(list): " )
  ( setf params ( read ) )
  ( setf nn ( init-neural-net params ) ) 
  ( format t "File to write to?(quoted): " )
  ( setf nn-file ( read ) )
  ( format t "Would you like to seed the population?(y/n): " )
  ( setf seed ( read ) )
  ( if ( equal seed 'y )
    ( let () 
      ( format t "With what individual file?(quoted): " )
      ( setf ind-file ( read ) )
    )
  )
  ( setf pop ( init-population nn gst ) )
  ( if ( equal seed 'y )
    ( seed-population pop ind-file nn gst )
  )
  ( setf best-individual ( car ( select-individuals pop ) ) )
  ( format t "best individual: ~A~%" ( net-to-list ( individual-neural-net best-individual ) ) )
  ( format t "cost of best individual: ~A~% " ( individual-cost best-individual ) )
  ( terpri )
  ( dotimes ( i *nr-generations* )
    ( write-nn-to-file ( individual-neural-net best-individual ) nn-file )
    ( setf pop ( next-gen pop gst ) )
    ( check-average pop )
    ( setf best-individual ( car ( select-individuals pop ) ) )
    ( if *recording-stats* ( write-nn-training-stats-vs-time *recording-file* best-individual i ) )
    ( format t "cost of best individual: ~A~% " ( individual-cost best-individual ) )
    ( if *decreasing-learning-rate*
      ( setf *learning-rate* ( - *learning-rate* ( float ( / *learning-rate* *nr-generations* ) ) ) )
    )
    ( setf *mut-agg* ( - *mut-agg* ( / *mut-agg* *nr-generations* ) ) )
  )
  ( terpri )
  ( summarize pop )
  ( setf best-individual ( car ( select-individuals pop ) ) )
  ( format t "best individual: ~A~%" ( net-to-list ( individual-neural-net best-individual ) ) )
  ( format t "cost of best individual: ~A~% " ( individual-cost best-individual ) )
  ( individual-neural-net best-individual )
)

( defun nn-test ( nn )
  ( setf *file* "normalized_gst_3x3.txt" )
  ( setf gst ( read-gst-from-file ) )
  ( setf bad-entries 0 )
  ( dolist ( entry ( game-state-table-state-probability-tuple-list gst ) ) 
    ( setf state ( state-probability-tuple-game-state entry ) )
    ( display state )
    ( setf output ( forward-prop nn state t ) )
    ( setf actual ( state-probability-tuple-probability entry ) )
    ( format t "output: ~A~%actual: ~A~%" output actual ) 
    ( setf cost ( abs ( - actual output ) ) )
    ( setf bad-entries ( + bad-entries cost ) )
    ( format t "cost : ~A~%~%"  cost ) 
  )
  ( format t "Bad entries: ~A~%" bad-entries )
  ( setf len ( length ( game-state-table-state-probability-tuple-list gst ) ) )
  ( setf ret ( / ( - len bad-entries ) len ) ) 
  ( format t "retention rate: ~A\%~%" ret )
)

;; Task 10 all inclusive demo
;
( defun dobo-demo-10 ( &aux nn l ) 
  ( dobo-demo-10.1 )
  ( dobo-demo-10.2 ) 
  ( dobo-demo-10.3 ) 
  ( format t "Lets train a neural network! ~%" )
  ( setf temp *nr-generations* )
  ( setf *nr-generations* 10 )
  ( setf *demo-cost* t )
  ( setf nn ( train-neural-network ) )
  ( format t "Now lets test it! ~%" )
  ( nn-test nn )
  ( setf *nr-generations* temp )
  ( setf *demo-cost* nil )
)

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

( defun net-vs-random-player-stats ( params l n )
  ( setf nn ( init-neural-net params ) )
  ( setf nn ( list-to-net l nn ) )
  ( setf player1 ( make-instance 'random-machine-player ) )
  ( setf player2 ( make-instance 'neural-net-player :neural-net nn ) )
  ( dotimes ( i n )
    ( setf game ( generic-play player1 player2 nil nil ) )
    ( if ( = ( mod i ( / n 2 ) ) 0 )
      ( let ()
        ( format t "Player switch ~%" )
        ( setf temp player1 ) 
        ( setf player1 player2 )
        ( setf player2 temp )
      )
    )
  )
  ( display player1 )
  ( display player2 )
)

( defmethod write-nn-training-stats-vs-time ( ( f string ) ( i individual ) ( n integer ) ) 
  ( with-open-file ( stream f :direction :output :if-exists :append :if-does-not-exist :create )
    ;writing stats to file   
    ( format stream "~A , ~A~%" n ( individual-cost i ) ) 
  )
)

;; Some graphs to find the best meta variable settings for the
 ; genetic algorithm

;; graph 1 30% mutation, (1/10) mutation aggression with varying 
 ; crossover and copy ratio
( defun graph1 ()
  ( setf *recording-stats* t )
  ( setf *stoch-init* nil )
  ( setf *nr-generations* 300 )
  ( setf *mut-agg* ( / 1 10 ) )
  ( setf *pc-m* 30 )
  ;config 1
  ( setf *recording-file* "graph_1_1.csv" )
  ( setf *pc-x* 30 ) ; These must total to 100% 
  ( setf *pc-c* 70 ) ; to mainatin population size
  ( ga-text-demo '( 9 3 1 ) "nn_test.txt" nil nil ) 
  ;config 2
  ( setf *recording-file* "graph_1_2.csv" )
  ( setf *pc-x* 50 ) ; These must total to 100% 
  ( setf *pc-c* 50 ) ; to mainatin population size
  ( ga-text-demo '( 9 3 1 ) "nn_test.txt" nil nil ) 
  ;config 3
  ( setf *recording-file* "graph_1_3.csv" )
  ( setf *pc-x* 70 ) ; These must total to 100% 
  ( setf *pc-c* 30 ) ; to mainatin population size
  ( ga-text-demo '( 9 3 1 ) "nn_test.txt" nil nil ) 
)

;; graph 2 50% mutation, (1/10) mutation aggression with varying 
 ; crossover and copy ratio
( defun graph2 ()
  ( setf *recording-stats* t )
  ( setf *stoch-init* nil )
  ( setf *nr-generations* 300 )
  ( setf *mut-agg* ( / 1 10 ) )
  ( setf *pc-m* 50 )
  ;config 1
  ( setf *recording-file* "graph_2_1.csv" )
  ( setf *pc-x* 30 ) ; These must total to 100% 
  ( setf *pc-c* 70 ) ; to mainatin population size
  ( ga-text-demo '( 9 3 1 ) "nn_test.txt" nil nil ) 
  ;config 2
  ( setf *recording-file* "graph_2_2.csv" )
  ( setf *pc-x* 50 ) ; These must total to 100% 
  ( setf *pc-c* 50 ) ; to mainatin population size
  ( ga-text-demo '( 9 3 1 ) "nn_test.txt" nil nil ) 
  ;config 3
  ( setf *recording-file* "graph_2_3.csv" )
  ( setf *pc-x* 70 ) ; These must total to 100% 
  ( setf *pc-c* 30 ) ; to mainatin population size
  ( ga-text-demo '( 9 3 1 ) "nn_test.txt" nil nil ) 
)

;; graph 3 70% mutation, (1/10) mutation aggression with varying 
 ; crossover and copy ratio
( defun graph3 ()
  ( setf *recording-stats* t )
  ( setf *stoch-init* nil )
  ( setf *nr-generations* 300 )
  ( setf *mut-agg* ( / 1 10 ) )
  ( setf *pc-m* 70 )
  ;config 1
  ( setf *recording-file* "graph_3_1.csv" )
  ( setf *pc-x* 30 ) ; These must total to 100% 
  ( setf *pc-c* 70 ) ; to mainatin population size
  ( ga-text-demo '( 9 3 1 ) "nn_test.txt" nil nil ) 
  ;config 2
  ( setf *recording-file* "graph_3_2.csv" )
  ( setf *pc-x* 50 ) ; These must total to 100% 
  ( setf *pc-c* 50 ) ; to mainatin population size
  ( ga-text-demo '( 9 3 1 ) "nn_test.txt" nil nil ) 
  ;config 3
  ( setf *recording-file* "graph_3_3.csv" )
  ( setf *pc-x* 70 ) ; These must total to 100% 
  ( setf *pc-c* 30 ) ; to mainatin population size
  ( ga-text-demo '( 9 3 1 ) "nn_test.txt" nil nil ) 
)

;; graph 4 30% mutation, (1/5) mutation aggression with varying 
 ; crossover and copy ratio
( defun graph4 ()
  ( setf *recording-stats* t )
  ( setf *stoch-init* nil )
  ( setf *nr-generations* 300 )
  ( setf *mut-agg* ( / 1 5 ) )
  ( setf *pc-m* 30 )
  ;config 1
  ( setf *recording-file* "graph_4_1.csv" )
  ( setf *pc-x* 30 ) ; These must total to 100% 
  ( setf *pc-c* 70 ) ; to mainatin population size
  ( ga-text-demo '( 9 3 1 ) "nn_test.txt" nil nil ) 
  ;config 2
  ( setf *recording-file* "graph_4_2.csv" )
  ( setf *pc-x* 50 ) ; These must total to 100% 
  ( setf *pc-c* 50 ) ; to mainatin population size
  ( ga-text-demo '( 9 3 1 ) "nn_test.txt" nil nil ) 
  ;config 3
  ( setf *recording-file* "graph_4_3.csv" )
  ( setf *pc-x* 70 ) ; These must total to 100% 
  ( setf *pc-c* 30 ) ; to mainatin population size
  ( ga-text-demo '( 9 3 1 ) "nn_test.txt" nil nil ) 
)

;; graph 5 50% mutation, (1/5) mutation aggression with varying 
 ; crossover and copy ratio
( defun graph5 ()
  ( setf *recording-stats* t )
  ( setf *stoch-init* nil )
  ( setf *nr-generations* 300 )
  ( setf *mut-agg* ( / 1 5 ) )
  ( setf *pc-m* 50 )
  ;config 1
  ( setf *recording-file* "graph_5_1.csv" )
  ( setf *pc-x* 30 ) ; These must total to 100% 
  ( setf *pc-c* 70 ) ; to mainatin population size
  ( ga-text-demo '( 9 3 1 ) "nn_test.txt" nil nil ) 
  ;config 2
  ( setf *recording-file* "graph_5_2.csv" )
  ( setf *pc-x* 50 ) ; These must total to 100% 
  ( setf *pc-c* 50 ) ; to mainatin population size
  ( ga-text-demo '( 9 3 1 ) "nn_test.txt" nil nil ) 
  ;config 3
  ( setf *recording-file* "graph_5_3.csv" )
  ( setf *pc-x* 70 ) ; These must total to 100% 
  ( setf *pc-c* 30 ) ; to mainatin population size
  ( ga-text-demo '( 9 3 1 ) "nn_test.txt" nil nil ) 
)

;; graph 6 70% mutation, (1/5) mutation aggression with varying 
 ; crossover and copy ratio
( defun graph6 ()
  ( setf *recording-stats* t )
  ( setf *stoch-init* nil )
  ( setf *nr-generations* 300 )
  ( setf *mut-agg* ( / 1 5 ) )
  ( setf *pc-m* 70 )
  ;config 1
  ( setf *recording-file* "graph_6_1.csv" )
  ( setf *pc-x* 30 ) ; These must total to 100% 
  ( setf *pc-c* 70 ) ; to mainatin population size
  ( ga-text-demo '( 9 3 1 ) "nn_test.txt" nil nil ) 
  ;config 2
  ( setf *recording-file* "graph_6_2.csv" )
  ( setf *pc-x* 50 ) ; These must total to 100% 
  ( setf *pc-c* 50 ) ; to mainatin population size
  ( ga-text-demo '( 9 3 1 ) "nn_test.txt" nil nil ) 
  ;config 3
  ( setf *recording-file* "graph_6_3.csv" )
  ( setf *pc-x* 70 ) ; These must total to 100% 
  ( setf *pc-c* 30 ) ; to mainatin population size
  ( ga-text-demo '( 9 3 1 ) "nn_test.txt" nil nil ) 
)

;; graph 7 30% mutation, (1/3) mutation aggression with varying 
 ; crossover and copy ratio
( defun graph7 ()
  ( setf *recording-stats* t )
  ( setf *stoch-init* nil )
  ( setf *nr-generations* 300 )
  ( setf *mut-agg* ( / 1 3 ) )
  ( setf *pc-m* 30 )
  ;config 1
  ( setf *recording-file* "graph_7_1.csv" )
  ( setf *pc-x* 30 ) ; These must total to 100% 
  ( setf *pc-c* 70 ) ; to mainatin population size
  ( ga-text-demo '( 9 3 1 ) "nn_test.txt" nil nil ) 
  ;config 2
  ( setf *recording-file* "graph_7_2.csv" )
  ( setf *pc-x* 50 ) ; These must total to 100% 
  ( setf *pc-c* 50 ) ; to mainatin population size
  ( ga-text-demo '( 9 3 1 ) "nn_test.txt" nil nil ) 
  ;config 3
  ( setf *recording-file* "graph_7_3.csv" )
  ( setf *pc-x* 70 ) ; These must total to 100% 
  ( setf *pc-c* 30 ) ; to mainatin population size
  ( ga-text-demo '( 9 3 1 ) "nn_test.txt" nil nil ) 
)

;; graph 8 50% mutation, (1/3) mutation aggression with varying 
 ; crossover and copy ratio
( defun graph8 ()
  ( setf *recording-stats* t )
  ( setf *stoch-init* nil )
  ( setf *nr-generations* 300 )
  ( setf *mut-agg* ( / 1 3 ) )
  ( setf *pc-m* 50 )
  ;config 1
  ( setf *recording-file* "graph_8_1.csv" )
  ( setf *pc-x* 30 ) ; These must total to 100% 
  ( setf *pc-c* 70 ) ; to mainatin population size
  ( ga-text-demo '( 9 3 1 ) "nn_test.txt" nil nil ) 
  ;config 2
  ( setf *recording-file* "graph_8_2.csv" )
  ( setf *pc-x* 50 ) ; These must total to 100% 
  ( setf *pc-c* 50 ) ; to mainatin population size
  ( ga-text-demo '( 9 3 1 ) "nn_test.txt" nil nil ) 
  ;config 3
  ( setf *recording-file* "graph_8_3.csv" )
  ( setf *pc-x* 70 ) ; These must total to 100% 
  ( setf *pc-c* 30 ) ; to mainatin population size
  ( ga-text-demo '( 9 3 1 ) "nn_test.txt" nil nil ) 
)

;; graph 9 70% mutation, (1/3) mutation aggression with varying 
 ; crossover and copy ratio
( defun graph9 ()
  ( setf *recording-stats* t )
  ( setf *stoch-init* nil )
  ( setf *nr-generations* 300 )
  ( setf *mut-agg* ( / 1 3 ) )
  ( setf *pc-m* 70 )
  ;config 1
  ( setf *recording-file* "graph_9_1.csv" )
  ( setf *pc-x* 30 ) ; These must total to 100% 
  ( setf *pc-c* 70 ) ; to mainatin population size
  ( ga-text-demo '( 9 3 1 ) "nn_test.txt" nil nil ) 
  ;config 2
  ( setf *recording-file* "graph_9_2.csv" )
  ( setf *pc-x* 50 ) ; These must total to 100% 
  ( setf *pc-c* 50 ) ; to mainatin population size
  ( ga-text-demo '( 9 3 1 ) "nn_test.txt" nil nil ) 
  ;config 3
  ( setf *recording-file* "graph_9_3.csv" )
  ( setf *pc-x* 70 ) ; These must total to 100% 
  ( setf *pc-c* 30 ) ; to mainatin population size
  ( ga-text-demo '( 9 3 1 ) "nn_test.txt" nil nil ) 
)




