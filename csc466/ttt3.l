( load "lp.l" )

;;;; TASK 3 - modeling human and random players

; The root of the player hierarchy
;
( defclass player () 
  ( ( name :accessor player-name :initarg :name :initform 'emanon ) )
)

; A random machine player is a player
;
( defclass random-machine-player ( player ) () )

( defmethod display ( ( p random-machine-player ) )
  ( format t "RANDOM MACHINE PLAYER ...~%" )
  ( format t "name = ~A~%" ( player-name p ) )
  ( format t "~%" )
  nil 
)

; A human player is a player
;
( defclass human-player ( player ) () )

( defmethod display ( ( p human-player ) )
  ( format t "HUMAN PLAYER ...~%" )
  ( format t "name = ~A~%" )
  ( format t "~%" )
  nil
)

; A move making method for a random machine player.
;

( defmethod make-move ( ( p random-machine-player ) ( report t ) &aux move )
  ( if report ( format t "BEGIN RANDOM PLAYER MOVE ...~%" ) )
  ( setf move ( pick *avail* ) )
  ( if report ( format t "randomly selecting ~A for my move~%" move ) )
  ( setf *avail* ( remove move *avail* ) )
  ( if report ( format t "END RANDOM PLAYER MOVE ...~%" ) )
  move
)

; A move making method for a human player
;
( defmethod make-move ( ( p human-player ) ( report t ) &aux move )
  ( if report ( format t "BEGIN HUMAN PLAYER MOVE ...~%") )
  ( format t "Please select a move from ~A~%" *avail* )
  ( setf move ( read ) )
  ( cond
    ( ( not ( member move *avail* ) ) 
      ( make-move p )
    )
    ( t
      ( setf *avail* ( remove move *avail* ) )
      move 
    )
  )
  ( if report ( format t "END HUMAN PLAYER MOVE~%" ) )
  move 
)

; A generic play method. The *play-so-far* variable will be used by the heuristic machine
;
( defmethod generic-play ( ( x player ) ( o player ) ( report t ) &aux move )
  ( setf *avail* '( nw n ne w c e sw s se ) )
  ( setf *play-so-far* () )
  ( dolist ( player '( x o x o x o x o x ) )
    ( if ( or report ( equal ( type-of o ) 'human-player-machine ) )
      ( visualize *play-so-far* )
    )
    ( cond
      ( ( eq player 'x )
        ( setf move ( make-move x report ) )
      )
      ( ( eq player 'o )
        ( setf move ( make-move o report ) )
      )
    )
    ( setf *play-so-far* ( snoc move *play-so-far* ) )
    ( if ( game-over-p *play-so-far* ) ( return nil ) )
  )
  *play-so-far*
)

; Predicate to determine if the game is over or not
;
( defmethod game-over-p ( ( play list ) )
  ( cond 
    ( ( line-p ( odd play ) ) 'w )
    ( ( line-p ( even play ) ) 'l )
    ( ( = ( length play ) 9 ) 'd )
    ( t nil )
  )
)

( defmethod odd ( ( l list ) )
  ( cond 
    ( ( null l ) () )
    ( ( null ( cdr l ) ) ( list ( car l ) ) )
    ( t ( cons ( car l ) ( odd ( cddr l ) ) ) )
  )
)

( defmethod even ( ( l list ) )
  ( cond
    ( ( null l ) () )
    ( ( null ( cdr l ) ) () )
    ( t ( cond ( cadr l ) ( even ( cddr l ) ) ) )
  )
)

( defmethod line-p ( ( l list ) )
  ( cond 
    ( ( < ( length l ) 3 )
      nil
    )
    ( ( = ( length l ) 3 )
      ( line ( first l ) ( second l ) ( third l ) )
    )
    ( ( = ( length l ) 4 )
      ( or
        ( line ( first l ) ( second l ) ( third l ) )
        ( line ( first l ) ( second l ) ( fourth l ) )
        ( line ( first l ) ( third l ) ( fourth l ) )
        ( line ( second l ) ( third l ) ( fourth l ) ) 
      )
    )
    ( ( = ( length l ) 5 )
      ( or 
        ( line ( first l ) ( second l ) ( third l ) )
        ( line ( first l ) ( second l ) ( fourth l ) ) 
        ( line ( first l ) ( second l ) ( fifth l ) )
        ( line ( first l ) ( third l ) ( fourth l ) )
        ( line ( first l ) ( third l ) ( fifth l ) ) 
        ( line ( first l ) ( fourth l ) ( fifth l ) )
        ( line ( second l ) ( third l ) ( fourth l ) )
        ( line ( second l ) ( third l ) ( fifth l ) )
        ( line ( second l ) ( fourth l ) ( fifth l ) )
        ( line ( third l ) ( fourth l ) ( fifth l ) )
      )
    )
  )
)

( defmethod line ( x y z )
  ( and ( eq x y ) ( eq y z ) ( eq x z ) )
)

; Two random machine players play one game
;
( defmethod demo-random-random ( &aux p x o ) 
  ( setf x ( make-instance 'random-machine-player ) )
  ( setf o ( make-instance 'random-machine-player ) )
  ( setf p ( generic-play x o t ) )
  ( format t "~A~%" p )
  ( visualize p )
  ( format t "~A~%" ( analyze p ) )
  nil
)

; A random machine player plays one game agains a human player.
;
( defmethod demo-random-human ( &aux p x o )
  ( setf x ( make-instance 'random-machine-player ) )
  ( setf o ( make-instance 'human-player ) )
  ( setf p ( generic-play x o t ) )
  ( format t "~A~%" p ) 
  ( visualize p )
  ( format t "~A~%" ( analyze p ) )
  nil
) 

;simulate the play of a game
( defmethod play ( &aux play avail move )
  ( setf play () )
  ( setf avail '( nw n ne w c e sw s se ) )
  ( dolist ( player '( x o x o x o x o x ) )
    ( cond
      ( ( eq player 'x )
        ( setf move ( pick avail ) )
        ( setf avail ( remove move avail ) )
        ( setf play ( snoc move play ) )
      )
      ( ( eq player 'o )
        ( setf move ( pick avail ) )
        ( setf avail ( remove move avail ) )
        ( setf play ( snoc move play ) )
      )
    )
  )
  play
)

;; Visualize the play of a game in 2D with turn number annotations
;; to the marks.
;;
( defmethod visualize ( ( gameplay list ) &aux poslist orderlist )
  ( setf poslist '( nw n ne w c e sw s se ) )
  ( setf orderlist '( X1 O1 X2 O2 X3 O3 X4 O4 X5 ) )
  ( dolist ( pos poslist )
    ;find where in the gameplay this move was played, and print who played it
    ( setf index ( position pos gameplay ) ) 
    ( if index 
      ( format t " ~A" ( nth index orderlist ) ) 
      ( format t " --" ) 
    )
    ( if ( or ( eq pos 'ne ) ( eq pos 'e ) ) ( format t "~%" ) ) 
  )
  ( format t "~%" )
)

;;Analyze the play of a game as W or L or D from X's perspective.
;;
;
( defmethod analyze ( ( play list ) &aux result moves )
  ( setf result 'D )
  ;start checking for wins after x has had 3 moves, being position 5
  ( dotimes ( i 5 ) 
    ( if ( eq ( mod i 2 ) 0 )
      ;if it is an odd move, grab all the odd moves and vice versa
      ( setf moves ( odd ( subseq play 0 ( + i 5 ) ) ) )
      ( setf moves ( even ( subseq play 0 ( + i 5 ) ) ) )
    )
    ;check if there is a winning sequence
    ( if ( win-p moves )
      ( let ()
        ( if ( eq ( mod i 2 ) 0 )
         ( setf result 'W )
         ( setf result 'L )
        )
        ( return )
      )
    )
  )
  result
)

(defmethod win-p ( ( play list ) )
  ( or
    ;check all rows
    ( member-all '( nw n ne ) play )
    ( member-all '( w c e ) play )
    ( member-all '( sw s se ) play )
    ;check all cols
    ( member-all '( nw w sw ) play )
    ( member-all '( n c s ) play )
    ( member-all '( ne e se ) play )
    ;check diagonals 
    ( member-all '( nw c se ) play )
    ( member-all '( ne c sw ) play )
  )
)

( defmethod member-all ( ( l1 list ) ( l2 list ) )
  ( cond
    ( ( NULL l1 )
       t
    )
    ( ( not ( member ( car l1 ) l2 ) ) 
      NIL
    )
    ( t 
      ( member-all ( cdr l1 ) l2 )
    )
  )
)

;; Simulate one play of the game, visualize it, analyze it
;
( defmethod demo ( &aux p )
  ( setf p ( play ) )
  ( format t "~A~%" p )
  ( visualize p ) 
  ( format t "~A~%" ( analyze p ) )
  nil
)

;; Method to evaluate performance of player X (random player vs. random player).
;; It can be run in two modes, verbose (demo=t) or quiet (demo=nil).
;
( defmethod stats ( ( n number ) ( demo t ) &aux w l d p result )
  ( if demo ( format t "BEGIN GATHERING STATISTICS ...~%" ) )
  ( setf w 0 l 0 d 0 )
  ( dotimes ( i n )
    ( setf p ( play ) )
    ( if demo ( format t "~A~%" p ) )
    ( if demo ( visualize p ) )
    ( setf result ( analyze p ) )
    ( if demo ( format t "~A~%" result ) )
    ( cond
      ( ( eq result 'w ) ( setf w ( + w 1 ) ) ) 
      ( ( eq result 'l ) ( setf l ( + l 1 ) ) )
      ( ( eq result 'd ) ( setf d ( + d 1 ) ) ) 
    )
  )
  ( setf results ( mapcar #'probability ( list w l d ) ( list n n n ) ) )
  ( if demo ( format t "END GATHERING STATISTICS~%" ) )
  ( mapcar #'list '( w l d ) results )
)

( defmethod probability ( ( special integer ) ( total integer ) )
  ( / ( float special ) ( float total ) )
)

( defmethod odd ( ( l list ) )
  ( cond
    ( ( null l ) 
      ()  
    )
    ( ( null ( cdr l ) ) 
      ( list ( car l ) ) 
    )
    ( t 
      ( cons ( car l ) ( odd ( cddr l ) ) ) 
    )
  )
)

( defmethod even ( ( l list ) )
  ( cond
    ( ( null l ) 
      () 
    )
    ( ( null ( cdr l ) ) () )
    ( t ( cons ( cadr l ) ( even ( cddr l ) ) ) )
  )
)
;first inefficient visualize with a bandaid
;( defmethod visualize ( ( gameplay list ) &aux poslist )
;  ( setf poslist '( nw n ne w c e sw s se ) )
;  ( setf print () ) 
;  ( loop while ( not ( NULL poslist ) ) do
;    ( setf curpos ( car poslist ) )
;    ( setf poslist ( cdr poslist ) )
;    ( setf move-num ( position curpos gameplay ) )
;    ( if move-num () ( setf print ( snoc '-- print ) ) )   
;    ( if move-num () ( setf move-num 10 ) )
;    ( cond 
;      ( ( = ( mod move-num 2 ) 0 )
;        ( setf symbol 'X )
;      )
;      ( t 
;        ( setf symbol 'O ) 
;      )
;    )
;    ( cond 
;      ( ( eq symbol 'X ) 
;        ( cond 
;          ( ( = move-num 0 )  
;            ( setf print ( snoc 'X1 print ) )
;          )
;          ( ( = move-num 2 )  
;            ( setf print ( snoc 'X2 print ) )
;          )
;          ( ( = move-num 4 )  
;            ( setf print ( snoc 'X3 print ) )
;          )
;          ( ( = move-num 6 )  
;            ( setf print ( snoc 'X4 print ) )
;          )
;          ( ( = move-num 8 )  
;            ( setf print ( snoc 'X5 print ) )
;          )
;        )
;      )
;      ( t
;        ( cond 
;          ( ( = move-num 1 )  
;            ( setf print ( snoc 'O1 print ) )
;          )
;          ( ( = move-num 3 )  
;            ( setf print ( snoc 'O2 print ) )
;          )
;          ( ( = move-num 5 )  
;            ( setf print ( snoc 'O3 print ) )
;          )
;          ( ( = move-num 7 )  
;            ( setf print ( snoc 'O4 print ) )
;          )
;        ) 
;      )
;    )
;  )
;  ;now to print the list
;  ( format t "~%~A ~A ~A ~%~A ~A ~A ~%~A ~A ~A"
;    ( nth 0 print )
;    ( nth 1 print )
;    ( nth 2 print )
;    ( nth 3 print )
;    ( nth 4 print )
;    ( nth 5 print )
;    ( nth 6 print )
;    ( nth 7 print )
;    ( nth 8 print )
;  )
;)
