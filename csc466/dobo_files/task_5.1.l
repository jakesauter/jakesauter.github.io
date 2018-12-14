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

