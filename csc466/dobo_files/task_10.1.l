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
