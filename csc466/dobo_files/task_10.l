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

