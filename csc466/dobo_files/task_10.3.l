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
