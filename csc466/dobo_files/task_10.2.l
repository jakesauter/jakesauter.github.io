;; Task 10.2: Modeling and Manipulating and Individual
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
