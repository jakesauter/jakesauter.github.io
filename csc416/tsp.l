( load "lp.l" )

;a tour is a list of letters (26 possible cities)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Making the map
;the map will be a list of property lists
 ;each point on the map will be a list of distances to every other point
( defmethod init-map ()
  ( setf ( symbol-plist 'a ) '( b 900 c 600 d 700 e 600 ) )
  ( setf ( symbol-plist 'b ) '( a 900 c 800 d 1100 e 1700 ) )
  ( setf ( symbol-plist 'c ) '( a 600 b 800 d 700 e 1500 ) )
  ( setf ( symbol-plist 'd ) '( a 700 b 1100 c 700 e 600 ) )
  ( setf ( symbol-plist 'e ) '( d 600 c 1500 b 1700 a 600 ) )
  ( list 'a 'b 'c 'd 'e )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Modelling an individual
  ; a tour will be a list of property lists
  ; the cost of one movement from city i to i+1 is the cost of property
    ; of the city i+1 on the propertly list i
( defclass individual ()
  (
    ( tour :accessor individual-tour :initarg :tour :initform () )
    ( fitness :accessor individual-fitness :initarg :fitness )
    ( number :accessor individual-number :initarg :number )
  )
)

;demo

( defmethod individual-demo ( &aux i0 i1 i2 i3 one two three )
  ( setf i0 ( random-individual ) )
  ( display i0 )
  ( setf one ( individual-tour i0 ) )
  ( setf i1 ( random-individual ) )
  ( display i1 )
  ( setf two ( individual-tour i1 ) )
  ( setf i2 ( random-individual ) )
  ( display i2 )
  ( setf three ( individual-tour i2 ) )
  ( setf i3 ( random-individual ) )
  ( display i3 )
  ( setf four ( individual-tour i3 ) )
  ( format t "Fitness of i0 = ~A~%" ( fitness one ) )
  ( format t "Fitness of i1 = ~A~%" ( fitness two ) )
  ( format t "Fitness of i2 = ~A~%" ( fitness three ) )
  ( format t "Fitness of i3 = ~A~%" ( fitness four ) )
)


;method to produce a random individual
( defmethod random-individual ( &aux individual ) 
  ( setf individual () )
  ( let* ( ( tour ( random-tour ) ) ) 
    ( make-instance 'individual
      :tour tour
      :fitness ( fitness tour )
      :number 0 
    )
  ) 
)

( defmethod new-individual ( ( nr number ) ( tour list ) )
  ( make-instance 'individual
    :tour tour 
    :fitness ( fitness tour )
    :number nr 
  )
)

;helper function for random-individual that returns a random 
 ;tour
( defun random-tour ()
  ( setf tour () )
  ( setf points ( init-map ) )
  ( loop ;generating the random tour of all points
    ( let (i) 
      ( setf i ( random ( length points ) ) )
      ( push ( nth i points ) tour ) 
      ( setf points ( remove ( nth i points ) points :count 1 ) )
      ( if ( null points ) ( return-from random-tour tour ) )
    )
  )
)

( defmethod fitness ( ( tour list ) )
  ( let* ( ( sum 0 ) ( first ( car tour ) ) ) 
    ( loop ; loop to sum the distance of the tour
      ( setf sum ( + sum ( get ( nth 0 tour ) ( nth 1 tour ) ) ) )
      ( setf tour ( remove ( nth 0 tour ) tour :count 1 ) )
      ( if ( singleton-p tour ) ( return-from fitness ( + sum ( get ( nth 0 tour ) first ) ) ) )
    )
  )
)

( defmethod display ( ( i individual ) )
  ( display-nnl i ) ( terpri ) 
)

( defmethod display-nnl ( ( i individual ) )
  ( princ ( individual-number i ) )
  ( princ ( filler ( individual-number i ) ) )
  ( prin1 ( individual-tour i ) )
  ( princ "  " )
  ( prin1 ( individual-fitness i ) )
  ( princ ( filler ( individual-fitness i ) ) )
)

( defmethod filler ( ( n number ) )
  ( cond 
    ( ( < n 10 ) "     " )
    ( ( < n 100 ) "    " )
    ( ( < n 1000 ) "   " )
    ( ( < n 10000 ) "  " ) 
    ( ( < n 100000 ) " " )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Defining Mutation
 ; this method will take a tour (list) and swap two randomly selected elements
( defun mutate ( ( i individual ) )
  ( setf l ( copy-list ( individual-tour i ) ) )
  ( setf p1 ( random ( length l ) ) )
  ( setf p2 ( random ( length l ) ) )
  ( setf temp ( nth p1 l ) )
  ( setf ( nth p1 l ) ( nth p2 l ) )
  ( setf ( nth p2 l ) temp )
  ( new-individual  0 l )
)  

;demo
( defmethod mutation-demo ( &aux s m ) 
  ( setf s ( random-individual ) )
  ( dotimes ( i 10 )
    ( format t "s = ~A~%" ( individual-tour s ) )
    ( setf m ( mutate s ) )
    ( format t "m = ~A~%" ( individual-tour m ) )
  )
)
    


( defconstant *pc-m* 50 )

( defmethod maybe-mutate ( ( i individual ) )
  ( setf x ( random 100 ) )
  ( cond 
    ( ( <= ( + 1 x ) *pc-m* ) 
      ( mutate i )
    )
    ( T
      i 
    )
  )
)

;demo
( defmethod maybe-mutate-demo () 
  ( setf i ( random-individual ) )
  ( display i )
  ( dotimes ( x 20 )
    ( setf n ( maybe-mutate i ) )
    ( display-nnl n ) 
    ( if ( not ( equal n i ) ) ( princ " *" ) )
    ( terpri )
    ( setf i n )
  )
)

( setf *crossover-trace* nil )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Defining Crossover
 
( defmethod crossover ( ( i1 individual ) ( i2 individual ) )
  ( setf l1 ( individual-tour i1 ) )
  ( setf l2 ( individual-tour i2 ) )
  ( setf child ( copy-list l1 ) )
  ( setf clist ( find-crossover l1 l2 ) )
  ;the crossover will take the returned subset from l2, 
   ;and replace it with the equivalent subset in l1
  ( let* ( ( i ( nth 0 clist ) ) ( j ( nth 1 clist ) ) ( limit  ( nth 2 clist ) ) )
    ( loop ;replacing the equivalent set in l1
      ( if  *crossover-trace* 
        ( let ()
          ( format t "replacing ~A with ~A" 
            ( nth i child ) ( nth j l2 ) ) ( terpri )
        )
      )
      ( setf ( nth i child ) ( nth j l2 ) )
      ( setf i ( + i 1 ) )
      ( setf j ( + j 1 ) )
      ( setf limit ( - limit 1 ) )
      ( if ( = limit 0 ) ( return-from crossover ( new-individual 0 child ) ) )
    )
  )
)

( defmethod crossover-demo ( &aux m f x )
  ( setf m ( random-individual ) )
  ( setf f ( random-individual ) )
  ( dotimes ( i 10 )
    ( format t "m = ~A~%" ( individual-tour m ) )
    ( setf x ( crossover m f ) ) 
    ( format t "x = ~A~%" ( individual-tour x ) )
    ( format t "f = ~A~%" ( individual-tour f ) )
  )
)

;this method finds a crossover between two lists that
 ;finds two equivalent subsets of two tours 
   ;;algorithm: start with the set size to be half
   ;;the size of the individual and go down as the 
   ;;sizes of sets cannot be found
;the return value is a list containing that starting
 ;index in the first list, the starting index in the 
 ;second list and the length of the similar subset
( defun find-crossover ( ( l1 list ) ( l2 list ) )
  ;;change this function to take in individuals and set
   ;l1 and l2 to the tours of the individuals
  ( setf n ( length l1 ) )
  ( setf s ( ceiling ( / n 2 ) ) )
  ( let* ( ( i 0 ) ( j 0 ) )
    ( loop ;looping though all set sizes
      ( setf i 0 )
      (  loop ;looping through starting positions
            ;for composing subsets from set 1
        ( setf sl1 ( sublist i ( + i s ) l1 ) )
        ( setf j 0 )
        ( loop ;looping through starting positions
                 ;for composing subsets from set 2,
                 ;also checking for equivalency to break
          ( setf sl2 ( sublist j ( + j s ) l2 ) )
          ( setf sl1eq ( copy-list sl1 ) )
          ( setf sl2eq ( copy-list sl2 ) )
          ( if ( eqss sl1eq sl2eq ) ( return-from find-crossover ( list i j s ) ) )
          ;;incriment j and check if within bounds to break
          ( setf j ( + j 1 ) )
          ( if ( or ( > ( + j s ) ( length l2 ) ) ) ( return ) )
        )
        ( setf i ( + i 1 ) )
        ( if ( or ( > ( + i s ) ( length l1 ) ) ) ( return ) )
      )
      ( setf s ( - s 1 ) )
      ( if ( < s 1 ) ( return-from find-crossover ( list "failure" ) ) )
    )
  )
)

( defun crossover-test ()
  ( setf i1 ( random-individual ) )
  ( setf i2 ( random-individual ) )
  ( display i1 )
  ( display i2 )
  ( format t "~A" ( crossover i1 i2 ) )
)

;;pick and element from the first list, try finding it
 ;; in the second list, if it is found, delete both and
 ;; move onto the next element in the first list, otherwise
 ;; the sets are not equal and return false
 ;; once there are no elements are left in s1, if there are
 ;; any elements left in s2 then the sets are not equal
( defun eqss ( ( s1 list ) ( s2 list ) )
   ( let* ( ( i 0 ) ( j 0 ) )
     ( loop 
       ( if ( and ( null s1 ) ( null s2 ) ) ( return-from eqss T ) )
       ( if ( or ( null s1 ) ( null s2 ) ) ( return-from eqss nil ) )
       ( if ( = i ( length s1 ) ) ( return-from eqss nil ) )
       ( setf s1elem ( nth i s1 ) )
       ( setf i ( + i 1 ) )
       ( setf j 0 )
       ( loop
         ( setf s2elem ( nth j s2 ) )
         ( setf j ( + j 1 ) )
         ( if ( equal s1elem s2elem ) ;delete from both lists
           ( let () 
             ( setf s1 ( remove s1elem s1 :count 1 ) )
             ( setf s2 ( remove s2elem s2 :count 1 ) )
             ( setf i ( - i 1 ) )
             ( setf j ( - j 1 ) )
             ( return )
           )
         )
         ( if ( = j ( length s2 ) )
           ( return )
         )
       )
     )
   )
)

;helper function to generate a sublist from x to y in list z
( defun sublist ( ( x integer ) ( y integer ) ( z list ) )
  ( let* ( ( i x ) ( j y ) ( l ( copy-list z ) ) )
    ( setf rl ( list ( nth i l ) ) )
    ( setf i ( + i 1 ) )
    ( loop
      ( if ( = i j ) ( return-from sublist rl ) )
      ( setf rl ( append rl ( list ( nth i l ) ) ) )
      ( setf i ( + i 1 ) )
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Modeling a population

( defconstant *population-size* 100 )
( defconstant *selection-size* 10 )

( defclass population ()
  (
    ( individuals :accessor population-individuals :initarg :individuals )
    ( generation :accessor population-generation :initform 0 )
  )
)

;demo
( defmethod population-demo ( &aux p )
  ( setf p ( initial-population ) )
  ( display p )
  ( format t "Average fitness = ~A~%~%" ( average p ) )
  ( setf *select-demo* t )
  ( format t "Sampling ...~%~%" )
  ( select-individual p ) ( terpri ) 
  ( format t "Sampling ...~%~%" )
  ( select-individual p ) ( terpri ) 
  ( format t "Sampling ...~%~%" )
  ( select-individual p ) ( terpri ) 
)

( defmethod size ( ( p population ) )
  ( length ( population-individuals p ) )
)

( defmethod display ( ( p population ) )
  ( terpri ) ( terpri )
  ( princ "Generation " )
  ( prin1 ( population-generation p ) )
  ( princ " population . . . " )
  ( terpri ) ( terpri )
  ( dolist ( i ( population-individuals p ) )
    ( display i )
  )
  ( terpri )
)

( defmethod initial-population ( &aux individuals )
  ( setf individuals () )
  ( dotimes ( i *population-size* ) 
    ( push ( new-individual ( + i 1 ) ( random-tour ) ) individuals )
  )
  ( make-instance 'population :individuals ( reverse individuals ) )
)

( defmethod average ( ( p population ) &aux ( sum 0 ) )
  ( setf individuals ( population-individuals p ) )
  ( dotimes ( i *population-size* )
    ( setf sum ( + sum ( individual-fitness ( nth i individuals ) ) ) )
  )
  ( / sum *population-size* )
)

( setf *select-demo* nil )

( defmethod select-individual ( ( p population ) &aux mfi candidates )
  ( setf candidates ( select-individuals p ) )
  ( setf mfi ( most-fit-individual candidates ) )
  ( if *select-demo* ( select-demo-helper candidates mfi ) )
  mfi 
)

( defmethod select-demo-helper ( ( l list ) ( i individual ) )
  ( princ "the sample of individuals ..." ) ( terpri )
  ( mapcar #'display l )
  ( terpri )
  ( princ "the most fit of the sample ..." ) ( terpri ) 
  ( display i )
  ( terpri ) 
  nil 
)

( defmethod select-individuals ( ( p population ) &aux individuals candidates rn )
  ( setf individuals ( population-individuals p ) )
  ( setf candidates () )
  ( dotimes ( i *selection-size* )
    ( setf rn ( random *population-size* ) ) 
    ( push ( nth rn individuals ) candidates )
  )
  candidates
)

( defmethod most-fit-individual ( ( l list ) &aux max-value max-individual )
  ( setf min-individual ( car l ) )
  ( setf min-value ( individual-fitness min-individual ) )
  ( dotimes ( i ( length l ) )
    ( if ( < ( individual-fitness ( nth i l ) ) min-value )
      ( let ()
        ( setf min-individual ( nth i l ) )
        ( setf min-value ( individual-fitness min-individual ) )
      )
    )
  )
  min-individual
) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

( defmethod maybe-mutate-demo () 
  ( setf i ( random-individual ) )
  ( display i )
  ( dotimes ( x 20 )
    ( setf n ( maybe-mutate i ) )
    ( display-nnl n ) 
    ( if ( not ( equal n i ) ) ( princ " *" ) )
    ( terpri )
    ( setf i n )
  )
)


;copy 
( setf *copy-demo* nil )

( defconstant *pc-c* 40 )

( defmethod perform-copies ( ( cp population ) ( np population ) )
  ( dotimes ( i ( nr-copies ) )
    ( perform-one-copy cp np )
  )
)

( defmethod nr-copies () 
  ( * ( / *pc-c* 100 ) *population-size* )
)

( defmethod perform-one-copy ( ( cp population ) ( np population ) &aux x m mm new-i )
  ( setf m ( select-individual cp ) )
  ( if *copy-demo* 
    ( let ()
      ( format t "Selected individual = ~%" ) 
      ( display m )
    )
  )
  ( setf mm ( maybe-mutate m ) )
  ( if *copy-demo*
    ( let ()
      ( format t "Possibly mutated individual = ~%" )
      ( display mm )
    )
  )
  ( setf ( individual-number mm ) ( + 1 ( size np ) ) )
  ( if *copy-demo*
    ( let ()
      ( format t "Renumbered individual = ~%" ) 
      ( display mm ) 
    )
  )
  ( setf new-i ( new-individual ( + 1 ( size np ) ) ( individual-tour mm ) ) )
  ( setf 
    ( population-individuals np )
    ( append ( population-individuals np ) ( list new-i ) )
  )
  nil
)

( defmethod empty-population ( ( cp population ) &aux np )
  ( setf np ( make-instance 'population ) )
  ( setf ( population-individuals np ) () )
  ( setf ( population-generation np ) ( + 1 ( population-generation cp ) ) )
  np 
)

;copy demo
( defmethod perform-copies-demo ( &aux cp np )
  ( setf cp ( initial-population ) )
  ( setf np ( empty-population cp ) )
  ( terpri ) ( display np ) ( terpri ) ( terpri )
  ( setf *select-demo* t )
  ( setf *copy-demo* t )
  ( dotimes ( i 10 )
    ( perform-one-copy cp np )
    ( terpri ) ( display np ) ( terpri ) ( terpri )
  )
  ( setf *select-demo* nil )
  ( setf *copy-demo* nil )
  nil 
)

;crossover
( setf *crossover-demo* nil )

( defconstant *pc-x* 60 )

( defmethod perform-crossovers ( ( cp population ) ( np population ) )
  ( dotimes ( i ( nr-crossovers ) )
    ( perform-one-crossover cp np )
  )
)

( defmethod nr-crossovers ()
  ( * ( / *pc-x* 100 ) *population-size* )
)

( defmethod perform-one-crossover ( ( cp population ) ( np population ) )
 ( let ( x m mm mother father new-i )
   ( setf mother ( select-individual cp ) )
   ( setf father ( select-individual cp ) )
   ( if *crossover-demo*
     ( let ()
       ( format t "Selected mother = ~%" ) 
       ( display mother )
       ( format t "Selected father = ~%" ) 
       ( display father )
     )
   )
   ( setf m ( crossover mother father ) )
   ( if *crossover-demo*
     ( let ()
       ( format t "the crossover = ~%" )
       ( display m ) 
     )
   )
   ( setf mm ( maybe-mutate m ) )
   ( if *crossover-demo* 
     ( let ()
       ( format t "the possibly mutated-individual = ~%" )
       ( display mm )
     )
   )
   ( setf ( individual-number mm ) ( + 1 ( size np ) ) )
   ( if *crossover-demo* 
     ( let ()
       ( format t "the renumbered individual = ~%" ) 
       ( display mm )
     )
   )
   ( setf new-i ( new-individual ( + 1 ( size np ) ) ( individual-tour mm ) ) )
   ( setf 
     ( population-individuals np ) 
     ( append ( population-individuals np ) ( list new-i ) )
   )
  )
)

;crossover demo
( defmethod perform-crossovers-demo ( &aux cp np )
  ( setf cp ( initial-population ) )
  ( setf np ( empty-population cp ) )
  ( terpri ) ( display np ) ( terpri ) ( terpri )
  ( setf *select-demo* t )
  ( setf *crossover-demo* t )
  ( dotimes ( i 10 )
    ( perform-one-crossover cp np )
    ( terpri ) ( display np ) ( terpri ) ( terpri ) 
  )
  ( setf *select-demo* nil )
  ( setf *crossover-demo* nil )
  nil
)

;;The Genetic Algorithm 
( defmethod ga-text-demo ( &aux p )
  ( setf p ( initial-population ) )
  ( terpri )
  ( summarize p )
  ( dotimes ( i *nr-generations* )
    ( setf p ( next-generation p ) )
    ( check-average p )
  )
  ( terpri )
  ( summarize p )
)

;;providing information on progress of generations
( defmethod summarize ( ( p population ) )
  ( display p )
  ( check-average p ) 
  ( terpri ) 
)

( defmethod check-average ( ( p population ) )
  ( format t "average fitness of population ~A = ~f~%"
    ( population-generation p )
    ( setf ap ( average p ) )
  )
  ap 
)

;;producing the next generation
( defconstant *nr-generations* 25 )

( defmethod next-generation ( ( cp population ) &aux np )
  ( setf np ( empty-population cp ) )
  ( perform-copies cp np )
  ( perform-crossovers cp np ) 
  np
)
