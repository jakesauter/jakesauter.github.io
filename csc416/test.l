( defun find-crossover ( ( l1 list ) ( l2 list ) )
  ( write-line "finding crossover . . ." )
  ( setf n ( length l1 ) )
  ( setf s ( floor ( / n 2 ) ) )
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

