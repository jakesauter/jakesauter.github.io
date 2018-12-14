( setf crossover-trace t )

;;pick and element from the first list, try finding it
 ;; in the second list, if it is found, delete both and
 ;; move onto the next element in the first list, otherwise
 ;; the sets are not equal and return false
 ;; once there are no elements are left in s1, if there are
 ;; any elements left in s2 then the sets are not equal
( defun eqss ( ( s1 list ) ( s2 list ) )
  ( if crossover-trace ( format t "comparing s1: ~A and s2: ~A" s1 s2) ( terpri ) )
   ( loop
     ( setf i 0 )
     ( if ( null s1 ) 
       ( if ( null s2 )
          ( return-from eqss T )
       )
       ( return-from eqss nil )
     )
     ( setf s1elem ( nth i s1 ) )
     ( setf i ( + i 1 ) )
     ( setf j 0 )
     ( loop
       ( setf s2elem ( nth j s2 ) )
       ( setf j ( + j 1 ) )
       ( if ( equal s1elem s2elem ) ;delete from both lists
         ( let ()           
           ( write-line "removing" ) 
           ( setf s1 ( remove s1elem s1 :count 1 ) )
           ( setf s2 ( remove s2elem s2 :count 1 ) )
           ( return )
         )
       ) 
       ( if ( = j ( length s2 ) ) ( return ) )
     )
   )
)

