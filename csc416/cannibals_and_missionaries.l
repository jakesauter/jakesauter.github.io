( defun mc ()
	( establish-world )
	( init-move-list )
	( make-moves )
)

( defun establish-world () 
	( setf *left-bank* '( M M M C C C B ) )
	( setf *right-bank* '() )
)

( defun init-move-list ()
	( setf *move-list* () )
)

( defun make-moves ()
	( display-world )
	( cond 
		( ( goalp ) 
			( write-line "good work!" )
			nil
		)
	 	( ( feast-state-p )
			( write-line "yummy yummy yummy, I got Good in my tummy!!" )
			nil
		)
		( t
			( let ( ( m ( read ) ) )
				( if ( applicable-p m )
					( let () ( perform-move m) ( make-moves ) )
					( let () ( write-line "move inapplicable" ) nil )
				)
			)
		)
	)
)

( defun perform-move ( move ) 
	( setf *move-list* ( snoc move *move-list* ) )
	( if ( equal ( current-bank ) *left-bank* )
		( move-lr move )
		( move-rl move )
	)
)

( defun move-lr ( ml )
	( if ( null ml ) ( return-from move-lr ) )
	( move-lr-l ( first ml ) )
	( move-lr ( rest ml ) )
)

( defun move-rl ( ml )
	( if ( null ml ) ( return-from move-rl ) )
	( move-rl-l ( first ml ) )
	( move-rl ( rest ml ) )
)

( defun display-world ()
	( cond
   		( ( equal 0 ( length *left-bank* ) )
     			( write-line "*left-bank* NIL" ) )
   		( t
    			( format t "left bank: ~{~A~^, ~}.~%" *left-bank* ) 
   		)
	)
  	( cond
    		( ( equal 0 ( length *right-bank* ) )
     			( write-line "*right-bank* NIL" ) )
   		( t
    			( format t "right bank: ~{~A~^, ~}.~%" *right-bank*)
   		)	
  	)
)

( defun goalp ()
	( and ( equal ( count-type *right-bank* 'M ) 3 )
        ( equal ( count-type *right-bank* 'C ) 3 ) )
)

(defun feast-state-p ()
	( or 
		( and 
			( > ( count-type *left-bank* 'M ) 0 )
			( <
		       		( count-type *left-bank* 'M )
				( count-type *left-bank* 'C)
			)
		)
        	(and
		        (> (count-type *right-bank* 'M) 0)
			(<
			        (count-type *right-bank* 'M)
				(count-type *right-bank* 'C)
			)
		)
        )			    
  )

(defun move-lr-l (m)
	( cond
		( ( equal m 'B) 
    			( setf *left-bank* ( remove 'B *left-bank* :count 1 ) )
    			( setf *right-bank* ( cons 'B *right-bank* ) ) )
   		( t
    			(setf *left-bank* (remove m *left-bank* :count 1))
   		        (setf *right-bank* ( cons m *right-bank* ) ) 	
		)
   	)	
)

(defun move-rl-l (m)
	( cond
		( ( equal m 'B) 
   			(setf *right-bank* (remove 'B *right-bank* :count 1))
    			(setf *left-bank* (cons 'B *left-bank*)))
   		( t
    			( setf *right-bank* ( remove m *right-bank* :count 1 ) )
    			( setf *left-bank* ( cons m *left-bank* ) ) 
		) 
        )
)

( defun applicable-p (m)
	( and
		( >= ( count-type ( current-bank ) 'C)
      	        ( count-type m 'C ) )
   		( >= (count-type ( current-bank ) 'M)
    		   (count-type m 'M)
		)
   		( > 4 ( length m ) )
  	        ( = 1 ( count-type m 'B ) )
   	)
)

( defun current-bank()
	( cond
		( ( = ( count-type *left-bank* 'B ) 1 ) *left-bank* )
  	        (t *right-bank*)
   	)
  )

( defun count-type (lst e)
	( cond
		( ( equal ( length lst ) 0 ) 0 )
   		( ( and ( singleton-p lst ) ( equal ( car lst ) e ) ) 1 )
  	        ( ( singleton-p lst ) 0 )
   		( ( equal ( car lst ) e )
    		( + ( count-type ( cdr lst ) e ) 1 ) )
  	        ( t ( + ( count-type ( cdr lst ) e ) 0 ) )
    	)
)

( defun display-solution ()
	(cond
   		( ( = 0 ( length *move-list* ) ) nil )
   		( t ( dispaly-helper *move-list* ) )
	)
)

( defun dispaly-helper (lst)
	( cond
   		( ( singleton-p lst ) (format t "~{~A~^ ~}~%" ( car lst ) ) )
  		( t ( format t "~{~A~^ ~}~%" ( rdc lst ) ) ) ( display-helper  ( rac lst ) ) 
	)
)
		 
