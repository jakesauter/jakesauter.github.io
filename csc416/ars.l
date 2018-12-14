( load "missionaries_and_canniabls_state_space_problem_solver.l" )
( load "accumulator.l" )
( load "name_generator.l" )

( defmethod establish-operators ()
  ( setf *move-cm-lr* 
    ( make-instance 'operator 
      :name 'move-cm-lr
      :precondition "there is a cannibal, a missionary, and a boat on the left bank" 
      :description "move the cannibal and the missionary from the left bank to the right bank, using the boat"
    )
  )
  ( setf *move-cm-rl*
    ( make-instance 'operator 
      :name 'move-cm-rl 
      :precondition "meh" 
      :description "meh"
    )
  )
  ( setf *move-mm-lr*
    ( make-instance 'operator 
      :name 'move-mm-lr 
      :precondition "meh"
      :description "meh"
    )
  )
  ( setf *move-mm-rl*
    ( make-instance 'operator 
      :name 'move-mm-rl 
      :precondition "meh" 
      :description "meh"
    )
  )
  ( setf *move-cc-lr* 
    ( make-instance 'operator 
      :name 'move-cc-lr 
      :precondition "meh" 
      :description "meh" 
    )
  )
  ( setf *move-cc-rl* 
    ( make-instance 'operator 
      :name 'move-cc-rl 
      :precondition "meh" 
      :description "meh"
    ) 
  )
  ( setf *move-m-lr* 
    ( make-instance 'operator 
      :name 'move-m-lr 
      :precondition "meh"
      :description "meh"
    )
  )
  ( setf *move-m-rl* 
    ( make-instance 'operator 
      :name 'move-m-rl 
      :precondition "meh" 
      :description "meh"
    )
   )
  ( setf *move-c-lr* 
    ( make-instance 'operator 
      :name 'move-c-lr 
      :precondition "meh" 
      :description "meh" 
    )
  )
  ( setf *move-c-rl* 
    ( make-instance 'operator 
      :name 'move-c-rl
      :precondition "meh" 
      :description "meh" 
    )
  )
  ; making the list of operators
  ( setf *operator-list* '
    ( *move-cm-lr* 
      *move-cm-rl* 
      *move-mm-lr*
      *move-mm-rl*
      *move-cc-lr*
      *move-cc-rl*
      *move-m-lr* 
      *move-m-rl*
      *move-c-lr*
      *move-c-rl*
    ) 
  )
)

( defmethod children-of ( ( e-node node ) &aux kids ) 
    ( if ( applicable-p *move-cm-rl*( node-state e-node ) ) 
      ( push ( child-of e-node *move-cm-lr* ) kids )    
    )
    ( if ( applicable-p *move-cm-rl* ( node-state e-node ) ) 
      ( push ( child-of e-node *move-cm-rl* ) kids )    
    )
    ( if ( applicable-p *move-mm-lr* ( node-state e-node ) ) 
      ( push ( child-of e-node *move-mm-lr* ) kids )    
    )
    ( if ( applicable-p *move-mm-rl* ( node-state e-node ) )   
      ( push ( child-of e-node *move-mm-rl* ) kids )    
    )
    ( if ( applicable-p *move-cc-lr* ( node-state e-node ) )  
      ( push ( child-of e-node *move-cc-lr* ) kids )    
    )
    ( if ( applicable-p *move-cc-rl* ( node-state e-node ) ) 
      ( push ( child-of e-node *move-cc-rl* ) kids )    
    )
    ( if ( applicable-p *move-m-lr* ( node-state e-node ) ) 
      ( push ( child-of e-node *move-m-lr* ) kids )    
    )
    ( if ( applicable-p *move-m-rl* ( node-state e-node ) ) 
      ( push ( child-of e-node *move-m-rl* ) kids )    
    )
    ( if ( applicable-p *move-c-lr* ( node-state e-node ) )  
      ( push ( child-of e-node *move-c-lr* ) kids )    
    )
    ( if ( applicable-p *move-c-rl* ( node-state e-node ) ) 
      ( push ( child-of e-node *move-c-rl* ) kids )    
    )
    kids
)

( defmethod apply-operator ( ( o operator ) ( s state ) )
  ( cond 
    ( ( eq ( operator-name o ) 'move-cm-lr ) 
      ( move-cm-lr-f s )
    )  
    ( ( eq ( operator-name o ) 'move-cm-rl ) 
      ( move-cm-rl-f s )
    )  
    ( ( eq ( operator-name o ) 'move-mm-lr ) 
      ( move-mm-lr-f s )
    )  
    ( ( eq ( operator-name o ) 'move-mm-rl ) 
      ( move-mm-rl-f s )
    )  
    ( ( eq ( operator-name o ) 'move-cc-lr ) 
      ( move-cc-lr-f s )
    )  
    ( ( eq ( operator-name o ) 'move-cc-rl ) 
      ( move-cc-rl-f s ) 
    )  
    ( ( eq ( operator-name o ) 'move-m-lr ) 
      ( move-m-lr-f s )
    )  
    ( ( eq ( operator-name o ) 'move-m-rl )
      ( move-m-rl-f s )
    )  
    ( ( eq ( operator-name o ) 'move-c-lr ) 
      ( move-c-lr-f s )
    )  
    ( ( eq ( operator-name o ) 'move-c-rl ) 
      ( move-c-rl-f s )
    )
  )  
)

( defmethod applicable-p ( ( o operator ) ( s state ) )
  ( cond 
    ( ( eq ( operator-name o ) 'move-cm-lr ) 
      ( move-cm-lr-p s )
    )  
    ( ( eq ( operator-name o ) 'move-cm-rl ) 
      ( move-cm-rl-p s )
    )  
    ( ( eq ( operator-name o ) 'move-mm-lr ) 
      ( move-mm-lr-p s )
    )  
    ( ( eq ( operator-name o ) 'move-mm-rl ) 
      ( move-mm-rl-p s )
    )  
    ( ( eq ( operator-name o ) 'move-cc-lr ) 
      ( move-cc-lr-p s )
    )  
    ( ( eq ( operator-name o ) 'move-cc-rl ) 
      ( move-cc-rl-p s ) 
    )  
    ( ( eq ( operator-name o ) 'move-m-lr ) 
      ( move-m-lr-p s )
    )  
    ( ( eq ( operator-name o ) 'move-m-rl ) 
      ( move-m-rl-p s )
    )  
    ( ( eq ( operator-name o ) 'move-c-lr ) 
      ( move-c-lr-p s )
    )  
    ( ( eq ( operator-name o ) 'move-c-rl ) 
      ( move-c-rl-p s )
    )  
  )
)

( defmethod move-cm-lr-p ( ( s state ) )
  ( and 
    ( >= ( length ( bank-missionaries ( state-left-bank s ) ) ) 1 ) 
    ( >= ( length ( bank-cannibals ( state-left-bank s ) ) ) 1 )  
    ( eq ( bank-boat ( state-left-bank s ) ) 'b )
  )
)

( defmethod move-cm-lr-f ( ( s state ) ) 
  ( setf ( bank-cannibals ( state-left-bank s ) ) ( remove 'c ( bank-cannibals ( state-left-bank s ) ) :count 1 ) )
  ( setf ( bank-missionaries ( state-left-bank s ) ) ( remove 'm ( bank-missionaries ( state-left-bank s ) ) :count 1 ) )
  ( setf ( bank-boat ( state-left-bank s ) ) NIL )
  ( setf ( bank-cannibals ( state-right-bank s ) ) ( append '(c) ( bank-cannibals ( state-right-bank s ) ) ) )
  ( setf ( bank-missionaries ( state-right-bank s ) ) ( append '(m) ( bank-missionaries ( state-right-bank s ) ) ) )
  ( setf ( bank-boat ( state-right-bank s ) ) 'b )
)

( defmethod move-cm-rl-p ( ( s state ) )
  ( and 
    ( >= ( length ( bank-missionaries ( state-left-bank s ) ) ) 1 ) 
    ( >= ( length ( bank-cannibals ( state-left-bank s ) ) ) 1 )  
    ( eq ( bank-boat ( state-left-bank s ) ) 'b )
  )
)

( defmethod move-cm-rl-f ( ( s state ) )
  ( setf ( bank-cannibals ( state-right-bank s ) ) ( remove 'c ( bank-cannibals ( state-right-bank s ) ) :count 1 ) )
  ( setf ( bank-missionaries ( state-right-bank s ) ) ( remove 'm ( bank-missionaries ( state-right-bank s ) ) :count 1 ) )
  ( setf ( bank-boat ( state-right-bank s ) ) NIL )
  ( setf ( bank-cannibals ( state-left-bank s ) ) ( append '(c) ( bank-cannibals ( state-left-bank s ) ) ) )
  ( setf ( bank-missionaries ( state-left-bank s ) ) ( append '(m) ( bank-missionaries ( state-left-bank s ) ) ) )
  ( setf ( bank-boat ( state-left-bank s ) ) 'b )
)

( defmethod move-mm-lr-p ( ( s state ) )
  ( and
    ( >= ( length ( bank-missionaries ( state-left-bank s ) ) ) 1 ) 
    ( >= ( length ( bank-cannibals ( state-left-bank s ) ) ) 1 )  
    ( eq ( bank-boat ( state-left-bank s ) ) 'b )
  )
)

( defmethod move-mm-lr-f ( ( s state ) )
  ( setf ( bank-missionaries ( state-left-bank s ) ) ( remove 'm ( bank-missionaries ( state-left-bank s ) ) :count 2 ) )
  ( setf ( bank-boat ( state-right-bank s ) ) nil )
  ( setf ( bank-missionaries ( state-right-bank s ) ) ( append '(m m) ( bank-missionaries ( state-right-bank s ) ) ) )
  ( setf ( bank-boat ( state-right-bank s ) ) 'b )
)

( defmethod move-mm-rl-p ( ( s state ) )
  ( and
    ( >= ( length ( bank-missionaries ( state-left-bank s ) ) ) 1 ) 
    ( >= ( length ( bank-cannibals ( state-left-bank s ) ) ) 1 )  
    ( eq ( bank-boat ( state-left-bank s ) ) 'b )
  )
)

( defmethod move-mm-rl-f ( ( s state ) )
  ( setf ( bank-missionaries ( state-right-bank s ) ) ( remove 'm ( bank-missionaries ( state-right-bank s ) ) :count 2 ) )
  ( setf ( bank-boat ( state-right-bank s ) ) nil )
  ( setf ( bank-missionaries ( state-left-bank s ) ) ( append '(m m) ( bank-missionaries ( state-left-bank s ) ) ) )
  ( setf ( bank-boat ( state-left-bank s ) ) 'b )
)

( defmethod move-cc-lr-p ( ( s state ) )
  ( and 
    ( >= ( length ( bank-missionaries ( state-left-bank s ) ) ) 1 ) 
    ( >= ( length ( bank-cannibals ( state-left-bank s ) ) ) 1 )  
    ( eq ( bank-boat ( state-left-bank s ) ) 'b )
  )
)

( defmethod move-cc-lr-f ( ( s state ) )
  ( setf ( bank-missionaries ( state-left-bank s ) ) ( remove 'c ( bank-cannibals ( state-left-bank s ) ) :count 2 ) )
  ( setf ( bank-boat ( state-left-bank s ) ) nil )
  ( setf ( bank-missionaries ( state-right-bank s ) ) ( append '(c c) ( bank-cannibals ( state-right-bank s ) ) ) )
  ( setf ( bank-boat ( state-right-bank s ) ) 'b )
)

( defmethod move-cc-rl-p  ( ( s state ) )
  ( and
    ( >= ( length ( bank-missionaries ( state-left-bank s ) ) ) 1 ) 
    ( >= ( length ( bank-cannibals ( state-left-bank s ) ) ) 1 )  
    ( eq ( bank-boat ( state-left-bank s ) ) 'b )
  )
)

( defmethod move-cc-rl-f ( ( s state ) )
  ( setf ( bank-missionaries ( state-right-bank s ) ) ( remove 'c ( bank-missionaries ( state-right-bank s ) ) :count 2 ) )
  ( setf ( bank-boat ( state-right-bank s ) ) nil )
  ( setf ( bank-missionaries ( state-left-bank s ) ) ( append '(c c) ( bank-missionaries ( state-left-bank s ) ) ) )
  ( setf ( bank-boat ( state-left-bank s ) ) 'b )
)

( defmethod move-m-lr-p ( ( s state ) ) 
  ( and 
    ( >= ( length ( bank-missionaries ( state-left-bank s ) ) ) 1 ) 
    ( >= ( length ( bank-cannibals ( state-left-bank s ) ) ) 1 )  
    ( eq ( bank-boat ( state-left-bank s ) ) 'b )
  )
)

( defmethod move-m-lr-f ( ( s state ) )
  ( setf ( bank-missionaries ( state-left-bank s ) ) ( remove 'm ( bank-missionaries ( state-left-bank s ) ) :count 1 ) )
  ( setf ( bank-boat ( state-left-bank s ) ) nil )
  ( setf ( bank-missionaries ( state-right-bank s ) ) ( append '(m) ( bank-missionaries ( state-right-bank s ) ) ) )
  ( setf ( bank-boat ( state-right-bank s ) ) 'b )
)

( defmethod move-m-rl-p ( ( s state ) )
  ( and
    ( >= ( length ( bank-missionaries ( state-left-bank s ) ) ) 1 ) 
    ( >= ( length ( bank-cannibals ( state-left-bank s ) ) ) 1 )  
    ( eq ( bank-boat ( state-left-bank s ) ) 'b )
  )
)

( defmethod move-m-rl-f ( ( s state ) )
  ( setf ( bank-missionaries ( state-right-bank s ) ) ( remove 'm ( bank-missionaries ( state-right-bank s ) ) :count 1 ) )
  ( setf ( bank-boat ( state-right-bank s ) ) nil )
  ( setf ( bank-missionaries ( state-left-bank s ) ) ( append '(m) ( bank-missionaries ( state-left-bank s ) ) ) ) 
  ( setf ( bank-boat ( state-left-bank s ) ) 'b )
)

( defmethod move-c-lr-p ( ( s state ) )
  ( and 
    ( >= ( length ( bank-missionaries ( state-left-bank s ) ) ) 1 ) 
    ( >= ( length ( bank-cannibals ( state-left-bank s ) ) ) 1 )  
    ( eq ( bank-boat ( state-left-bank s ) ) 'b )
  )
)

( defmethod move-c-lr-f ( ( s state ) )
  ( setf ( bank-missionaries ( state-left-bank s ) ) ( remove 'c ( bank-missionaries ( state-left-bank s ) ) :count 1 ) )
  ( setf ( bank-boat ( state-left-bank s ) ) nil )
  ( setf ( bank-missionaries ( state-right-bank s ) ) ( append '(c) ( bank-missionaries ( state-right-bank s ) ) ) )
  ( setf ( bank-boat ( state-right-bank s ) ) 'b )
)

( defmethod move-c-rl-p ( ( s state ) )
  ( and
    ( >= ( length ( bank-missionaries ( state-left-bank s ) ) ) 1 ) 
    ( >= ( length ( bank-cannibals ( state-left-bank s ) ) ) 1 )  
    ( eq ( bank-boat ( state-left-bank s ) ) 'b )
  )
)

( defmethod move-c-rl-f ( ( s state ) )
  ( setf ( bank-missionaries ( state-right-bank s ) ) ( remove 'c ( bank-missionaries ( state-right-bank s ) ) :count 1 ) )
  ( setf ( bank-boat ( state-right-bank s ) ) nil )
  ( setf ( bank-missionaries ( state-left-bank s ) ) ( append '(c) ( bank-missionaries ( state-left-bank s ) ) ) )
  ( setf ( bank-boat ( state-left-bank s ) ) 'b )
) 

( defmethod display-solution ( ( n node ) )
  ( cond 
    ( ( rootp n )
      ( terpri )
    )
    ( t
      ( display-solution ( node-parent n ) )
      ( princ ( operator-description ( node-operator n ) ) )
    )
  )
)

( defmethod display-explored-nodes ()
  ( prin1 'EXPLORED ) ( terpri ) ( terpri ) 
  ( mapcar #'display *explored* )
  ( terpri )
  NIL
)

( defmethod display-unexplored-nodes ()
  ( prin1 'UNEXPLORED ) ( terpri ) ( terpri ) 
  ( mapcar #'display *unexplored* )
  ( terpri )
  NIL
)

( defmethod rootp ( ( n node ) )
  ( eq ( node-name n ) 'root )
)

( defmethod goalp ( ( n node ) )
      ( and 
        ( eq ( bank-boat ( state-right-bank ( node-state n ) ) ) 'b )
        ( = ( length ( bank-missionaries ( state-right-bank ( node-state n ) ) ) ) 3 )
        ( = ( length ( bank-cannibals ( state-right-bank ( node-state n ) ) ) ) 3 )
      )
)

( defmethod exploredp ( ( n node ) )
  ( member-node-p n *explored* )
)

( defmethod member-node-p ( ( n node ) ( lst list ) )
  ( cond 
    ( ( null lst )
      nil
    )
    ( ( equal-node-p n ( first lst ) )
      t
    )
    ( t
      ( member-node-p n ( rest lst ) )
    )
  )
)

( defmethod equal-node-p ( ( n1 node ) ( n2 node ) )
  ( and 
    ( equal-bank-p ( state-right-bank ( node-state n1 ) ) ( state-right-bank ( node-state n2 ) ) )
    ( equal-bank-p ( state-left-bank ( node-state n1 ) ) ( state-left-bank ( node-state n2 ) ) )
  )
)

( defmethod equal-bank-p ( ( b1 bank ) ( b bank ) )
  ( and 
    ( equal ( bank-boat b1 ) ( bank-boat b2 ) )
    ( equal ( bank-missionaries b1 ) ( bank-missionaries b2 ) )
    ( equal ( bank-cannibals b1 ) ( bank-cannibals b2 ) )
  )
)
