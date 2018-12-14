; Program Description
;
; Missionaries and Cannibals State Space Problem Solver
;
; This program is a state space problem solver for the classic
;    "Missionaries and Cannibals" problem. An explicit state
;    space graph is grown in concert with breadth first search
;    for a solution.
;
; Banks are represented as a 3-slot class consisting of
;   missionaries , cannibals, and a boat.
;
; States are represented as a 2-slot class consisting of 
;   left-bank and right-bank
;
; Operators are represented as a 3-slot class consisting of a name, 
;   a precondition, and a description
;
; Nodes are represented as a 4-slot class consisting of
;   a state, a parent node, a move (state space operator) , and a name.
;-------------------------------------------------------------------------
; Modelling a bank

  ( defclass bank ()
    (
      ( missionaries :accessor bank-missionaries :initarg :missionaries )
      ( cannibals :accessor bank-cannibals :initarg :cannibals )
      ( boat :accessor bank-boat :initarg :boat )
    )
   )

  ( defmethod copy-bank ( ( b bank ) )
    ( make-instance 'bank :missionaries ( bank-missionaries b ) :cannibals ( bank-cannibals b ) :boat ( bank-boat b ) )
  )

  ( defmethod display ( ( b bank ) )
    ( format t "~A" ( append (bank-missionaries b) (bank-cannibals b) (bank-boat b ) ) )
    nil
  )
;-------------------------------------------------------------------------
; Modelling a state

  ( defclass state ()
    ( 
      ( left-bank :accessor state-left-bank :initarg :left-bank :initform () )
      ( right-bank :accessor state-right-bank :initarg :right-bank :initform () )
    )
  )

  ( defmethod copy-state ( ( s state ) )
    ( make-instance 'state 
      :left-bank ( copy-bank ( state-left-bank s ) ) 
      :right-bank ( copy-bank ( state-right-bank s ) )
    )
  )

  ( defmethod display ( ( s state ) )
    ( terpri ) 
    ( princ "left: " )
    ( display ( state-left-bank s ) )
    ( terpri )
    ( princ "right: " )
    ( display ( state-right-bank s ) )
    ( terpri )
    nil
  )

;------------------------------------------------------------------------
; Modelling a node

  ( defclass node () 
    (
      ( name :accessor node-name :initarg :name )
      ( state :accessor node-state :initarg :state )
      ( parent :accessor node-parent :initarg :parent )
      ( operator :accessor node-operator :initarg :operator )
    )
  ) 

  ( defmethod display ( ( n node ) )
    ( princ "node name: " )
    ( format t "~A ~%" ( node-name n ) )
    ( if ( not ( rootp n ) )
      ( let ()
        ( princ "node parent: " )
        ( format t "~A ~%" ( node-name ( node-parent n ) ) ) 
        ( princ "node operator: " )
        ( display ( node-operator n ) )
      )
    )
    ;( terpri )
    ( display ( node-state n ) )
    ( terpri )
    nil
  )

;-------------------------------------------------------------------------
; Modelling an operator

  ( defclass operator ()
    (
      ( name :accessor operator-name :initarg :name )
      ( precondition :accessor operator-precondition :initarg :precondition )
      ( description :accessor operator-description :initarg :description )
    )
  )

  ( defmethod describe-operators ()
    ( mapcar #'describe-operator *operator-list* )
    ( terpri )
    NIL
  )

  ( defmethod describe-operator ( ( op operator ) )
    ( format t "Operator name: ~A" ( operator-name op ) )
    ( format t "Operator precondition: ~A" ( operator-precondition op ) )
    ( format t "Operator description: ~A" ( operator-description op ) )
  )

  ( defmethod display ( ( o operator ) )
    ( format t "~A" ( operator-name o ) )
  ) 

;-------------------------------------------------------------------------
; Main method

  ( defmethod mc ()
    ( establish-operators )
    ( setup )
    ( solve )
  )		

;-------------------------------------------------------------------------
; The setup

  ( defmethod setup ( &aux root lb rb istate )
    ;; establish root node
    ( setf lb ( make-instance 'bank :missionaries '(m m m) :cannibals '(c c c) :boat 'b ) )
    ( setf rb ( make-instance 'bank :missionaries '() :cannibals '() :boat nil ) )
    ( setf istate ( make-instance 'state
      :left-bank lb
      :right-bank rb 
      )
    )
    ( setf root ( make-instance 'node
      :state istate
      :name 'root
      )
    )
   
    
    ;; initialize list of unexplored nodes
    ( setf *unexplored* ( list root ) )
    ;; initialize list of explored nodes
    ( setf *explored* () )
    ; get ready to create good names
    ( setf counter ( make-instance 'accumulator ) )
    ( setf *ng* ( make-instance 'name-generator :prefix "N" :counter counter ) )
  )

;-----------------------------------------------------------------------
; breadth first search
  ( setf *tracing-search* T )

  ( defmethod solve ( &aux kids e-node )
    ( if *tracing-search* ( let ()
      ( terpri ) ( write-line ">>>Solve") ( terpri )
      ( display-explored-nodes )
      ( display-unexplored-nodes )
    ) )
    ( setf e-node ( pop *unexplored* ) )
    ( cond
      ( ( goalp e-node ) 
        ( display-solution e-node )
      )
      ( ( exploredp e-node )
        ( solve )
      )
      ( t 
        ( push e-node *explored* )
        ( setf kids ( children-of e-node ) )
        ( write-line "----------------the children of ------------------------" )
        ( display e-node )
        ( write-line "--------------------are---------------------------------" )
        ( mapcar #'display kids )
        ( write-line "--------------------------------------------------------" )
        ( setf *unexplored* ( append *unexplored* kids ) )
        ( solve )
      )
    ) 
    NIL
  )

;----------------------------------------------------------------------
; generating children

  ( defmethod child-of ( ( n node ) ( o operator ) &aux c )
    ( setf new-node ( make-instance 'node ) )
    ( setf ( node-name new-node ) ( next *ng* ) )
    ( setf ( node-parent new-node ) n )
    ( setf ( node-operator new-node ) o )
    ( setf c ( copy-state ( node-state n ) ) )
    ( apply-operator o c ) 
    ( setf ( node-state new-node ) c )
    new-node
  )
  
