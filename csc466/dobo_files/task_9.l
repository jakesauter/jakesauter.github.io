;;;; Task 9 Defining A Neural Net and Forward Propogation
;

( defclass neural-net ()
  (
    ( weights :accessor neural-net-weights :initarg :weights :initform '() )    
    ( biases :accessor neural-net-biases :initarg :biases :initform '() )
    ( params :accessor neural-net-params :initarg :params :initform '() )
  )
)

( defmethod display ( ( nn neural-net ) )
  ( format t "--------Weights--------" )
  ( dolist ( x ( reverse ( neural-net-weights nn ) ) )
    ( display x ) 
  )
  ( format t "--------Biases---------" )
  ( dolist ( x ( reverse ( neural-net-biases nn ) ) )
    ( display x ) 
  )
)

;; Method to write a neural net to a file
;
( defmethod write-nn-to-file ( ( nn neural-net ) ( nn-file string ) )
  ( setf nn ( net-to-list nn ) )
  ( with-open-file ( stream nn-file :direction :output )
    ( format stream "( " )
    ( dolist ( e nn )
      ( format stream "~A " e )
    )
    ( format stream ")~%~%" ) 
  )
)

;; Method to copy a neural net
;
( defmethod copy ( ( nn neural-net ) )
  ( list-to-net ( net-to-list nn ) nn ) 
)

;; Method to initialize a neural network with a list of 
; the nodes contained in each layer. An additional optional 
; variable can be provided to load the neural net from a file
;
( defun init-neural-net ( l &optional ind-file &aux ind nn weights biases params )
  ( if ind-file 
    ( let ( ( in ( open ind-file :if-does-not-exist nil ) ) )
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
  ) 
  ( setf weights '() )
  ( setf biases '() )
  ( setf params l )
  ( loop while ( not ( = ( length l ) 1 ) ) do
    ( setf weights ( append weights 
      ( list ( make-array ( list ( nth 0 l ) ( nth 1 l ) ) :initial-element 1 ) ) ) 
    )
    ( setf l ( cdr l ) )
    ( setf biases ( append biases
      ( list ( make-array ( list 1 ( nth 0 l ) ) :initial-element 0 ) ) ) 
    )
  )
  ( setf nn ( make-instance 'neural-net :weights weights :biases biases :params params ) )
  ( if ind-file
    ( setf nn ( list-to-net ind ( make-instance 'neural-net :weights weights :biases biases :params params ) ) )
  )
  nn
)

;; Method to forward propogate an input vector through the neural network
;
( defmethod forward-prop ( ( nn neural-net ) ( x array ) &optional notTraining demo &aux activation output )
  ( if demo 
    ( let () 
      ( format t "input: " )
      ( display x ) 
    )
  )
  ( setf activation ( v-add 
                        ( dot ( transpose x ) ( car ( neural-net-weights nn ) ) ) 
                        ( car ( neural-net-biases nn ) ) ) 
  )
  ( if demo
    ( let ()
      ( format t "first layer activation: ~%" ) 
      ( display activation )
    )
  ) 
  ( loop for i from 1 to ( - ( length ( neural-net-weights nn ) ) 1 ) do
    ( setf w ( nth i ( neural-net-weights nn ) ) )
    ( setf b ( nth i ( neural-net-biases nn ) ) )
    ( setf activation ( v-add ( dot ( sigmoid activation ) w ) b ) )
    ( if demo
      ( let ()  
        ( format t "activation of layer ~A: ~%" i )
        ( display activation )
      )
    )
  )
  ( setf output ( sigmoid ( float ( sum-list ( get-col 0 activation ) ) ) ) )
  ( if notTraining
    ( if ( > output .5 )
      ( setf output 1 )
      ( setf output 0 ) 
    ) 
  )
  ( if demo ( format t "net output: ~A~%~%" output ) )
  output
)

;; Wrapper for froward prop that input vectorizes a state before
; forward propogation is applied
;
( defmethod forward-prop ( ( nn neural-net ) ( s state ) &optional notTraining demo )
  ( forward-prop nn ( input-vector s ) notTraining demo )
)

;; Method to turn a state into an input vector
 ; for a neural network
( defmethod input-vector ( ( s state ) )
  ( input-vector ( state-board s ) )
)

;; Helper method to 
( defmethod input-vector ( ( x array ) )
  ( setf vector ( make-array ( list ( expt ( array-dimension x 0 ) 2 ) 1 ) ) )
  ( setf pos 0 )
  ( dotimes ( i ( array-dimension x 0 ) )
    ( dotimes ( j ( array-dimension x 1 ) )
      ( setf ( aref vector pos 0 ) ( aref x i j ) ) 
      ( setf pos ( + pos 1 ) )
    ) 
  ) 
  vector
)

( defun dobo-demo-9 ()
  ( format t "initializing a neural net of size 9-3-1~%" )
  ( setf nn ( init-neural-net '( 9 3 1 ) ) )
  ( display nn ) 
  ( setf x ( make-array '( 9 1 ) :initial-element 1 ) )
  ( forward-prop nn x nil t ) 
  nn
)
