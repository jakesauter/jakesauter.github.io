;;;; Task 8 -- Implementing Necessary Matrix/Vector Functionality
; matrix transpose
; matrix multiplication
; input parameterize
; init-neural-net that makes a dynamic layered net
;

;; Method to copy an array 
;
( defmethod copy-array ( ( x array ) )
  ( setf return-array ( make-array ( array-dimensions x ) ) )
  ( dotimes ( i ( array-dimension x 0 ) )
    ( dotimes ( j ( array-dimension x 1 ) )
      ( setf ( aref return-array i j ) ( aref x i j ) )
    )
  )
  return-array
)

;; Method to apply the sigmoid function to 
; a single floating point number
;
( defmethod sigmoid ( ( x float ) )
  ( handler-case 
    ( / 1 ( + 1 ( exp ( - x ) ) ) ) 
    ( floating-point-underflow () 
      0
    )
    ( floating-point-overflow ()
      1
    )
  )
)

;; Method to apply the sigmoid function
; to every element in an array 
;
( defmethod sigmoid ( ( x array ) )
  ( setf nx ( copy-array x ) )
  ( dotimes ( i ( array-dimension x 0 ) )
    ( dotimes ( j ( array-dimension x 1 ) )
      ( setf input ( float ( aref x i j ) ) )   
      ( setf ( aref nx i j ) ( sigmoid input ) )  
    )    
  )
  nx
)

;; Method to dipslay an array
;
( defmethod display ( ( x array ) )
  ( terpri )
  ( dotimes ( i ( array-dimension x 0 ) )
    ( dotimes ( j ( array-dimension x 1 ) )
      ( format t "  ~A" ( aref x i j ) )
    )
    ( terpri )
  )
  ( terpri )
)

;; Method to apply a matrix transpose
;
( defmethod transpose ( ( x array ) )
  ( setf nx ( make-array ( list ( array-dimension x 1 ) ( array-dimension x 0 ) ) ) )
  ( dotimes ( i ( array-dimension x 1 ) )
    ( set-row i ( reverse ( get-col i x ) ) nx )
  ) 
  nx
)

;; Method to sum all elements in a list
;
( defmethod sum-list ( ( l list ) )
  ( cond
    ( ( null l )
      0
    )
    ( t 
      ( + ( car l ) ( sum-list ( cdr l ) ) )
    )
  )
)

;; Method to calculate the dot product of two lists
;
( defmethod dot ( ( l1 list ) ( l2 list ) &aux ret )
  ( setf ret 0 )
  ( if ( not ( = ( length l1 ) ( length l2 ) ) )
    ( let ()
      ( format t "Cannot dot vectors of different lengths!~%" )
      ( return-from dot nil )
    )
  )
  ( cond 
    ( ( null l1 )
      0      
    )
    ( t
      ( setf ret ( + ( dot ( cdr l1 ) ( cdr l2 ) ) ( * ( car l1 ) ( car l2 ) ) ) )
    )
  )
  ret
)

;; Method to perform elemtwise addtion of to matrices
;
( defmethod v-add ( ( x1 array ) ( x2 array ) )
  ( setf nx ( copy-array x1 ) ) 
  ( if ( not ( = ( array-dimension x1 0 ) ( array-dimension x2 0 ) ) )
    ( let ()
      ( format t "Cannot add vectors of size ~A x ~A and ~A x ~A~%"
                  ( array-dimension x1 0 ) ( array-dimension x1 1 ) 
                  ( array-dimension x2 0 ) ( array-dimension x2 1 ) 
      )
      ( return-from v-add nil ) 
    )
  )
  ;all good to go 
  ( dotimes ( i ( array-dimension x1 0 ) )
    ( dotimes ( j ( array-dimension x2 1 ) ) 
      ( setf ( aref nx i j ) ( + ( aref x1 i j ) ( aref x2 i j ) ) ) 
    )
  )
  nx
)

;; Method to perform elemetwise multiplication of
; a constant to every matrix entry
;
( defmethod multiply ( ( c float ) ( x array ) )
  ( setf nx ( copy-array x ) )
  ( dotimes ( i ( array-dimension x 0 ) )
    ( dotimes ( j ( array-dimension x 1 ) )
      ( setf ( aref nx i j ) ( * c ( aref x i j ) ) )
    )
  )
  nx
)

;; Method to calculate the dot product of two matrices
;
( defmethod dot ( ( x1 array ) ( x2 array ) )
  ( if ( not ( equal ( array-dimension x1 1 ) ( array-dimension x2 0 ) ) )
    ( let () 
      ( format t "Cannot multiply a ~A x ~A by a ~A by ~A!~%" 
        ( array-dimension x1 0 ) ( array-dimension x1 1 ) 
        ( array-dimension x2 0 ) ( array-dimension x2 1 ) 
      )
      ( return-from dot nil )
    )
  ) 
  ;all good to go
  ( setf nx ( make-array ( list ( array-dimension x1 0 ) ( array-dimension x2 1 ) ) ) )
  ( dotimes ( i ( array-dimension nx 0 ) )
    ( dotimes ( j ( array-dimension nx 1 ) )
      ( setf ( aref nx i j )  
          ( dot ( get-row i x1 ) ( get-col j x2 ) ) 
      )
    ) 
  )
  nx
)

( defun dobo-demo-8 ()
  ( setf matrix ( make-array '( 4 4 ) :initial-element 1 ) )
  ( format t "matrix: ~%" )
  ( display matrix )
  ( format t "matrix multiplied by 5: ~%" )
  ( display ( multiply 5.0 matrix ) )
  ( format t "matrix 1 dotted with its product with 5: ~%" )
  ( display ( dot matrix ( multiply 5.0 matrix ) ) )
  ( format t "matrix 1 summed with itself: ~%" )
  ( display ( v-add matrix matrix ) )
  ( format t "column 0 of matrix 1 summed: ~A ~%~%" ( sum-list ( get-col 0 matrix ) ) )
  ( set-row 0 '( 0 0 0 0 ) matrix )
  ( format t "matrix: ~%" )
  ( display matrix )
  ( format t "transpose of the matrix: ~%" )
  ( display ( transpose matrix ) )
  ( format t "sigmoid of matrix: ~%" )
  ( display ( sigmoid matrix ) )
)
