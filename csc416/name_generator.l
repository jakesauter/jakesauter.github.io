( defclass name-generator ()
  (
    ( prefix :accessor name-generator-prefix :initarg :prefix :initform "N" )
    ( counter :accessor name-generator-counter :initarg :counter :initform ( make-instance 'accumulator ) )
  )
)

( defmethod next ( ( n name-generator ) )
  ( inc ( name-generator-counter n ) 1 )
  ( concatenate 'string ( name-generator-prefix n ) ( write-to-string ( accumulator-value counter )  ) )
)
