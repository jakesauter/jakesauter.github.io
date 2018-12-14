( defclass accumulator ()
  (
    ( value :accessor accumulator-value :initarg :value :initform 0 )
  )
)

( defmethod inc ( ( a accumulator ) ( num integer ) )
  ( setf ( accumulator-value a ) ( + ( accumulator-value a ) num ) )
)

( defmethod dec ( ( a accumulator ) ( num integer ) )
  ( setf ( accumulator-value a ) ( - ( accumulator-value a) num ) )
)

( defmethod reset ( ( a accumulator ) )
  ( setf ( accumulator-value a ) 0 )
)
