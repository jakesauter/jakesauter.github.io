( load "accumulator.l" )

( defclass die () 
  (	
    ( num-faces :accessor die-num-faces :initarg :num-faces :initform 6 )
    ( top-face :accessor die-top-face :initarg :top-face :initform 1 )
    ( face-history :accessor die-face-history :initarg :face-history :initform () )
  )
)

( defmethod roll-die ( ( d die ) )
  ( setf (die-face-history d ) ( append  ( die-face-history d ) ( list ( die-top-face d ) ) ) )
  ( setf ( die-top-face d ) ( + 1 ( random  ( die-num-faces d) ) ) )
  NIL
)

( defmethod roll-for-pair ( ( d1 die ) ( d2 die ) )
  ( roll-die d1 )
  ( roll-die d2 )
  ( cond
    ( ( not ( = ( die-top-face d1 ) ( die-top-face d2 ) ) )
        ( roll-for-pair d1 d2 )
    )
    (T
      nil   
    )
  )
)

( defmethod roll-for-sum ( ( d1 die ) ( d2 die ) ( i integer ) )
  ( roll-die d1 )
  ( roll-die d2 )
  ( cond
    ( (not ( = i ( + ( die-top-face d1 ) ( die-top-face d2 ) ) ) )
      ( roll-for-sum d1 d2 i )
    )
    ( T
      NIL
    )
  )
)

( defmethod roll-for-7-11-pair ( ( d1 die ) ( d2 die ) )
  ( roll-die d1 )
  ( roll-die d2 )
  ( cond
    ( ( or ( = ( die-top-face d1 ) ( die-top-face d2 ) ) ( = 7 ( + ( die-top-face d1 ) ( die-top-face d2 ) ) )
                                                         ( = 11 ( + (die-top-face d1 ) ( die-top-face d2 ) ) ) )
       nil 
    )
    (t
      ( roll-for-pair d1 d2 )
    )
  )
)


( defmethod roll-for-sum-e ( ( d1 die ) ( d2 die ) ( i integer) )
  ( setf counter ( make-instance 'accumulator ) )
  (dotimes (n 100000)
    ( setf ( die-face-history d1 ) () )
    ( setf ( die-face-history d2 ) () )
    ( roll-for-sum d1 d2 i )
    ( inc counter ( length ( die-face-history d1 ) ) )
  )
  
  ( format t "empirical result: ~f times" ( let () ( / ( accumulator-value counter ) 100000 ) ) )
)

  
( defmethod roll-for-pair-e ( ( d1 die ) ( d2 die ) ) 
  ( setf counter ( make-instance 'accumulator ) )
  (dotimes (n 100000)
    ( setf ( die-face-history d1 ) () )
    ( setf ( die-face-history d2 ) () )
    ( roll-for-pair d1 d2 )
    ( inc counter ( length ( die-face-history d1 ) ) )
  )

  
  ( format t "empirical result: ~f times" ( let () ( / ( accumulator-value counter ) 100000 ) ) )
)

( defmethod roll-for-7-11-pair-e ( ( d1 die ) ( d2 die ) )
  ( setf counter ( make-instance 'accumulator ) )
  (dotimes (n 100000)
    ( setf ( die-face-history d1 ) () )
    ( setf ( die-face-history d2 ) () )
    ( roll-for-7-11-pair d1 d2 )
    ( inc counter ( length ( die-face-history d1 ) ) )
  )

  
  ( format t "empirical result: ~f times" ( let () ( / ( accumulator-value counter ) 100000 ) ) )
)
