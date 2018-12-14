

( defmethod first-n ( ( n integer ) ( rgb-string list )  )
  ( cond
    ( ( = n 0 )
      ()
    )
    ( T
      ( cons ( car rgb-string ) ( first-n  ( - n 1 ) ( cdr rgb-string ) ) ) 
    )
  ) 
)

( defmethod rest-n ( ( n integer ) ( rgb-string list ) )
  ( cond 
    ( ( = n 25 )
      () 
    )
    ( T 
      ( cons ( nth n rgb-string ) ( rest-n ( + n 1 ) rgb-string ) )
    )
  )  
)
