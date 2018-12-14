( load "lp.l" )

;simulate the play of a game
( defmethod play ( &aux play avail move )
  ( setf play () )
  ( setf avail '( nw n ne w c e sw s se ) )
  ( dolist ( player '( x o x o x o x o x ) )
    ( cond
      ( ( eq player 'x )
        ( setf move ( pick avail ) )
        ( setf avail ( remove move avail ) )
        ( setf play ( snoc move play ) )
      )
      ( ( eq player 'o )
        ( setf move ( pick avail ) )
        ( setf avail ( remove move avail ) )
        ( setf play ( snoc move play ) )
      )
    )
  )
  play
)

