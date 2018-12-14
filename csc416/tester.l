( setf full-bank ( make-instance 'bank :missionaries '(m m m) :cannibals '(c c c) :boat 'b ) )

( setf empty-bank ( make-instance 'bank :missionaries () :cannibals () :boat NIL ) )

( setf test-state ( make-instance 'state :left-bank full-bank :right-bank empty-bank ) )

( write-line "state" )
( display test-state )
( terpri )
( terpri )


