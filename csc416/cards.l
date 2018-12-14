;;;;File: cards.1
;;;;Line: A lisp representation of a deck of cards



( defconstant *ranks* '( 1 2 3 4 5 6 7 8 9 10 Jack Queen King Ace) )
( defconstant *suits* '( Club Diamond Heart Spade ) )


( defun new-suit ( suit )
	( mapcar #'list *ranks* ( make-list ( length *ranks* ) :initial-element suit ) )
)

( defun new-deck ()
	( mapcan #'new-suit *suits* )
)

