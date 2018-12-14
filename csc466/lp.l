;;;;File: lp.l
;;;;Line: Lisp solutions for basic list processing challenges 

( defun singleton-p (lst)
	(cond 
		((NULL lst)
			NIL
		)
		((NULL (CDR lst))
			T
		)
		(T
			Nil
		)
	)
)

( defun rac (lst)
	(cond
		((singleton-p lst)
			(car lst)
		)
		(t
			(rac ( cdr lst ) )
		)
	)
)

( defun rdc (lst)
	( cond
		((singleton-p lst)
			()	
		)
		(t
			(cons (car lst) (rdc (cdr lst ) ) )
		)
	)
)

( defun snoc ( x lst )
	(cond
		( ( Null lst ) 
			( list x )
		)
		( t
			( cons ( car lst ) (snoc x ( cdr lst) ) )
		) 
	)
)

( defun palindrome-p (x)
	( cond
		( ( NULL x ) T)
		( (singleton-p x) t )
		( (equal ( car x ) ( rac x ) )
			( palindrome-p (cdr (rdc x) ) )	
		)
		( t
			NIL
		)
	)
)
( defun select ( x lst ) 
	( cond
		( (= x 0)
			(car lst)
		)
		( t
			(select (- x 1) (cdr lst) )
		)
	)
)

( defun pick ( lst )
	( select ( random ( length lst ) ) lst )
)

( defun sum ( lst )
	( cond
		( (NULL lst) 
			0
		)
		( t
			( + ( car lst ) ( sum ( cdr lst ) ) )
		)
	)
)

( defun product ( lst )
	( cond
		( (NULL lst )  
			1
		)
		( t
			( * ( car lst ) (product ( cdr lst ) ) )
		)
	)
)

( defun iota ( x ) 
	( cond
		( ( = x 0 ) 
			()
		)
		( t
			( snoc x ( iota ( - x 1 ) ) )
		)
	)
)

( defun duplicate ( n lo )
	( cond
		( ( = n 0 )
			()
		)
		( t
			( snoc lo ( duplicate ( - n 1 ) lo ) )
		)
	)
)

( defun factorial ( x )
	( product ( iota x ) )
)

( defun power ( x n )
	( product ( duplicate n x ) )
)	

( defun filter-in ( fun lst )
	( cond
		( ( equal lst () )	
			()
		)
		( ( equal ( funcall fun ( car lst ) ) T )
			( cons ( car lst ) ( filter-in fun ( cdr lst ) ) )
		)
		( T
			( filter-in fun ( cdr lst ) )
		)
	)
)

( defun filter-out ( fun lst )
	(cond 
		( ( equal lst () )
			()
		)
		( ( equal (funcall fun ( car lst ) ) NIL )
			( cons ( car lst ) ( filter-out fun ( cdr lst ) ) )
		)
		( T
			( filter-out fun ( cdr lst ) ) 
		)
	)
)

( defun take-from ( x lst )
	( cond
		( ( equal lst () )
			()
		)
		( ( not ( equal x ( car lst ) ) )
			( cons ( car lst ) ( take-from x ( cdr lst ) ) )
		)
		( T
			( take-from x ( cdr lst ) )
		)
	)
)
	
( defun take-one-from ( x lst )
	( cond
		( ( equal lst () )
			()
		)
		( ( equal x ( car lst ) )
			( cdr lst )
		)
		( T
			( cons ( car lst ) ( take-one-from x ( cdr lst ) ) )
		)
	)
)


( defun random-permutation ( lst &aux element remainder) 
	( cond
		( ( equal lst () )
			()
		)
		( T
			( setf element (nth ( random ( length lst ) ) lst ) )
			( setf remainder ( take-one-from element lst ) )
			( cons element ( random-permutation remainder ) )
		)
	)
)
