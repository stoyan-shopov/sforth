\ simple c-like 'struct' data structure words

: struct ( -- size-address current-offset)
	create here 0 0 , does> create @ allot
	;
: end-struct ( size-address offset --)
	swap !
	;
: field ( offset size -- updated-offset)
	create over , + does> @ +
	;

.( test drive) cr cr

struct vector
	1 cells field x
	1 cells field y
end-struct

: .v ( vector-address --)
	dup x @ ." x = " . y @ ." y = " . cr ;
vector v
12 v x !
34 v y !

.( contents of vector v: ) v .v

