
-1 constant b'
: b-div \ ( n -- (2^32 mod n ) (2^32 / n))
	b' 0 rot dup >r sm/rem
	swap 1+ r> over = if drop 1+ 0 then
	swap
	;

0 value a'	\ high word of the divisor
0 value a''	\ low word of the divisor
: shift-divisor ( --)
	a' 1 lshift
	$80000000 a'' and 0<> if 1 or then
	to a'
	a'' 1 lshift to a''
	;

\ slow long division
: // ( d n -- q)
	rot to a'' swap to a'
	\ check for overflow
	a' over < invert if ( overflow) drop $ffffffff then
	\ initialize quotient
	0
	32 0 do
	shift-divisor over a' > invert if over a' swap - to a' 1 else 0 then
	swap 1 lshift or
	loop
	nip
	\ here the stack holds the quotient, a' holds the remainder
	;

