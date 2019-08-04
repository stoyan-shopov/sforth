/*
Copyright (c) 2014-2016 stoyan shopov

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/
/******************************************************************************/

#include "engine.h"
#include "enabled-words.h"

void /* "!" */		do_store(void)
/*******************************************************************************
	!	[store]
( x a-addr -- )
	Store 'x' at 'a-addr'.
*******************************************************************************/
		{ dcell x = popd(); *(cell *) ((cell)(x >> CELL_NR_BITS)) = (cell) x; }
/******************************************************************************/
void /* "#" */		do_number_sign(void)
/******************************************************************************/
/*******************************************************************************
	#	[number-sign] 
( ud_1 -- ud_2)
	Divide 'ud_1' by the number in 'BASE' giving the
	quotient 'ud_2' and the remainder 'n'. ('n' is
	the least significant digit of 'ud_1'.) Convert 'n'
	to external form and add the resulting character to the beginning
	of the pictured numeric output string. An ambiguous condition
	exists if 'num' executes outside of a 
	'num-start' 'num-end' delimited number conversion.
*******************************************************************************/
{
static const char digs[10 + 26] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
dcell x = popd(), q; cell r; if (!(MIN_BASE <= base && base <= MAX_BASE)) sabort("bad numeric base"); q = x / base, r = x % base; pushd(q); pno.buf[-- pno.idx] = digs[r]; }

/******************************************************************************/
void /* "#>" */		do_number_sign_greater(void)
/*******************************************************************************
	#>	[number-sign-greater] 
( xd -- c-addr u)
	Drop 'xd'. Make the pictured numeric output string
	available as a character string. 'c-addr' and 'u'
	specify the resulting character string. A program may replace
	characters within the string.
*******************************************************************************/
		{ popd(); push((cell)(pno.buf + pno.idx)); push(PNOBUF_SIZE - pno.idx); }

/******************************************************************************/
void /* "#s" */		do_number_sign_s(void)
/*******************************************************************************
	#S	[number-sign-s] 
( ud_1 -- ud_2)
	Convert one digit of 'ud_1' according to the rule for
	'num'. Continue conversion until the quotient is zero.
	'ud_2' is zero. An ambiguous condition exists if
	'numS' executes outside of a 'num-start' 'num-end'
	delimited number conversion.
*******************************************************************************/
		{ do do_number_sign(); while (topd()); }

/******************************************************************************/
void /* "'" */		do_tick(void)
/*******************************************************************************
	'	[tick] 
( "<spaces>name" -- xt)
	Skip leading space delimiters. Parse 'name' delimited by
	a space. Find 'name' and return 'xt', the execution
	token for 'name'. An ambiguous condition exists if
	'name' is not found. When interpreting,
	" ' xyz EXECUTE" is equivalent to " xyz".
*******************************************************************************/
		{ do_bl(); do_word(); do_find(); if (!pop()) sabort("word not found"); }

/******************************************************************************/
void /* "(" */		do_paren(void)
/*******************************************************************************
	(	[paren] 
\compile
	Perform the execution semantics given below.
\execute
	( "ccc<paren>" -- )
	Parse 'ccc' delimited by ")"
	(right parenthesis).
	'(' is an immediate word.
	The number of characters in 'ccc' may be zero to the
	number of characters in the parse area.
*******************************************************************************/
{
	while (1)
	{
		push(')');
		do_parse();
		if (input_spec.source_id == SOURCE_ID_STRING)
		{
			pop();
			break;
		}
		else
		{
			/* input is the user input device, or a file */
			pop();
			if (input_spec.buf[input_spec.idx - 1] == ')')
				break;
			pop();
			do_refill();
			if (!pop())
				break;
		}
	}
	pop();
}

/******************************************************************************/
void /* "*" */		do_star(void)
/*******************************************************************************
	*	[star] 
( n_1|u_1 n_2|u_2 -- n_3|u_3)
	Multiply 'n_1|u_1' by 'n_2|u_2' giving the product
	'n_3|u_3'.
*******************************************************************************/
		{ dcell x = popd(); push((cell)((cell)(x >> CELL_NR_BITS) * (cell)x)); }

/******************************************************************************/
void /* "*\/" */		do_star_slash(void)
/*******************************************************************************/
//	*/	[star-slash] 
/*******************************************************************************
( n_1 n_2 n_3 -- n_4)
	Multiply 'n_1' by 'n_2' producing the intermediate
	double-cell result $d$. Divide $d$ by 'n_3' giving the
	single-cell quotient 'n_4'. An ambiguous condition exists
	if 'n_3' is zero or if the quotient 'n_4' lies
	outside the range of a signed number. If $d$ and 'n_3'
	differ in sign, the implementation-defined result returned will
	be the same as that returned by either the phrase
	'toR' 'M*' 'Rfrom' 'FM/MOD' 'SWAP' 'DROP'
	or the phrase
	'toR' 'M*' 'Rfrom' 'SM/REM' 'SWAP' 'DROP'.
*******************************************************************************/
		{ sdcell x; scell n3, n4; n3 = spop(); x = (sdcell) spop() * (sdcell) spop(); if (n3) n4 = x / n3; else /* ??? what to do here */ sabort("divide by zero"); spush(n4); }

/******************************************************************************/
void /* "*\/mod" */	do_star_slash_mod(void)
/*******************************************************************************/
//	*/MOD	[star-slash-mod] 
/*******************************************************************************
( n_1 n_2 n_3 -- n_4 n_5)
	Multiply 'n_1' by 'n_2' producing the intermediate
	double-cell result $d$. Divide $d$ by 'n_3' producing the
	single-cell remainder 'n_4' and the single-cell quotient
	'n_5'. An ambiguous condition exists if 'n_3' is
	zero, or if the quotient 'n_5' lies outside the range of a
	single-cell signed integer. If $d$ and 'n_3' differ in
	sign, the implementation-defined result returned will be the
	same as that returned by either the phrase
	'toR' 'M*' 'Rfrom' 'FM/MOD' or the phrase
	'toR' 'M*' 'Rfrom' 'SM/REM'.
*******************************************************************************/
		{ sdcell x; scell n3, n4, n5; n3 = spop(); x = (sdcell) spop() * (sdcell) spop(); if (n3) n4 = x % n3, n5 = x / n3; else /* ??? what to do here */ sabort("divide by zero"); spush(n4); spush(n5); }

/******************************************************************************/
void /* "+" */		do_plus(void)
/*******************************************************************************
	+	[plus] 
( n_1|u_1 n_2|u_2 -- n_3|u_3)
	Add 'n_2|u_2' to 'n_1|u_1', giving the sum
	'n_3|u_3'.
*******************************************************************************/
		{ dcell x = popd(); push(((cell) (x >> CELL_NR_BITS)) + ((cell) x)); }

/******************************************************************************/
void /* "+!" */		do_plus_store(void)
/*******************************************************************************
	+!	[plus-store] 
( n|u a-addr -- )
	Add 'n|u' to the single-cell number at 'a-addr'.
*******************************************************************************/
		{ dcell x = popd(); * ((cell *) ((cell) (x >> CELL_NR_BITS))) += (cell) x; }

/******************************************************************************/
void /* "+loop" */	do_plus_loop(void)
/*******************************************************************************
	+LOOP	[plus-loop] 
\interpret
	Interpretation semantics for this word are undefined.
\compile
	( C]{do-sys -- )
	Append the run-time semantics given below to the current
	definition. Resolve the destination of all unresolved
	occurrences of 'LEAVE' between the location given
	by 'do-sys' and the next location for a transfer of
	control, to execute the words following '+LOOP'.
\runtime
	( n -- )
	( R]{loop-sys_1 -- |loop-sys_2)
	An ambiguous condition exists if the loop control parameters
	are unavailable. Add 'n' to the loop index. If the loop
	index did not cross the boundary between the loop limit minus
	one and the loop limit, continue execution at the beginning
	of the loop. Otherwise, discard the current loop control
	parameters and continue execution immediately following the
	loop.
*******************************************************************************/
{
int i;
	* here.word ++ = & xt_runtime_plus_loop;
	do_until();
	do_unloop();
	/* resolve any 'leave's */
	i = rpop();
	while (i --)
		rfrom(), do_then();
}

/******************************************************************************/
void /* "," */		do_comma(void)
/*******************************************************************************
	,	[comma] 
( x -- )
	Reserve one cell of data space and store 'x' in the cell.
	If the data-space pointer is aligned when ',' begins
	execution, it will remain aligned when ',' finishes
	execution. An ambiguous condition exists if the data-space
	pointer is not aligned prior to execution of ','.
*******************************************************************************/
		{ * here.cell ++ = pop(); }

/******************************************************************************/
void /* "-" */		do_minus(void)
/*******************************************************************************
	-	[minus] 
( n_1|u_1 n_2|u_2 -- n_3|u_3)
	Subtract 'n_2|u_2' from 'n_1|u_1', giving the
	difference 'n_3|u_3'.
*******************************************************************************/
		{ dcell x = popd(); push((cell) x - (cell)(x >> CELL_NR_BITS)); }

/******************************************************************************/
void /* "." */		do_dot(void)
/*******************************************************************************
	.	[dot] 
( n -- )
	Display 'n' in free field format.
*******************************************************************************/
		{ ddup(); do_abs(); push(0); do_less_number_sign(); do_number_sign_s(); rot(); do_sign(); do_number_sign_greater(); do_type(); do_space(); }

/******************************************************************************/
void /* ".\"" */		do_dot_quote(void)
/*******************************************************************************
	."	[dot-quote] 
\interpret
	Interpretation semantics for this word are undefined.
\compile
	( "ccc<quote>" -- )
	Parse 'ccc' delimited by " (double-quote).
	Append the run-time semantics given below to the current
	definition.
\runtime
	(  -- )
	Display 'ccc'.
*******************************************************************************/
		{ do_s_quote(); postpone("type"); }

/******************************************************************************/
void /* "/" */		do_slash(void)
/*******************************************************************************
	/	[slash] 
( n_1 n_2 -- n_3)
	Divide 'n_1' by 'n_2', giving the single-cell quotient
	'n_3'. An ambiguous condition exists if 'n_2' is zero.
	If 'n_1' and 'n_2' differ in sign, the
	implementation-defined result returned will be the same as that
	returned by either the phrase
	'toR' 'StoD' 'Rfrom' 'FM/MOD' 'SWAP' 'DROP'
	or the phrase
	'toR' 'StoD' 'Rfrom' 'SM/REM' 'SWAP' 'DROP'.
*******************************************************************************/
		{ dcell x = popd(); if (!(cell)(x >> CELL_NR_BITS)) sabort("divide by zero"); push(((scell) (cell) x) / ((scell)(cell)(x >> CELL_NR_BITS))); }

/******************************************************************************/
void /* "/mod" */		do_slash_mod(void)
/*******************************************************************************
	/MOD	[slash-mod] 
( n_1 n_2 -- n_3 n_4)
	Divide 'n_1' by 'n_2', giving the single-cell remainder
	'n_3' and the single-cell quotient 'n_4'. An ambiguous
	condition exists if 'n_2' is zero. If 'n_1' and
	'n_2' differ in sign, the implementation-defined result
	returned will be the same as that returned by either the phrase
	'toR' 'StoD' 'Rfrom' 'FM/MOD'
	or the phrase
	'toR' 'StoD' 'Rfrom' 'SM/REM'.
*******************************************************************************/
		{ dcell x = popd(); if (!(cell)(x >> CELL_NR_BITS)) sabort("divide by zero"); push(((scell) (cell) x) % ((scell)(cell)(x >> CELL_NR_BITS))); push(((scell) (cell) x) / ((scell)(cell)(x >> CELL_NR_BITS))); }

/******************************************************************************/
void /* "0<" */		do_zero_less(void)
/*******************************************************************************
	0<	[zero-less] 
( n -- flag)
	'flag' is true if and only if 'n' is less than zero.
*******************************************************************************/
		{ push((spop() < 0) ? C_TRUE : C_FALSE); }

/******************************************************************************/
void /* "0=" */		do_zero_equals(void)
/*******************************************************************************
	0=	[zero-equals] 
( x -- flag)
	'flag' is true if and only if 'x' is equal to zero.
*******************************************************************************/
		{ push(pop() ? C_FALSE : C_TRUE); }

/******************************************************************************/
void /* "1+" */		do_one_plus(void)
/*******************************************************************************
	1+	[one-plus] 
( n_1|u_1 -- n_2|u_2)
	Add one (1) to 'n_1|u_1' giving the sum
	'n_2|u_2'.
*******************************************************************************/
		{ push(pop() + 1);  }

/******************************************************************************/
void /* "1-" */		do_one_minus(void)
/*******************************************************************************
	1-	[one-minus] 
( n_1|u_1 -- n_2|u_2)
	Subtract one (1) from 'n_1|u_1' giving the difference
	'n_2|u_2'.
*******************************************************************************/
		{ push(pop() - 1);  }

/******************************************************************************/
void /* "2!" */		do_two_store(void)
/*******************************************************************************
	2!	[two-store] 
( x_1 x_2 a-addr -- )
	Store the cell pair 'x_1 x_2' at 'a-addr', with
	'x_2' at 'a-addr' and 'x_1' at the next
	consecutive cell. It is equivalent to the sequence
	'SWAP' 'OVER' '!' 'CELL+' '!'.
*******************************************************************************/
		{ cell * p = (cell *) pop(); * p ++ = pop(); * p = pop(); }

/******************************************************************************/
void /* "2*" */		do_two_star(void)
/*******************************************************************************
	2*	[two-star] 
( x_1 -- x_2)
	'x_2' is the result of shifting 'x_1' one bit toward
	the most-significant bit, filling the vacated least-significant
	bit with zero.
*******************************************************************************/
		{ push(pop() << 1); }

/******************************************************************************/
void /* "2/" */		do_two_slash(void)
/*******************************************************************************
	2/	[two-slash] 
( x_1 -- x_2)
	'x_2' is the result of shifting 'x_1' one bit toward
	the least-significant bit, leaving the most-significant bit
	unchanged.
*******************************************************************************/
		{ spush(spop() >> 1); }

/******************************************************************************/
void /* "2@" */		do_two_fetch(void)
/*******************************************************************************
	2@	[two-fetch] 
( a-addr -- x_1 x_2)
	Fetch the cell pair 'x_1 x_2' stored at 'a-addr'.
	'x_2' is stored at 'a-addr' and 'x_1' at the
	next consecutive cell. It is equivalent to the sequence
	'DUP' 'CELL+' '@' 'SWAP' '@'.
*******************************************************************************/
		{ cell * p = (cell *) pop(); push(p[1]); push(* p); }

/******************************************************************************/
void /* "2drop" */	do_two_drop(void)
/*******************************************************************************
	2DROP	[two-drop] 
( x_1 x_2 -- )
	Drop cell pair 'x_1 x_2' from the stack.
*******************************************************************************/
		{ popd(); }

/******************************************************************************/
void /* "2dup" */		do_two_dup(void)
/*******************************************************************************
	2DUP	[two-dupe] 
( x_1 x_2 -- x_1 x_2 x_1 x_2)
	Duplicate cell pair 'x_1 x_2'.
*******************************************************************************/
		{ pushd(topd()); }

/******************************************************************************/
void /* "2over" */	do_two_over(void)
/*******************************************************************************
	2OVER	[two-over] 
( x_1 x_2 x_3 x_4 -- x_1 x_2 x_3 x_4 x_1 x_2)
	Copy cell pair 'x_1 x_2' to the top of the stack.
*******************************************************************************/
		{ two_over(); }

/******************************************************************************/
void /* "2swap" */	do_two_swap(void)
/*******************************************************************************
	2SWAP	[two-swap] 
( x_1 x_2 x_3 x_4 -- x_3 x_4 x_1 x_2)
	Exchange the top two cell pairs.
*******************************************************************************/
		{ dcell x = popd(), y = popd(); pushd(x); pushd(y); }

/******************************************************************************/
void /* ":" */		do_colon(void)
/*******************************************************************************
	:	[colon] 
( C]{"<spaces>name" -- colon-sys)
	Skip leading space delimiters. Parse 'name' delimited by a
	space. Create a definition for 'name', called a ``colon
	definition''. Enter compilation state and start the current
	definition, producing 'colon-sys'. Append the initiation
	semantics given below to the current definition.
	The execution semantics of 'name' will be determined by the
	words compiled into the body of the definition. The current
	definition shall not be findable in the dictionary until it is
	ended (or until the execution of 'DOES' in some systems).
\init ( i*x -- i*x)
	( R]{ -- nest-sys)
	Save implementation-dependent information 'nest-sys'
	about the calling definition. The stack effects 'i*x'
	represent arguments to 'name'.
\execute[name]
	( i*x -- j*x)
	Execute the definition 'name'. The stack effects
	'i*x' and 'j*x' represent arguments to and
	results from 'name', respectively.
*******************************************************************************/
		{ if (state != STATE_INTERPRETING) sabort("bad state"); state = STATE_COMPILING; do_create(); latest->is_smudged = 1; latest->cfa = runtime_colon; }

/******************************************************************************/
void /* ";" */		do_semicolon(void)
/*******************************************************************************
	;	[semicolon] 
\interpret
	Interpretation semantics for this word are undefined.
\compile
	( C]{colon-sys -- )
	Append the run-time semantics below to the current definition. End
	the current definition, allow it to be found in the dictionary and
	enter interpretation state, consuming 'colon-sys'. If the
	data-space pointer is not aligned, reserve enough data space to
	align it.
\runtime
	(  -- )
	( R]{nest-sys -- )
	Return to the calling definition specified by 'nest-sys'.
*******************************************************************************/
		{ if (state != STATE_COMPILING) sabort("bad state"); do_exit(); latest->is_smudged = 0; state = STATE_INTERPRETING; }

/******************************************************************************/
void /* "<" */		do_less_than(void)
/*******************************************************************************
	<	[less-than] 
( n_1 n_2 -- flag)
	'flag' is true if and only if 'n_1' is less than
	'n_2'.
*******************************************************************************/
		{ dcell x = popd(); push(((scell)(cell) x < (scell)(cell)(x >> CELL_NR_BITS)) ? C_TRUE : C_FALSE); }

/******************************************************************************/
void /* "<#" */	        do_less_number_sign(void)
/*******************************************************************************
	<#	[less-number-sign] 
(  -- )
	Initialize the pictured numeric output conversion process.
*******************************************************************************/
		{ pno.idx = sizeof pno.buf; /* pno.buf[-- pno.idx] = ' '; */ }

/******************************************************************************/
void /* "=" */	        do_equals(void)
/*******************************************************************************
	=	[equals] 
( x_1 x_2 -- flag)
	'flag' is true if and only if 'x_1' is bit-for-bit
	the same as 'x_2'.
*******************************************************************************/
		{ dcell x = popd(); push(((cell)(x >> CELL_NR_BITS) == (cell) x) ? C_TRUE : C_FALSE); }

/******************************************************************************/
void /* ">" */	        do_greater_than(void)
/*******************************************************************************
	>	[greater-than] 
( n_1 n_2 -- flag)
	'flag' is true if and only if 'n_1' is greater than 'n_2'.
*******************************************************************************/
		{ dcell x = popd(); push(((scell)(cell) x > (scell)(cell)(x >> CELL_NR_BITS)) ? C_TRUE : C_FALSE); }

/******************************************************************************/
void /* ">body" */	do_to_body(void)
/*******************************************************************************
	>BODY	[to-body] 
( xt -- a-addr)
	'a-addr' is the data-field address corresponding to
	'xt'. An ambiguous condition exists if 'xt' is not
	for a word defined via 'CREATE'.
*******************************************************************************/
		{ push((cell) &((struct word *) pop())->pfa); }

/******************************************************************************/
void /* ">in" */	        do_to_in(void)
/*******************************************************************************
	>IN	[to-in] 
(  -- a-addr)
	'a-addr' is the address of a cell containing the offset in
	characters from the start of the input buffer to the start of
	the parse area.
*******************************************************************************/
		{ push((cell) &input_spec.idx); }

/******************************************************************************/
void /* ">number" */      do_to_number(void)
/*******************************************************************************
	>NUMBER	[to-number] 
( ud_1 c-addr_1 u_1 -- ud_2 c-addr_2 u_2)
	'ud_2' is the unsigned result of converting the characters
	within the string specified by 'c-addr_1 u_1' into digits,
	using the number in 'BASE', and adding each into 'ud_1'
	after multiplying 'ud_1' by the number in 'BASE'.
	Conversion continues left-to-right until a character that is not
	convertible, including any ``+'' or ``-'', is encountered or the
	string is entirely converted.
	'c-addr_2' is the location of the first unconverted character
	or the first character past the end of the string if the string was
	entirely converted. 'u_2' is the number of unconverted
	characters in the string. An ambiguous condition exists if
	'ud_2' overflows during the conversion.
*******************************************************************************/
{
	dcell res; bool sign = false; cell i = pop(); uint8_t c, * s = (uint8_t *) pop();
	res = popd();
	cell saved_base = base;
	/* first, handle character literals, in the form: 'x' */
	if (i == 3 && s[0] == '\'' && s[2] == '\'')
	{
		res += (unsigned) s[1];
		i = 0;
		s += 3;
	}
	/* handle numeric base selection symbols */
	if (i) switch (*s)
	{
		case '#': base = 10; if (0)
		case '$': base = 16; if (0)
		case '%': base = 2;
			  s ++, i --;
	}
	if (i && MIN_BASE <= base && base <= MAX_BASE)
	{
		/* handle sign */
		c = * s;
		sign = (c == '-') ? true : false;
		if (c == '-' || c == '+')
			s ++, i --;
		if (!i)
			/* failure */
			s--, i ++, sign = false;
		while (i)
		{
			c = * s;
			c = xtolower(c);
			if ('0' <= c && c <= '9')
				c -= '0';
			else if ('a' <= c && c <= 'z')
				c -= 'a' - 10;
			else
				/* cannot convert digit */
				break;
			if (c >= base)
				/* failure */
				break;
			res *= base;
			res += c;
			s ++;
			i --;
		}
	}
	if (sign)
		res = - res;
	pushd(res), push((cell) s), push(i);
	base = saved_base;
}

/******************************************************************************/
void /* ">r" */	        do_to_r(void)
/*******************************************************************************
	>R	[to-r] 
\interpret
	Interpretation semantics for this word are undefined.
\execute
	( x -- )
	( R]{ -- x)
	Move 'x' to the return stack.
*******************************************************************************/
		{ rpush(pop()); }

/******************************************************************************/
void /* "?dup" */	        do_question_dup(void)
/*******************************************************************************
	?DUP	[question-dupe] 
( x -- 0 | x x)
	Duplicate 'x' if it is non-zero.
*******************************************************************************/
		{ cell x; if ((x = top())) push(x); }

/******************************************************************************/
void /* "@" */	        do_fetch(void)
/*******************************************************************************
	@	[fetch] 
( a-addr -- x)
	'x' is the value stored at 'a-addr'.
*******************************************************************************/
		{ push(*(cell *) pop()); }

/******************************************************************************/
void /* "abort" */	do_abort(void)
/*******************************************************************************
	ABORT	 
( i*x -- )
	( R]{j*x -- )
	Empty the data stack and perform the function of 'QUIT',
	which includes emptying the return stack, without displaying
	a message.
*******************************************************************************/
		{ sp = rsp = 0; longjmp(jmpenv, SF_EXC_CODE_ABORT); }

/******************************************************************************/
void /* "abort\"" */	do_abort_quote(void)
/*******************************************************************************
	ABORT"	[abort-quote] 
\interpret
	Interpretation semantics for this word are undefined.
\compile
	( "ccc<quote>" -- )
	Parse 'ccc' delimited by a " (double-quote).
	Append the run-time semantics given below to the current
	definition.
\runtime
	( i*x x_1 -- | i*x)
	( R]{j*x -- | j*x)
	Remove 'x_1' from the stack. If any bit of 'x_1' is not
	zero, display 'ccc' and perform an implementation-defined
	abort sequence that includes the function of 'ABORT'.
*******************************************************************************/
		{ do_s_quote(); postpone("rot"); do_if(); postpone("type"); postpone("cr"); postpone("abort"); do_then(); postpone("2drop"); }

/******************************************************************************/
void /* "abs" */		do_abs(void)
/*******************************************************************************
	ABS	[abs] 
( n -- u)
	'u' is the absolute value of 'n'.
*******************************************************************************/
		{ scell x = spop(); spush(x >= 0 ? x : - x); }

/******************************************************************************/
void /* "accept" */ 	do_accept (void)

/*******************************************************************************
	ACCEPT	 
( c-addr +n_1 -- +n_2)
	Receive a string of at most '+n_1' characters. An ambiguous
	condition exists if '+n_1' is zero or greater than 32,767.
	Display graphic characters as they are received. A program that
	depends on the presence or absence of non-graphic characters in the
	string has an environmental dependency. The editing functions, if
	any, that the system performs in order to construct the string are
	implementation-defined.
	Input terminates when an implementation-defined line terminator is
	received. When input terminates, nothing is appended to the string,
	and the display is maintained in an implementation-defined way.
	'+n_2' is the length of the string stored at 'c-addr'.
*******************************************************************************/
	{ scell len = sf_pop(); cell x = 0; uint8_t * s = sf_pop(), c; if (len < 0) /* ambiguous condition */ len = 0;
		while (len --) { c = sfgetc(); if (c == '\n') goto out; * s ++ = c; x ++; }
		/* read remainder of line, until a new line terminator */ while (sfgetc() != '\n'); out: sf_push(x); }

void /* "align" */	do_align(void)
/*******************************************************************************
	ALIGN	 
(  -- )
	If the data-space pointer is not aligned, reserve enough space
	to align it.
*******************************************************************************/
		{ cell x = (cell) here.cell; x += sizeof(cell) - 1; x &= ~ (sizeof(cell) - 1); here.cell = (cell *) x; }

/******************************************************************************/
void /* "aligned" */	do_aligned(void)
/*******************************************************************************
	ALIGNED	 
( addr -- a-addr)
	'a-addr' is the first aligned address greater than or equal
	to 'addr'.
*******************************************************************************/
		{ cell x = pop(); x += sizeof(cell) - 1; x &= ~ (sizeof(cell) - 1); push(x); }

/******************************************************************************/
void /* "allot" */	do_allot(void)
/*******************************************************************************
	ALLOT	 
( n -- )
	If 'n' is greater than zero, reserve 'n' address units
	of data space. If 'n' is less than zero, release '|n|'
	address units of data space. If 'n' is zero, leave the
	data-space pointer unchanged.
	If the data-space pointer is aligned and 'n' is a multiple
	of the size of a cell when 'ALLOT' begins execution, it will
	remain aligned when 'ALLOT' finishes execution.
	If the data-space pointer is character aligned and 'n' is a
	multiple of the size of a character when 'ALLOT' begins
	execution, it will remain character aligned when 'ALLOT'
	finishes execution.
*******************************************************************************/
		{ here.chr += pop(); }

/******************************************************************************/
void /* "and" */		do_and(void)
/*******************************************************************************
	AND	 
( x_1 x_2 -- x_3)
	'x_3' is the bit-by-bit logical ``and'' of 'x_1'
	with 'x_2'.
*******************************************************************************/
		{ dcell x = popd(); push((cell)(x >> CELL_NR_BITS) & (cell) x); }

/******************************************************************************/
void /* "base" */		do_base(void)
/*******************************************************************************
	BASE	 
(  -- a-addr)
	'a-addr' is the address of a cell containing the current
	number-conversion radix \{\{2...36\}\}.
*******************************************************************************/
		{ push((cell) & base); }

/******************************************************************************/
void /* "begin" */	do_begin(void)
/*******************************************************************************
	BEGIN	 
\interpret
	Interpretation semantics for this word are undefined.
\compile
	( C]{ -- dest)
	Put the next location for a transfer of control, 'dest', onto
	the control flow stack. Append the run-time semantics given below
	to the current definition.
\runtime
	(  -- )
	Continue execution.
*******************************************************************************/
		{ push((cell) here.cell); }

/******************************************************************************/
void /* "bl" */		do_bl(void)
/*******************************************************************************
	BL	[b-l] 
(  -- char)
	'char' is the character value for a space.
*******************************************************************************/
		{ push(' '); }

/******************************************************************************/
void /* "c!" */		do_c_store(void)
/*******************************************************************************
	C!	[c-store] 
( char c-addr -- )
	Store 'char' at 'c-addr'. When character size is smaller
	than cell size, only the number of low-order bits corresponding to
	character size are transferred.
*******************************************************************************/
		{ dcell x = popd(); *(uint8_t *) ((cell)(x >> CELL_NR_BITS)) = (uint8_t) x; }

/******************************************************************************/
void /* "c," */		do_c_comma(void)
/*******************************************************************************
	C,	[c-comma] 
( char -- )
	Reserve space for one character in the data space and store
	'char' in the space. If the data-space pointer is character
	aligned when 'C,' begins execution, it will remain character
	aligned when 'C,' finishes execution.
	An ambiguous condition exists if the data-space pointer is not
	character-aligned prior to execution of 'C,'.
*******************************************************************************/
		{ * here.chr ++ = pop(); }

/******************************************************************************/
void /* "c@" */		do_c_fetch(void)
/*******************************************************************************
	C@	[c-fetch] 
( c-addr -- char)
	Fetch the character stored at 'c-addr'. When the cell size is
	greater than character size, the unused high-order bits are all
	zeroes.
*******************************************************************************/
		{ push(* (uint8_t *) pop()); }

/******************************************************************************/
void /* "cell+" */	do_cell_plus(void)
/*******************************************************************************
	CELL+	[cell-plus] 
( a-addr_1 -- a-addr_2)
	Add the size in address units of a cell to 'a-addr_1', giving
	'a-addr_2'.
*******************************************************************************/
		{ push((cell) ((cell *) pop() + 1)); }

/******************************************************************************/
void /* "cells" */	do_cells(void)
/*******************************************************************************
	CELLS	 
( n_1 -- n_2)
	'n_2' is the size in address units of 'n_1' cells.
*******************************************************************************/
		{ push(pop() * sizeof(cell)); }

/******************************************************************************/
void /* "char" */		do_char(void)
/*******************************************************************************
	CHAR	[char] 
( "<spaces>name" -- char)
	Skip leading space delimiters. Parse 'name' delimited by
	a space. Put the value of its first character onto the stack.
*******************************************************************************/
		{ do_bl(); do_parse(); if (pop()) push(*(uint8_t *) pop()); else pop(), push(0); }

/******************************************************************************/
void /* "char+" */	do_char_plus(void)
/*******************************************************************************
	CHAR+	[char-plus] 
( c-addr_1 -- c-addr_2)
	Add the size in address units of a character to
	'c-addr_1', giving 'c-addr_2'.
*******************************************************************************/
		{ push((cell)((uint8_t *) pop() + 1)); }

/******************************************************************************/
void /* "chars" */	do_chars(void)
/*******************************************************************************
	CHARS	[chars] 
( n_1 -- n_2)
	'n_2' is the size in address units of 'n_1'
	characters.
*******************************************************************************/
		{ push(pop() * sizeof(uint8_t)); }

/******************************************************************************/
void /* "constant" */	do_constant(void)
/*******************************************************************************
	CONSTANT	 
( x "<spaces>name" -- )
	Skip leading space delimiters. Parse 'name' delimited by
	a space. Create a definition for 'name' with the execution
	semantics defined below.
	'name' is referred to as a ``constant''.
\execute[name]
	(  -- x)
	Place 'x' on the stack.
*******************************************************************************/
		{ do_create(); * here.cell ++ = pop(); latest->cfa = runtime_constant; }

/******************************************************************************/
void /* "count" */	do_count(void)
/*******************************************************************************
	COUNT	 
( c-addr_1 -- c-addr_2 u)
	Return the character string specification for the counted
	string stored at 'c-addr_1'. 'c-addr_2' is the
	address of the first character after 'c-addr_1'. 'u'
	is the contents of the character at 'c-addr_1', which is
	the length in characters of the string at 'c-addr_2'.
*******************************************************************************/
		{ uint8_t * s = (uint8_t *) pop(); push((cell) s + 1); push(* s); }

/******************************************************************************/
void /* "cr" */		do_cr(void)
/*******************************************************************************
	CR	[c-r] 
(  -- )
	Cause subsequent output to appear at the beginning of the next
	line.
*******************************************************************************/
		{ push('\n'); do_emit(); }

/******************************************************************************/
void /* "create" */	do_create(void)
/*******************************************************************************
	CREATE	 
( "<spaces>name" -- )
	Skip leading space delimiters. Parse 'name' delimited by a
	space. Create a definition for 'name' with the execution
	semantics defined below. If the data-space pointer is not
	aligned, reserve enough data space to align it. The new
	data-space pointer defines 'name''s data field.
	'CREATE' does not allocate data space in 'name''s
	data field.
\execute[name]
	(  -- a-addr)
	'a-addr' is the address of 'name''s data field.
	The execution semantics of 'name' may be extended by
	using 'DOES'.
*******************************************************************************/
{
cell x;
uint8_t * s;
struct word * w;
	do_bl();
	do_word();
	do_count();
	s = here.chr;
	x = pop();
	if (!x) strabort(__func__, "refusing to use a zero-length string as a name");
	* s = x;
	xmemcpy((char *) (s + 1), (char *) pop(), x);
	here.chr += x + 1;
	do_align();
	w = (struct word *) here.cell;
	xmemset(w, 0, sizeof * w);
	w->link = latest;
	w->name = (union cstr *) s;
	w->cfa = runtime_create;
	latest = w;
	here.chr += sizeof(struct word);
	do_align();
}

/******************************************************************************/
void /* "decimal" */	do_decimal(void)
/*******************************************************************************
	DECIMAL	 
(  -- )
	Set the numeric conversion radix to ten (decimal).
*******************************************************************************/
		{ base = 10; }

/******************************************************************************/
void /* "depth" */	do_depth(void)
/*******************************************************************************
	DEPTH	 
(  -- +n)
	'+n' is the number of single-cell values contained in the
	data stack before '+n' was placed on the stack.
*******************************************************************************/
		{ spush(sp); }

/******************************************************************************/
void /* "do" */		do_do(void)
/*******************************************************************************
	DO	 
\interpret
	Interpretation semantics for this word are undefined.
\compile
	( C]{ -- do-sys)
	Place 'do-sys' onto the control-flow stack. Append the
	run-time semantics given below to the current definition. The
	semantics are incomplete until resolved by a consumer of
	'do-sys' such as 'LOOP'.
\runtime
	( n_1|u_1 n_2|u_2 -- )
	( R]{ -- loop-sys)
	Set up loop control parameters with index 'n_2|u_2' and
	limit 'n_1|u_1'. An ambiguous condition exists if
	'n_1|u_1' and 'n_2|u_2' are not both the same
	type. Anything already on the return stack becomes unavailable
	until the loop-control parameters are discarded.
*******************************************************************************/
		{ postpone("2>r"); do_begin(); /* initialize 'leave' counter; this will be used for resolving 'leave's by 'loop'/'+loop' */ rpush(0); }

/******************************************************************************/
void /* "does>" */	do_does(void)
/*******************************************************************************
	DOES>	[does] 
\interpret
	Interpretation semantics for this word are undefined.
\compile
	( C]{colon-sys_1 -- colon-sys_2)
	Append the run-time semantics below to the current definition.
	Whether or not the current definition is rendered findable in
	the dictionary by the compilation of 'DOES' is
	implementation defined. Consume 'colon-sys_1' and
	produce 'colon-sys_2'. Append the initiation semantics
	given below to the current definition.
\runtime
	(  -- )
	( R]{nest-sys_1 -- )
	Replace the execution semantics of the most recent definition,
	referred to as 'name', with the 'name' execution
	semantics given below. Return control to the calling definition
	specified by 'nest-sys_1'. An ambiguous condition exists
	if 'name' was not defined with 'CREATE' or a
	user-defined word that calls 'CREATE'.
\init
	( i*x -- i*x a-addr)
	( R]{ -- nest-sys_2)
	Save implementation-dependent information 'nest-sys_2'
	about the calling definition. Place 'name''s data field
	address on the stack. The stack effects 'i*x' represent
	arguments to 'name'.
\execute[name]
	( i*x -- j*x)
	Execute the portion of the definition that begins with the
	initiation semantics appended by the 'DOES' which modified
	'name'. The stack effects 'i*x' and 'j*x'
	represent arguments to and results from 'name',
	respectively.
*******************************************************************************/
{
	* here.word ++ = & xt_runtime_does;
	/* compile 'exit' */
	* here.word ++ = 0;
}

/******************************************************************************/
void /* "drop" */	        do_drop(void)
/*******************************************************************************
	DROP	 
( x -- )
	Remove 'x' from the stack.
*******************************************************************************/
		{ pop(); }

/******************************************************************************/
void /* "dup" */	        do_dup(void)
/*******************************************************************************
	DUP	[dupe] 
( x -- x x)
	Duplicate 'x'.
*******************************************************************************/
		{ ddup(); }

/******************************************************************************/
void /* "else" */	        do_else(void)
/*******************************************************************************
	ELSE	 
\interpret
	Interpretation semantics for this word are undefined.
\compile
	( C]{orig_1 -- orig_2)
	Put the location of a new unresolved forward reference
	'orig_2' onto the control flow stack. Append the run-time
	semantics given below to the current definition. The semantics
	will be incomplete until 'orig_2' is resolved (e.g., by
	'THEN'). Resolve the forward reference 'orig_1' using
	the location following the appended run-time semantics.
\runtime
	(  -- )
	Continue execution at the location given by the resolution of
	'orig_2'.
*******************************************************************************/
		{ do_ahead(); swap(); do_then(); }

/******************************************************************************/
void /* "emit" */	        do_emit(void)
/*******************************************************************************
	EMIT	 
( x -- )
	If 'x' is a graphic character in the implementation-defined
	character set, display 'x'. The effect of 'EMIT' for all
	other values of 'x' is implementation-defined.
	When passed a character whose character-defining bits have a
	value between hex 20 and 7E inclusive, the corresponding
	characters]{usage:ASCII}, is displayed. Because different output
	devices can respond differently to control characters, programs
	that use control characters to perform specific functions have
	an environmental dependency. Each EMIT deals with only one
	character.
*******************************************************************************/
		{ sfputc(pop()); }

/******************************************************************************/
/* void  "environment?"  do_environment(void) */
/*******************************************************************************
	ENVIRONMENT?	[environment-query] 
( c-addr u -- false | i*x true)
	'c-addr' is the address of a character string and 'u'
	is the string's character count. 'u' may have a value in
	the range from zero to an implementation-defined maximum which
	shall not be less than 31. The character string should contain a
	optional word sets to be checked for correspondence with an
	attribute of the present environment. If the system treats the
	attribute as unknown, the returned flag is 'false';
	otherwise, the flag is 'true' and the 'i*x' returned
	is of the type specified in the table for the attribute queried.
*******************************************************************************/
// THIS WORD IS NOT IMPLEMENTED; IF YOU NEED IT, YOU MUST DEFINE IT YOURSELF
/*
		{ xxx; }

*/
/******************************************************************************/
void /* "evaluate" */     do_evaluate(void)
/*******************************************************************************
	EVALUATE	 
( i*x c-addr u -- j*x)
	Save the current input source specification. Store minus-one
	(-1) in 'SOURCE-ID' if it is present. Make the string
	described by 'c-addr' and 'u' both the input source
	and input buffer, set 'toIN' to zero, and interpret. When
	the parse area is empty, restore the prior input source
	specification. Other stack effects are due to the words
	'EVALUATE'd.
*******************************************************************************/
{
struct input_spec prev_input_spec = input_spec;
bool x;
	if (!(x = is_jmpenv_valid))
	{
		if (setjmp(jmpenv))
		{
			/* exception taken */
			is_jmpenv_valid = false;
			return;
		}
		is_jmpenv_valid = true;
	}
	input_spec.source_id = SOURCE_ID_STRING; input_spec.idx = 0; input_spec.len = pop(); input_spec.str = (const uint8_t *) pop();
	interpret();
	input_spec = prev_input_spec;
	is_jmpenv_valid = x;
}

/******************************************************************************/
void /* "execute" */	do_execute(void)
/*******************************************************************************
	EXECUTE	 
( i*x xt -- j*x)
	Remove 'xt' from the stack and perform the semantics
	identified by it. Other stack effects are due to the word
	'EXECUTE'd.
*******************************************************************************/
		{ struct word * w = (struct word *) pop(); WP = w; if (!w->is_does_proper) w->cfa(); else exec_does(); }

/******************************************************************************/
void /* "exit" */	        do_exit(void)
/*******************************************************************************
	EXIT	 
\interpret
	Interpretation semantics for this word are undefined.
\execute
	(  -- )
	( R]{nest-sys -- )
	Return control to the calling definition specified by
	'nest-sys'. Before executing 'EXIT' within a
	do-loop, a program shall discard the loop-control parameters
	by executing 'UNLOOP'.
*******************************************************************************/
		{ * here.cell ++ = 0; }

/******************************************************************************/
void /* "fill" */	        do_fill(void)
/*******************************************************************************
	FILL	 
( c-addr u char -- )
	If 'u' is greater than zero, store 'char' in each of
	'u' consecutive characters of memory beginning at
	'c-addr'.
*******************************************************************************/
		{ cell c = pop(), i = pop(); xmemset((void *) pop(), c, i); }

/******************************************************************************/
void /* "find" */	        do_find(void)
/*******************************************************************************
	FIND	 
( c-addr -- c-addr 0 | xt 1 | xt -1)
	Find the definition named in the counted string at 'c-addr'.
	If the definition is not found, return 'c-addr' and zero.
	If the definition is found, return its execution token 'xt'.
	If the definition is immediate, also return one ('1'),
	otherwise also return minus-one ('-1'). For a given string,
	the values returned by 'FIND' while compiling may differ
	from those returned while not compiling.
*******************************************************************************/
{
struct word * w;
char * s;
int len;
	w = latest;
	s = (char *) pop();
	len = *((unsigned char *) s);
	if (len)
		while (w)
		{
			if (!w->is_smudged)
				/*
				if (!memcmp(s, w->name->str, len + 1))
				*/
				if (/* this is possible because of the
				     * new ':noname' code */ w->name)
					if (!xstrncasecmp(s, (char*) w->name->str, len + 1))
						break;
			w = w->link;
		}
	else
		w = 0;
	if (w)
	{
		push((cell) w);
		push(w->is_immediate ? 1 : -1);
	}
	else
	{
		push((cell) s);
		push(0);
	}
}

/******************************************************************************/
void /* "fm/mod" */	do_f_m_slash_mod(void)
/*******************************************************************************
	FM/MOD	[f-m-slash-mod] 
( d_1 n_1 -- n_2 n_3)
	Divide 'd_1' by 'n_1', giving the floored quotient
	'n_3' and the remainder 'n_2'. Input and output stack
	arguments are signed. An ambiguous condition exists if
	'n_1' is zero or if the quotient lies outside the range of
	a single-cell signed integer.
*******************************************************************************/
		{ sdcell d1; scell n1, q, r; n1 = spop(); d1 = spopd(); if (!n1) sabort("divide by zero"); q = d1 / n1; r = d1 % n1; /* fixup */ if (r && ((d1 < 0 && n1 > 0) || (d1 > 0 && n1 < 0))) q --, r += n1; spush(r); spush(q); }

/******************************************************************************/
void /* "here" */		do_here(void)
/*******************************************************************************
	HERE	 
(  -- addr)
	'addr' is the data-space pointer.
*******************************************************************************/
		{ push((cell) here.cell); }

/******************************************************************************/
void /* "hold" */		do_hold(void)
/*******************************************************************************
	HOLD	 
( char -- )
	Add 'char' to the beginning of the pictured numeric output
	string. An ambiguous condition exists if 'HOLD' executes
	outside of a 'num-start' 'num-end' delimited number
	conversion.
*******************************************************************************/
		{ pno.buf[-- pno.idx] = pop(); }

/******************************************************************************/
void /* "i" */		do_i(void)
/*******************************************************************************
	I	 
\interpret
	Interpretation semantics for this word are undefined.
\execute
	(  -- n|u)
	( R]{loop-sys -- loop-sys)
	'n|u' is a copy of the current (innermost) loop index.
	An ambiguous condition exists if the loop control parameters
	are unavailable.
*******************************************************************************/
		{ if (rsp < 2) sabort("return stack underflow"); push(rstack[rsp - 1]); }

/******************************************************************************/
void /* "if" */		do_if(void)
/*******************************************************************************
	IF	 
\interpret
	Interpretation semantics for this word are undefined.
\compile
	( C]{ -- orig)
	Put the location of a new unresolved forward reference
	'orig' onto the control flow stack. Append the run-time
	semantics given below to the current definition. The semantics
	are incomplete until 'orig' is resolved, e.g., by
	'THEN' or 'ELSE'.
\runtime
	( x -- )
	If all bits of 'x' are zero, continue execution at the
	location specified by the resolution of 'orig'.
*******************************************************************************/
		{ * here.word ++ = & xt_runtime_branch_on_false; push((cell) here.cell ++); }

/******************************************************************************/
void /* "immediate" */	do_immediate(void)
/*******************************************************************************
	IMMEDIATE	 
(  -- )
	Make the most recent definition an immediate word. An ambiguous
	condition exists if the most recent definition does not have a
	name or if it was defined as a 'tools'{SYNONYM}.
*******************************************************************************/
		{ latest->is_immediate = 1; }

/******************************************************************************/
void /* "invert" */	do_invert(void)
/*******************************************************************************
	INVERT	 
( x_1 -- x_2)
	Invert all bits of 'x_1', giving its logical inverse
	'x_2'.
*******************************************************************************/
		{ push(~ pop()); }

/******************************************************************************/
void /* "j" */		do_j(void)
/*******************************************************************************
	J	 
\interpret
	Interpretation semantics for this word are undefined.
\execute
	(  -- n|u)
	( R]{loop-sys_1 loop-sys_2 -- loop-sys_1 loop-sys_2)
	'n|u' is a copy of the next-outer loop index. An
	ambiguous condition exists if the loop control parameters of
	the next-outer loop, 'loop-sys_1', are unavailable.
*******************************************************************************/
		{ if (rsp < 4) sabort("return stack underflow"); push(rstack[rsp - 3]); }

/******************************************************************************/
/* void  "key",		do_key (void) */

/*******************************************************************************
	KEY	 
(  -- char)
	Receive one character 'char', a member of the
	implementation-defined character set. Keyboard events that do
	not correspond to such characters are discarded until a valid
	character is received, and those events are subsequently
	unavailable.
	All standard characters can be received. Characters received by
	'KEY' are not displayed.
	Any standard character returned by 'KEY' has the numeric
	Programs that require the ability to receive control characters
	have an environmental dependency.
*******************************************************************************/
// THIS WORD IS NOT IMPLEMENTED; IF YOU NEED IT, YOU MUST DEFINE IT YOURSELF
/*******************************************************************************
	LEAVE	 
\interpret
	Interpretation semantics for this word are undefined.
\execute
	(  -- )
	( R]{loop-sys -- )
	Discard the current loop control parameters. An ambiguous condition
	exists if they are unavailable. Continue execution immediately
	following the innermost syntactically enclosing
	'DO'\ldots'LOOP' or 'DO'\ldots'+LOOP'.
*******************************************************************************/
/******************************************************************************/
void /* "leave" */	do_leave(void)
{
	cell x = rpop();
	do_unloop();
	do_ahead();
	do_to_r();
	rpush(x + 1);
}

/*******************************************************************************
	LITERAL	 
\interpret
	Interpretation semantics for this word are undefined.
\compile
	( x -- )
	Append the run-time semantics given below to the current definition.
\runtime
	(  -- x)
	Place 'x' on the stack.
*******************************************************************************/
/******************************************************************************/
void /* "literal" */	do_literal(void)
		{ * here.word ++ = & xt_runtime_literal; * here.cell ++ = pop(); }

/*******************************************************************************
	LOOP	 
\interpret
	Interpretation semantics for this word are undefined.
\compile
	( C]{do-sys -- )
	Append the run-time semantics given below to the current
	definition. Resolve the destination of all unresolved
	occurrences of 'LEAVE' between the location given by
	'do-sys' and the next location for a transfer of
	control, to execute the words following the 'LOOP'.
\runtime
	(  -- )
	( R]{loop-sys_1 -- | loop-sys_2)
	An ambiguous condition exists if the loop control parameters are
	unavailable. Add one to the loop index. If the loop index is then
	equal to the loop limit, discard the loop parameters and continue
	execution immediately following the loop. Otherwise continue
	execution at the beginning of the loop.
*******************************************************************************/
/******************************************************************************/
void /* "loop" */		do_loop(void)
{
cell i;
	* here.word ++ = & xt_runtime_loop;
	do_until();
	postpone("2r>");
	postpone("2drop");
	i = rpop();
	while (i --)
	{
		do_r_from();
		do_then();
	}
}

/******************************************************************************/
void /* "lshift" */	do_lshift(void)
/*******************************************************************************
	LSHIFT	[l-shift] 
( x_1 u -- x_2)
	Perform a logical left shift of 'u' bit-places on
	'x_1', giving 'x_2'. Put zeroes into the least
	significant bits vacated by the shift. An ambiguous condition
	exists if 'u' is greater than or equal to the number of
	bits in a cell.
*******************************************************************************/
		{ dcell t = popd(); cell x, y; x = (cell) t; y = (cell)(t >> CELL_NR_BITS); push(x << y); }

/******************************************************************************/
void /* "m*" */		do_m_star(void)
/*******************************************************************************
	M*	[m-star] 
( n_1 n_2 -- d)
	'd' is the signed product of 'n_1' times 'n_2'.
*******************************************************************************/
		{ dcell x = popd(); spushd((sdcell)(scell)(cell)(x >> CELL_NR_BITS) * (sdcell)(scell)(cell) x); }

/******************************************************************************/
void /* "max" */		do_max(void)
/*******************************************************************************
	MAX	 
( n_1 n_2 -- n_3)
	'n_3' is the greater of 'n_1' and 'n_2'.
*******************************************************************************/
		{ dcell t = popd(); scell x, y; x = (cell) t; y = (cell)(t >> CELL_NR_BITS); spush(x > y ? x : y); }

/******************************************************************************/
void /* "min" */		do_min(void)
/*******************************************************************************
	MIN	 
( n_1 n_2 -- n_3)
	'n_3' is the lesser of 'n_1' and 'n_2'.
*******************************************************************************/
		{ dcell t = popd(); scell x, y; x = (cell) t; y = (cell)(t >> CELL_NR_BITS); spush(x < y ? x : y); }

/******************************************************************************/
void /* "mod" */		do_mod(void)
/*******************************************************************************
	MOD	 
( n_1 n_2 -- n_3)
	Divide 'n_1' by 'n_2', giving the single-cell remainder
	'n_3'. An ambiguous condition exists if 'n_2' is zero.
	If 'n_1' and 'n_2' differ in sign, the
	implementation-defined result returned will be the same as that
	returned by either the phrase
	'toR' 'StoD' 'Rfrom' 'FM/MOD' 'DROP'
	or the phrase
	'toR' 'StoD' 'Rfrom' 'SM/REM' 'DROP'.
*******************************************************************************/
		{ dcell x = popd(); if (!(cell)(x >> CELL_NR_BITS)) sabort("divide by zero"); push(((scell) (cell) x) % ((scell)(cell)(x >> CELL_NR_BITS))); }

/******************************************************************************/
void /* "move" */		do_move(void)
/*******************************************************************************
	MOVE	 
( addr_1 addr_2 u -- )
	If 'u' is greater than zero, copy the contents of 'u'
	consecutive address units at 'addr_1' to the 'u'
	consecutive address units at 'addr_2'. After 'MOVE'
	completes, the 'u' consecutive address units at 'addr_2'
	contain exactly what the 'u' consecutive address units at
	'addr_1' contained before the move.
*******************************************************************************/
		{ cell i = pop(); uint8_t * dest = (uint8_t *) pop(), * src = (uint8_t *) pop(); xmemmove(dest, src, i); }

/******************************************************************************/
void /* "negate" */	do_negate(void)
/*******************************************************************************
	NEGATE	 
( n_1 -- n_2)
	Negate 'n_1', giving its arithmetic inverse 'n_2'.
*******************************************************************************/
		{ spush(- spop()); }

/******************************************************************************/
void /* "or" */		do_or(void)
/*******************************************************************************
	OR	 
( x_1 x_2 -- x_3)
	'x_3' is the bit-by-bit inclusive-or of 'x_1' with
	'x_2'.
*******************************************************************************/
		{ dcell x = popd(); push((cell)(x >> CELL_NR_BITS) | (cell) x); }

/******************************************************************************/
void /* "over" */		do_over(void)
/*******************************************************************************
	OVER	 
( x_1 x_2 -- x_1 x_2 x_1)
	Place a copy of 'x_1' on top of the stack.
*******************************************************************************/
		{ over(); }

/*******************************************************************************
	POSTPONE	 
\interpret
	Interpretation semantics for this word are undefined.
\compile
	( "<spaces>name" -- )
	Skip leading space delimiters. Parse 'name' delimited by
	a space. Find 'name'. Append the compilation semantics of
	'name' to the current definition. An ambiguous condition
	exists if 'name' is not found.
*******************************************************************************/
/******************************************************************************/
void /* "postpone" */	do_postpone(void)
{
	do_bl();
	do_word();
	do_find();
	switch (spop())
	{
		case 0:
			do_count();
			do_type();
			sabort(": word not found");
			break;
		case 1:
			/* immediate word */
			do_compile_comma();
			break;
		case -1:
			/* non immediate word */
			do_literal();
			postpone("compile,");
			break;
	}
}

/******************************************************************************/
void /* "quit" */		do_quit(void)
/*******************************************************************************
	QUIT	 
(  -- )
	( R]{i*x -- )
	Empty the return stack, store zero in 'SOURCE-ID' if it is
	present, make the user input device the input source, and enter
	interpretation state. Do not display a message. Repeat the
	following:
	\begin{itemize}
	\item Accept a line from the input source into the input buffer,
		set 'toIN' to zero, and interpret.
	\item Display the implementation-defined system prompt if in
		interpretation state, all processing has been completed,
		and no ambiguous condition exists.
	\end{itemize}
	\begin{implement}
\cbstart\patch{ed12}
		\uline{':' 'QUIT'} \\
		\tab \uline{'p' \textdf{empty the return stack and set the input source to the user input device} )} \\
		\tab \uline{'POSTPONE' '['} \\
		\tab[2] \uline{'REFILL'} \\
		\tab \uline{'WHILE'} \\
		\tab[2] \uline{'[']' INTERPRET 'exception'{CATCH}} \\
		\tab[2] \uline{'CASE'} \\
		\tab[2] \uline{~0 'OF' 'STATE' '@' '0=' 'IF'
			'.q' OK" 'THEN' 'CR' 'ENDOF'} \\
		\tab[2] \uline{-1 'OF' 'p' \textdf{Aborted} ) 'ENDOF'} \\
		\tab[2] \uline{-2 'OF' 'p' \textdf{display message from 'ABORTq'} ) 'ENDOF'} \\
		\tab[2] \uline{'p' \textdf{default} ) 'DUP' '.q' Exception \# " 'd'} \\
		\tab[2] \uline{'ENDCASE'} \\
		\tab \uline{'REPEAT' 'tools'{BYE}} \\
		\uline{';'}
		\dffamily
\uline{%
		This assumes the existence of a system-implementation word
		{\tt INTERPRET} that embodies the text interpreter semantics described in}
\cbend
	\end{implement}
*******************************************************************************/
{
bool x;
	if (!(x = is_jmpenv_valid))
	{
		int exc_code;
		if ((exc_code = setjmp(jmpenv)))
		{
			/* exception taken - check reason */
			if (exc_code == SF_EXC_CODE_BYE)
			{
				/* the 'bye' word was executed - abort the 'quit' interpreter */
				is_jmpenv_valid = false;
				return;
			}
			/* for other exception reasons - continue interpreting */
		}
		is_jmpenv_valid = true;
	}
	rsp = 0;
	input_spec.source_id = SOURCE_ID_USER_INPUT_DEVICE;
	input_spec.buf = inbuf_area;
	input_spec.idx = input_spec.len = 0;
	state = STATE_INTERPRETING;
	while (1)
	{
		if (input_spec.idx == input_spec.len)
		{
			if (state == STATE_INTERPRETING)
			{
				print_str("sforth:sp:");
				print_udecimal(sp);
				print_str(":rsp:");
				print_udecimal(rsp);
				print_str("> ");
				sfsync();
			}
			do_refill();
			if (!pop())
				/* end of input */
				break;
		}
		interpret();
	}
	print_str("aborting quit: input exhausted\n");
	is_jmpenv_valid = x;
}

/******************************************************************************/
void /* "r>" */		do_r_from(void)
/*******************************************************************************
	R>	[r-from] 
\interpret
	Interpretation semantics for this word are undefined.
\execute
	(  -- x)
	( R]{x -- )
	Move 'x' from the return stack to the data stack.
*******************************************************************************/
		{ push(rpop()); }

/******************************************************************************/
void /* "r@" */	        do_r_fetch(void)
/*******************************************************************************
	R@	[r-fetch] 
\interpret
	Interpretation semantics for this word are undefined.
\execute
	(  -- x)
	( R]{x -- x)
	Copy 'x' from the return stack to the data stack.
*******************************************************************************/
		{ push(rtop()); }

/******************************************************************************/
void /* "recurse" */      do_recurse(void)
/*******************************************************************************
	RECURSE	 
\interpret
	Interpretation semantics for this word are undefined.
\compile
	(  -- )
	Append the execution semantics of the current definition to
	the current definition. An ambiguous condition exists if
	'RECURSE' appears in a definition after 'DOES'.
*******************************************************************************/
		{ * here.word ++ = latest;  }

/******************************************************************************/
void /* "repeat" */	do_repeat(void)
/*******************************************************************************
	REPEAT	 
\interpret
	Interpretation semantics for this word are undefined.
\compile
	( C]{orig dest -- )
	Append the run-time semantics given below to the current
	definition, resolving the backward reference 'dest'.
	Resolve the forward reference 'orig' using the location
	following the appended run-time semantics.
\runtime
	(  -- )
	Continue execution at the location given by 'dest'.
*******************************************************************************/
		{ * here.word ++ = & xt_runtime_branch; * here.cell = (cell)((const struct word **) pop() - here.word); here.cell ++; do_then(); }

/******************************************************************************/
void /* "rot" */	        do_rot(void)
/*******************************************************************************
	ROT	[rote] 
( x_1 x_2 x_3 -- x_2 x_3 x_1)
	Rotate the top three stack entries.
*******************************************************************************/
		{ rot(); }

/******************************************************************************/
void /* "rshift" */	do_rshift(void)
/*******************************************************************************
	RSHIFT	[r-shift] 
( x_1 u -- x_2)
	Perform a logical right shift of 'u' bit-places on
	'x_1', giving 'x_2'. Put zeroes into the most
	significant bits vacated by the shift. An ambiguous condition
	exists if 'u' is greater than or equal to the number of
	bits in a cell.
*******************************************************************************/
		{ dcell t = popd(); cell x, y; x = (cell) t; y = (cell)(t >> CELL_NR_BITS); push(x >> y); }

/******************************************************************************/
void /* "s\"" */	        do_s_quote(void)
/*******************************************************************************
	S"	[s-quote] 
\interpret
	Interpretation semantics for this word are undefined.
\compile
	( "ccc<quote>" -- )
	Parse 'ccc' delimited by " (double-quote).
	Append the run-time semantics given below to the current
	definition.
\runtime
	(  -- c-addr u)
	Return 'c-addr' and 'u' describing a string
	consisting of the characters 'ccc'. A program shall
	not alter the returned string.
*******************************************************************************/
{ do_c_quote(); postpone("count"); }

/******************************************************************************/
void /* "s>d" */	        do_s_to_d(void)
/*******************************************************************************
	S>D	[s-to-d] 
( n -- d)
	Convert the number 'n' to the double-cell number 'd'
	with the same numerical value.
*******************************************************************************/
		{ spushd((sdcell) spop()); }

/******************************************************************************/
void /* "sign" */	        do_sign(void)
/*******************************************************************************
	SIGN	 
( n -- )
	If 'n' is negative, add a minus sign to the beginning of
	the pictured numeric output string. An ambiguous condition exists
	if 'SIGN' executes outside of a 'num-start' 'num-end'
	delimited number conversion.
*******************************************************************************/
		{ if (spop() < 0) pno.buf[-- pno.idx] = '-'; }

/******************************************************************************/
void /* "sm/rem" */	do_s_m_slash_rem(void)
/*******************************************************************************
	SM/REM	[s-m-slash-rem] 
( d_1 n_1 -- n_2 n_3)
	Divide 'd_1' by 'n_1', giving the symmetric quotient
	'n_3' and the remainder 'n_2'. Input and output stack
	arguments are signed. An ambiguous condition exists if 'n_1'
	is zero or if the quotient lies outside the range of a single-cell
	signed integer.
*******************************************************************************/
		{ sdcell x; scell q, r, y; y = spop(); x = spopd(); if (y == 0) sabort("divide by zero"); q = x / y; r = x % y; spush(r); spush(q); }

/******************************************************************************/
void /* "source" */	do_source(void)
/*******************************************************************************
	SOURCE	 
(  -- c-addr u)
	'c-addr' is the address of, and 'u' is the number of
	characters in, the input buffer.
*******************************************************************************/
{
	switch (input_spec.source_id)
	{
		default:
			/* input is a file */
		case SOURCE_ID_USER_INPUT_DEVICE:
			push((cell) input_spec.buf);
			break;
		case SOURCE_ID_STRING:
			push((cell) input_spec.str);
			break;
	}
	push(input_spec.len);
}

/******************************************************************************/
void /* "space" */	do_space(void)
/*******************************************************************************
	SPACE	 
(  -- )
	Display one space.
*******************************************************************************/
		{ do_bl(); do_emit(); }

/******************************************************************************/
void /* "spaces" */	do_spaces(void)
/*******************************************************************************
	SPACES	 
( n -- )
	If 'n' is greater than zero, display 'n' spaces.
*******************************************************************************/
		{ cell i = pop(); while (i --) do_space(); }

/******************************************************************************/
void /* "state" */	do_state(void)
/*******************************************************************************
	STATE	 
(  -- a-addr)
	'a-addr' is the address of a cell containing the
	compilation-state flag. 'STATE' is \emph{true} when in
	compilation state, \emph{false} otherwise. The \emph{true} value
	in 'STATE' is non-zero, but is otherwise
	implementation-defined. Only the following standard words alter
	the value in 'STATE':
	':' (colon),
	';' (semicolon),
	'ABORT',
	'QUIT',
	':NONAME',
	'[' (left-bracket),
	']' (right-bracket).
\note
	A program shall not directly alter the contents of 'STATE'.
*******************************************************************************/
		{ push((cell) & state); }

/******************************************************************************/
void /* "swap" */		do_swap(void)
/*******************************************************************************
	SWAP	 
( x_1 x_2 -- x_2 x_1)
	Exchange the top two stack items.
*******************************************************************************/
		{ swap(); }

/******************************************************************************/
void /* "then" */		do_then(void)
/*******************************************************************************
	THEN	 
\interpret
	Interpretation semantics for this word are undefined.
\compile
	( C]{orig -- )
	Append the run-time semantics given below to the current
	definition. Resolve the forward reference 'orig' using
	the location of the appended run-time semantics.
\runtime
	(  -- )
	Continue execution.
*******************************************************************************/
		{ cell * p = (cell *) pop(); * p = (cell)(here.cell - p); }

/******************************************************************************/
void /* "type" */		do_type(void)
/*******************************************************************************
	TYPE	 
( c-addr u -- )
	If 'u' is greater than zero, display the character string
	specified by 'c-addr' and 'u'.
	When passed a character in a character string whose
	character-defining bits have a value between hex 20 and 7E
	inclusive, the corresponding standard character, specified
	Because different output devices can respond differently to
	control characters, programs that use control characters to
	perform specific functions have an environmental dependency.
*******************************************************************************/
		{ cell i = pop(); uint8_t * s = (uint8_t *) pop(); while (i --) push ((cell) * s ++), do_emit(); }

/******************************************************************************/
void /* "u." */		do_u_dot(void)
/*******************************************************************************
	U.	[u-dot] 
( u -- )
	Display 'u' in free field format.
*******************************************************************************/
		{ push(0); do_less_number_sign(); do_number_sign_s(); do_number_sign_greater(); do_type(); do_space(); }

/******************************************************************************/
void /* "u<" */		do_u_less_than(void)
/*******************************************************************************
	U<	[u-less-than] 
( u_1 u_2 -- flag)
	'flag' is true if and only if 'u_1' is less than
	'u_2'.
*******************************************************************************/
		{ dcell x = popd(); push(((cell) x < (cell)(x >> CELL_NR_BITS)) ? C_TRUE : C_FALSE); }

/******************************************************************************/
void /* "um*" */		do_u_m_star(void)
/*******************************************************************************
	UM*	[u-m-star] 
( u_1 u_2 -- ud)
	Multiply 'u_1' by 'u_2', giving the unsigned double-cell
	product 'ud'. All values and arithmetic are unsigned.
*******************************************************************************/
		{ dcell x = popd(); pushd((dcell)(cell)(x >> CELL_NR_BITS) * (dcell)(cell) x); }

/******************************************************************************/
void /* "um/mod" */	do_u_m_slash_mod(void)
/*******************************************************************************
	UM/MOD	[u-m-slash-mod] 
( ud u_1 -- u_2 u_3)
	Divide 'ud' by 'u_1', giving the quotient 'u_3'
	and the remainder 'u_2'. All values and arithmetic are
	unsigned. An ambiguous condition exists if 'u_1' is zero or
	if the quotient lies outside the range of a single-cell unsigned
	integer.
*******************************************************************************/
		{ dcell ud; cell u1 = pop(), q, r; ud = popd(); q = ud / u1; r = ud % u1; push(r); push(q); }

/******************************************************************************/
void /* "unloop" */	do_unloop(void)
/*******************************************************************************
	UNLOOP	 
\interpret
	Interpretation semantics for this word are undefined.
\execute
	(  -- )
	( R]{loop-sys -- )
	Discard the loop-control parameters for the current nesting
	level. An 'UNLOOP' is required for each nesting level
	before the definition may be 'EXIT'ed. An ambiguous
	condition exists if the loop-control parameters are unavailable.
*******************************************************************************/
		{ postpone("2r>"); postpone("2drop"); }

/******************************************************************************/
void /* "until" */	do_until(void)
/*******************************************************************************
	UNTIL	 
\interpret
	Interpretation semantics for this word are undefined.
\compile
	( C]{dest -- )
	Append the run-time semantics given below to the current
	definition, resolving the backward reference 'dest'.
\runtime
	( x -- )
	If all bits of 'x' are zero, continue execution at the
	location specified by 'dest'.
*******************************************************************************/
		// !!! gcc bug ??? - the row below does not work under mingw32, it works under linux
		// looks like this is related to some undefined behaviour, which is not reported
		// as a warning by gcc, though
		//{ * here.word ++ = & xt_runtime_branch_on_false; * here.cell ++ = (cell) ((const struct word **) pop() - here.word); }
		{ * here.word ++ = & xt_runtime_branch_on_false; * here.cell = (cell) ((const struct word **) pop() - here.word); here.cell ++; }

/******************************************************************************/
void /* "variable" */	do_variable(void)
/*******************************************************************************
	VARIABLE	 
( "<spaces>name" -- )
	Skip leading space delimiters. Parse 'name' delimited by
	a space. Create a definition for 'name' with the execution
	semantics defined below. Reserve one cell of data space at an
	aligned address.
	'name' is referred to as a ``variable''.
\execute[name]
	(  -- a-addr)
	'a-addr' is the address of the reserved cell. A program
	is responsible for initializing the contents of the reserved
	cell.
*******************************************************************************/
		{ do_create(); here.cell ++; }

/******************************************************************************/
void /* "while" */	do_while(void)
/*******************************************************************************
	WHILE	 
\interpret
	Interpretation semantics for this word are undefined.
\compile
	( C]{dest -- orig dest)
	Put the location of a new unresolved forward reference
	'orig' onto the control flow stack, under the existing
	'dest'. Append the run-time semantics given below to the
	current definition. The semantics are incomplete until
	'orig' and 'dest' are resolved (e.g., by
	'REPEAT').
\runtime
	( x -- )
	If all bits of 'x' are zero, continue execution at the
	location specified by the resolution of 'orig'.
*******************************************************************************/
		{ push(0); do_literal(); postpone("<>"); do_if(); push(1); roll(); }

/******************************************************************************/
void /* "word" */		do_word(void)
/*******************************************************************************
	WORD	 
( char "<chars>ccc<char>" -- c-addr)
	Skip leading delimiters. Parse characters 'ccc' delimited
	by 'char'.  An ambiguous condition exists if the length of
	the parsed string is greater than the implementation-defined
	length of a counted string.
	'c-addr' is the address of a transient region containing
	the parsed word as a counted string. If the parse area was
	empty or contained no characters other than the delimiter, the
	resulting string has a zero length.
	A program may replace characters within the string.
*******************************************************************************/
{
uint8_t c;
cell i;
const uint8_t * s;
	c = (uint8_t) pop();
	do_source();
	pop();
	s = (const uint8_t *) pop();
	wordpad.len = 0;
	if (input_spec.idx >= input_spec.len)
	{
		push((cell) & wordpad);
		return;
	}
	/* special case for whitespace characters */
	if (xisspace(c))
		while (1)
		{
			i = input_spec.idx;
			while (xisspace(s[i]) && i < input_spec.len)
				i ++;
			input_spec.idx = i;
			if (i == input_spec.len)
			{
				push((cell) & wordpad);
				return;
			}
			break;
		}
	push(c);
	do_parse();
	i = pop();
	if (i > sizeof wordpad.buf - 1)
		i = sizeof wordpad.buf - 1;
	wordpad.len = i;
	xmemcpy(wordpad.buf, (char *) pop(), i);
	/* also null-terminate the string; this is to facilitate numeric conversions
	 * in do_quit() */
	wordpad.buf[i] = 0;
	push((cell) & wordpad);
}

/******************************************************************************/
void /* "xor" */		do_xor(void)
/*******************************************************************************
	XOR	[x-or] 
( x_1 x_2 -- x_3)
	'x_3' is the bit-by-bit exclusive-or of 'x_1' with
	'x_2'.
*******************************************************************************/
		{ dcell res = popd(); push((res >> CELL_NR_BITS) ^ (res & (cell) -1)); }

/******************************************************************************/
void /* "[" */		do_left_bracket(void)
/*******************************************************************************
	[	[left-bracket] 
\interpret
	Interpretation semantics for this word are undefined.
\compile
	Perform the execution semantics given below.
\execute
	(  -- )
	Enter interpretation state. '[' is an immediate word.
*******************************************************************************/
		{ state = STATE_INTERPRETING; }

/******************************************************************************/
void /* "[']" */		do_bracket_tick(void)
/*******************************************************************************
	[']	[bracket-tick] 
\interpret
	Interpretation semantics for this word are undefined.
\compile
	( "<spaces>name" -- )
	Skip leading space delimiters. Parse 'name' delimited by
	a space. Find 'name'. Append the run-time semantics given
	below to the current definition.
	An ambiguous condition exists if 'name' is not found.
\runtime
	(  -- xt)
	Place 'name''s execution token 'xt' on the stack.
	The execution token returned by the compiled phrase
	" ['] X" is the same value returned by
	" ' X" outside of compilation state.
*******************************************************************************/
		{ do_tick(); do_literal(); }

/******************************************************************************/
void /* "[char]" */	do_bracket_char(void)
/*******************************************************************************
	[CHAR]	[bracket-char] 
\interpret
	Interpretation semantics for this word are undefined.
\compile
	( "<spaces>name" -- )
	Skip leading space delimiters. Parse 'name' delimited
	by a space. Append the run-time semantics given below to the
	current definition.
\runtime
	(  -- char)
	Place 'char', the value of the first character of
	'name', on the stack.
*******************************************************************************/
		{ do_bl(); do_word(); do_count(); pop(); push(* (uint8_t *) pop()); do_literal(); }

/******************************************************************************/
void /* "]" */		do_right_bracket(void)
/*******************************************************************************
	]	[right-bracket] 
(  -- )
	Enter compilation state.
*******************************************************************************/
		{ state = STATE_COMPILING; }

/******************************************************************************/
void /* ".(" */		do_dot_paren(void)
/*******************************************************************************
	.(	[dot-paren] 
\compile
	Perform the execution semantics given below.
\execute
	( "ccc<paren>" -- )
	Parse and display 'ccc' delimited by ")" (right
	parenthesis). '.p' is an immediate word.
*******************************************************************************/
{
	while (1)
	{
		push(')');
		do_parse();
		do_two_dup();
		do_type();
		if (input_spec.source_id == SOURCE_ID_STRING)
		{
			pop();
			break;
		}
		else
		{
			/* input is a file or the user input device */
			pop();
			if (input_spec.buf[input_spec.idx - 1] == ')')
				break;
			pop();
			do_refill();
			if (!top())
				/* refill failed */
				break;
			pop();
		}
	}
	pop();
}

/******************************************************************************/
void /* ".r" */		do_dot_r(void)
/*******************************************************************************
	.R	[dot-r] 
( n_1 n_2 -- )
	Display 'n_1' right aligned in a field 'n_2'
	characters wide. If the number of characters required to display
	'n_1' is greater than 'n_2', all digits are displayed
	with no leading spaces in a field as wide as necessary.
*******************************************************************************/
		{ scell x = pop();
		ddup(); do_abs(); push(0); do_less_number_sign(); do_number_sign_s(); rot(); do_sign(); do_number_sign_greater();
		x -= top(); if (x > 0) push(x), do_spaces();
		do_type(); do_space(); }

/******************************************************************************/
void /* "0<>" */		do_zero_not_equals(void)
/*******************************************************************************
	0<>	[zero-not-equals] 
( x -- flag)
	'flag' is true if and only if 'x' is not equal to
	zero.
*******************************************************************************/
		{ push(pop() ? C_TRUE : C_FALSE); }

/******************************************************************************/
void /* "0>" */		do_zero_greater(void)
/*******************************************************************************
	0>	[zero-greater] 
( n -- flag)
	'flag' is true if and only if 'n' is greater than
	zero.
*******************************************************************************/
		{ push((spop() > 0) ? C_TRUE : C_FALSE); }

/******************************************************************************/
void /* "2>r" */	        do_two_to_r(void)
/*******************************************************************************
	2>R	[two-to-r] 
\interpret
	Interpretation semantics for this word are undefined.
\execute
	( x_1 x_2 -- )
	( R]{ -- x_1 x_2)
	Transfer cell pair 'x_1 x_2' to the return stack.
	Semantically equivalent to 'SWAP' 'toR' 'toR'.
*******************************************************************************/
		{ two_to_r(); }

/******************************************************************************/
void /* "2r>" */	        do_two_r_from(void)
/*******************************************************************************
	2R>	[two-r-from] 
\interpret
	Interpretation semantics for this word are undefined.
\execute
	(  -- x_1 x_2)
	( R]{x_1 x_2 -- )
	Transfer cell pair 'x_1 x_2' from the return stack.
	Semantically equivalent to 'Rfrom' 'Rfrom' 'SWAP'.
*******************************************************************************/
		{ two_r_from(); }

/******************************************************************************/
void /* "2r@" */	        do_two_r_fetch(void)
/*******************************************************************************
	2R@	[two-r-fetch] 
\interpret
	Interpretation semantics for this word are undefined.
\execute
	(  -- x_1 x_2)
	( R]{x_1 x_2 -- x_1 x_2)
	Copy cell pair 'x_1 x_2' from the return stack.
	Semantically equivalent to 'Rfrom' 'Rfrom' '2DUP'
	'toR' 'toR' 'SWAP'.
*******************************************************************************/
		{ two_r_fetch(); }

/******************************************************************************/
void /* ":noname" */      do_colon_no_name(void)
/*******************************************************************************
	:NONAME	[colon-no-name] 
( C]{ -- colon-sys)
	( S]{ -- xt)
	Create an execution token 'xt', enter compilation state
	and start the current definition, producing 'colon-sys'.
	Append the initiation semantics given below to the current
	definition.
	The execution semantics of 'xt' will be determined by the
	words compiled into the body of the definition. This definition
	can be executed later by using 'xt' 'EXECUTE'.
	If the control-flow stack is implemented using the data stack,
	'colon-sys' shall be the topmost item on the data stack.
\init
	( i*x -- i*x)
	( R]{ -- nest-sys)
	Save implementation-dependent information 'nest-sys'
	about the calling definition. The stack effects 'i*x'
	represent arguments to 'xt'.
\execute[xt]
	( i*x -- j*x)
	Execute the definition specified by 'xt'. The stack
	effects 'i*x' and 'j*x' represent arguments to
	and results from 'xt', respectively.
*******************************************************************************/
		{ struct word * w; if (state != STATE_INTERPRETING) sabort("bad state"); state = STATE_COMPILING; do_align(); w = (struct word *) here.cell; w->is_does_proper = w->is_immediate = 0; w->cfa = runtime_colon; w->name = 0; w->link = latest; latest = w; push((cell) w); here.chr += sizeof * w; do_align(); }

/******************************************************************************/
void /* "<>" */	        do_not_equals(void)
/*******************************************************************************
	<>	[not-equals] 
( x_1 x_2 -- flag)
	'flag' is true if and only if 'x_1' is not bit-for-bit
	the same as 'x_2'.
*******************************************************************************/
		{ dcell x = popd(); push(((cell) x != (cell)(x >> CELL_NR_BITS)) ? C_TRUE : C_FALSE); }

/******************************************************************************/
void /* "?do" */	        do_question_do(void)
/*******************************************************************************
	?DO	[question-do] 
\interpret
	Interpretation semantics for this word are undefined.
\compile
	( C]{ -- do-sys)
	Put 'do-sys' onto the control-flow stack. Append the
	run-time semantics given below to the current definition. The
	semantics are incomplete until resolved by a consumer of
	\emph{do-sys} such as 'LOOP'.
\runtime
	( n_1|u_1 n_2|u_2 -- )
	( R]{ -- loop-sys)
	If 'n_1|u_1' is equal to 'n_2|u_2', continue
	execution at the location given by the consumer of
	'do-sys'. Otherwise set up loop control parameters with
	index 'n_2|u_2' and limit 'n_1|u_1' and continue
	executing immediately following 'qDO'. Anything already
	on the return stack becomes unavailable until the loop
	control parameters are discarded. An ambiguous condition
	exists if 'n_1|u_1' and 'n_2|u_2' are not both of
	the same type.
*******************************************************************************/
		{ postpone("over"); postpone("over"); postpone("="); do_if(); postpone("drop"); postpone("drop"); do_ahead(); push(1); do_roll(); do_then(); do_do(); rpop(); swap(); do_to_r(); rpush(1); }

/******************************************************************************/
void /* "again" */	do_again(void)
/*******************************************************************************
	AGAIN	 
\interpret
	Interpretation semantics for this word are undefined.
\compile
	( C]{dest -- )
	Append the run-time semantics given below to the current
	definition, resolving the backward reference 'dest'.
\runtime
	(  -- )
	Continue execution at the location specified by 'dest'.
	If no other control flow words are used, any program code
	after 'AGAIN' will not be executed.
*******************************************************************************/
		{ * here.word ++ = &xt_runtime_branch; * here.cell = (cell *) pop() - here.cell; here.cell ++; }

/******************************************************************************/
void /* "buffer:" */	do_buffer_colon(void)
/*******************************************************************************
	BUFFER:	[buffer-colon]
\item \stack{u "<spaces>name"}{}

	Skip leading space delimiters. Parse \param{name} delimited by a space.
	Create a definition for \param{name}, with the execution semantics defined
	below.  Reserve \param{u} address units at an aligned address.
	Contiguity of this region with any other region is undefined.

\execute[name]
	\stack{}{a-addr}

	\param{a-addr} is the address of the space reserved by \word{BUFFER:} when
	it defined \param{name}.  The program is responsible for initializing the
	contents.
*******************************************************************************/
{ do_create(); do_allot(); }

/******************************************************************************/
void /* "c\"" */	        do_c_quote(void)
/*******************************************************************************
% -------------------------------------------------------------------
	C"	[c-quote] 
\interpret
	Interpretation semantics for this word are undefined.
\compile
	( "ccc<quote>" -- )
	Parse 'ccc' delimited by " (double-quote) and
	append the run-time semantics given below to the current
	definition.
\runtime
	(  -- c-addr)
	Return 'c-addr', a counted string consisting of the
	characters 'ccc'. A program shall not alter the returned
	string.
*******************************************************************************/
{
uint8_t * s; cell len; do_ahead(); push('"'); do_parse(); s = here.chr; * s = len = pop(); xmemcpy(s + 1, (void *) pop(), len);
	here.chr += len + 1; do_align(); do_then(); push((cell) s); do_literal();
}

/******************************************************************************/
void /* "case" */	        do_case(void)
/*******************************************************************************
	CASE	 
\interpret
	Interpretation semantics for this word are undefined.
\compile
	( C]{ -- case-sys)
	Mark the start of the
	'CASE'\ldots'OF'\ldots'ENDOF'\ldots'ENDCASE'
	structure. Append the run-time semantics given below to the
	current definition.
\runtime
	(  -- )
	Continue execution.
*******************************************************************************/
		{ push(0); }

/******************************************************************************/
void /* "compile," */	do_compile_comma(void)
/*******************************************************************************
	COMPILE,	[compile-comma] 
\interpret
	Interpretation semantics for this word are undefined.
\execute
	( xt -- )
	Append the execution semantics of the definition represented
	by 'xt' to the execution semantics of the current
	definition.
*******************************************************************************/
		{ do_comma(); }

/******************************************************************************/
void /* "endcase" */	do_endcase(void)
/*******************************************************************************
	ENDCASE	[end-case] 
\interpret
	Interpretation semantics for this word are undefined.
\compile
	( C]{case-sys -- )
	Mark the end of the
	'CASE'\ldots'OF'\ldots'ENDOF'\ldots'ENDCASE'
	structure. Use 'case-sys' to resolve the entire structure.
	Append the run-time semantics given below to the current
	definition.
\runtime
	( x -- )
	Discard the case selector 'x' and continue execution.
*******************************************************************************/
		{ int i = pop(); postpone("drop"); while (i --) do_then(); }

/******************************************************************************/
void /* "endof" */	do_endof(void)
/*******************************************************************************
	ENDOF	[end-of] 
\interpret
	Interpretation semantics for this word are undefined.
\compile
	( C]{case-sys_1 of-sys -- case-sys_2)
	Mark the end of the 'OF'\ldots'ENDOF' part of the
	'CASE' structure. The next location for a transfer of
	control resolves the reference given by 'of-sys'. Append
	the run-time semantics given below to the current definition.
	Replace 'case-sys_1' with 'case-sys_2' on the
	control-flow stack, to be resolved by 'ENDCASE'.
\runtime
	(  -- )
	Continue execution at the location specified by the consumer
	of 'case-sys_2'.
*******************************************************************************/
		{ do_ahead(); rot(); rot(); do_then(); push(pop() + 1); }

/******************************************************************************/
void /* "erase" */	do_erase(void)
/*******************************************************************************
	ERASE	 
( addr u -- )
	If 'u' is greater than zero, clear all bits in each of
	'u' consecutive address units of memory beginning at
	'addr'.
*******************************************************************************/
		{ cell x = pop(); xmemset((void *) pop(), 0, x); }

/******************************************************************************/
void /* "false" */	do_false(void)
/*******************************************************************************
	FALSE	 
(  -- false)
	Return a 'false' flag.
*******************************************************************************/
		{ push(C_FALSE); }

/******************************************************************************/
void /* "hex" */		do_hex(void)
/*******************************************************************************
	HEX	 
(  -- )
	Set contents of 'BASE' to sixteen.
*******************************************************************************/
		{ base = 16; }

/******************************************************************************/
void /* "marker" */	do_marker(void)
/*******************************************************************************
	MARKER	 
( "<spaces>name" -- )
	Skip leading space delimiters. Parse 'name' delimited by
	a space. Create a definition for 'name' with the execution
	semantics defined below.
\execute[name]
	(  -- )
	Restore all dictionary allocation and search order pointers to
	the state they had just prior to the definition of 'name'.
	Remove the definition of 'name' and all subsequent
	definitions. Restoration of any structures still existing that
	could refer to deleted definitions or deallocated data space is
	not necessarily provided. No other contextual information such
	as numeric base is affected.
*******************************************************************************/
{
	push((cell) here.cell);
	push((cell) latest);
	do_create();
	do_comma();
	do_comma();
	latest->cfa = runtime_marker;
}

/******************************************************************************/
void /* "nip" */		do_nip(void)
/*******************************************************************************
	NIP	 
( x_1 x_2 -- x_2)
	Drop the first item below the top of stack.
*******************************************************************************/
		{ swap(); pop(); }

/******************************************************************************/
void /* "of" */		do_of(void)
/*******************************************************************************
	OF	 
\interpret
	Interpretation semantics for this word are undefined.
\compile
	( C]{ -- of-sys)
	Put 'of-sys' onto the control flow stack. Append the
	run-time semantics given below to the current definition.
	The semantics are incomplete until resolved by a consumer
	of 'of-sys' such as 'ENDOF'.
\runtime
	( x_1 x_2 -- x_1)
	If the two values on the stack are not equal, discard the
	top value and continue execution at the location specified
	by the consumer of 'of-sys', e.g., following the next
	'ENDOF'. Otherwise, discard both values and continue
	execution in line.
*******************************************************************************/
		{ postpone("over"); postpone("="); do_if(); postpone("drop"); }

/******************************************************************************/
void /* "pad" */		do_pad(void)
/*******************************************************************************
	PAD	 
(  -- c-addr)
	'c-addr' is the address of a transient region that can
	be used to hold data for intermediate processing.
*******************************************************************************/
		{ push((cell) here.cell); }

/******************************************************************************/
void /* "parse" */	do_parse(void)
/*******************************************************************************
	PARSE	 
( char "ccc<char>" -- c-addr u)
	Parse 'ccc' delimited by the delimiter 'char'.
	'c-addr' is the address (within the input buffer) and
	'u' is the length of the parsed string. If the parse area
	was empty, the resulting string has a zero length.
*******************************************************************************/
{
uint8_t c;
const uint8_t * s;
int i;
	do_source();
	pop();
	s = (const uint8_t *) pop();
	c = (uint8_t) pop();
	/* sanity check */
	if (input_spec.idx >= input_spec.len)
	{
		input_spec.idx = input_spec.len;
		push((cell) (s + input_spec.len));
		push(0);
		return;
	}
	i = 0;
	if (!xisspace(c))
	{
		for (i = input_spec.idx; i < input_spec.len; i ++)
			if (i[s] == c)
				break;
	}
	else
	{
		/* special case for whitespace characters */
		for (i = input_spec.idx; i < input_spec.len; i ++)
			if (!xisspace(i[s]))
				break;
		input_spec.idx = i;
		for (; i < input_spec.len; i ++)
			if (xisspace(i[s]))
				break;
	}
	push((cell) (s + input_spec.idx));
	push(i - input_spec.idx);
	if (i != input_spec.len)
		/* move past the delimiter character */
		i ++;
	input_spec.idx = i;
}

/******************************************************************************/
void /* "parse-name" */		do_parse_name(void)
/*******************************************************************************
	PARSE-NAME	[][X:parse-name] 
( "<spaces>name<space>" -- c-addr u)
	Skip leading space delimiters. Parse 'name' delimited by a
	space.
	'c-addr' is the address of the selected string within the
	input buffer and 'u' is its length in characters. If the
	parse area is empty or contains only white space, the resulting
	string has length zero.
	\begin{implement} % I.6.2.---- PARSE-NAME
		':' isspace? 'p' c -{}- f ) \\
		\tab 'BL' '1+' 'Uless' ';'
		':' isnotspace? 'p' c -{}- f ) \\
		\tab isspace? '0=' ';'
		':' xt-skip 'p' addr1 n1 xt -{}- addr2 n2 ) \\
		\tab 'bs' skip all characters satisfying xt ( c -{}- f ) \\
		\tab 'toR' \\
		\tab 'BEGIN' \\
		\tab[2] 'DUP' \\
		\tab 'WHILE' \\
		\tab[2] 'OVER' 'C@' 'R@' 'EXECUTE' \\
		\tab 'WHILE' \\
		\tab[2] 1 'string'{/STRING} \\
		\tab 'REPEAT' 'THEN' \\
		\tab 'Rfrom' 'DROP' ';'
		':' parse-name 'p' "name" -{}- c-addr u ) \\
		\tab 'SOURCE' 'toIN' '@' 'string'{/STRING} \\
		\tab '[']' isspace? xt-skip 'OVER' 'toR' \\
		\tab '[']' isnotspace? xt-skip 'p' end-word restlen r: start-word ) \\
		\tab '2DUP' 1 'MIN' '+' 'SOURCE' 'DROP' '-' 'toIN' '!' \\
		\tab 'DROP' 'Rfrom' 'TUCK' '-' ';'
	\end{implement}
*******************************************************************************/
	{ do_bl(); do_parse(); }

/******************************************************************************/
void /* "pick" */		do_pick(void)
/*******************************************************************************
	PICK	 
( x_u{\ldots}x_1 x_0 u -- x_u{\ldots)x_1 x_0 x_u}
	Remove 'u'. Copy the 'x_u' to the top of the stack.
	An ambiguous condition exists if there are less than 'u'+2
	items on the stack before 'PICK' is executed.
*******************************************************************************/
		{ pick(); }

/******************************************************************************/
void /* "refill" */	do_refill(void)
/*******************************************************************************
	REFILL	 
(  -- flag)
	Attempt to fill the input buffer from the input source,
	returning a true flag if successful.
	When the input source is the user input device, attempt to
	receive input into the terminal input buffer. If successful,
	make the result the input buffer, set 'toIN' to zero, and
	return \emph{true}. Receipt of a line containing no characters
	is considered successful. If there is no input available from
	the current input source, return \emph{false}.
	When the input source is a string from 'EVALUATE', return
	\emph{false} and perform no other action.
*******************************************************************************/
{
int i, c;
	if (input_spec.source_id == SOURCE_ID_STRING) { push(C_FALSE); return; }
	input_spec.idx = 0;
	i = 0;
	while (i < sizeof inbuf_area)
	{
		if (input_spec.source_id == SOURCE_ID_USER_INPUT_DEVICE)
			c = sfgetc();
		else
			c = sffgetc(input_spec.file_id);
		if (c == '\n' && input_spec.source_id == SOURCE_ID_USER_INPUT_DEVICE && env.cr_echo_enabled)
			sfputc(c);
		if (c == EOF)
		{
			input_spec.len = i;
			if (!i)
				push(C_FALSE);
			else
				push(C_TRUE);
			return;
		}
		switch (c)
		{
			case '\n':
				input_spec.len = i;
				push(C_TRUE);
				return;
			case ' ':
			case '\t':
				if (i >= REFILL_WATERMARK_LEVEL)
				{
					input_spec.len = i;
					push(C_TRUE);
					return;
				}
				/* fallout */
			default:
				input_spec.buf[i] = (uint8_t) c;
		}
		i ++;
	}
	input_spec.len = i;
	push(C_TRUE);
}

/******************************************************************************/
void /* "restore-input" */	do_restore_input(void)
/*******************************************************************************
	RESTORE-INPUT	 
( x_n {\ldots} x_1 n -- flag)
	Attempt to restore the input source specification to the state
	described by 'x_1' through 'x_n'. 'flag' is
	true if the input source specification cannot be so restored.
	An ambiguous condition exists if the input source represented
	by the arguments is not the same as the current input source.
*******************************************************************************/
{
	input_spec.source_id = pop();
	input_spec.idx = pop();
	input_spec.len = pop();
	input_spec.buf = (uint8_t *) pop();
	/*! \note	some external code (notably, word 'included' in module
	 *		sf-opt-file.c) needs setting the input specification
	 *		when switchin input sources, but in order to do that,
	 *		access to the internal 'input_spec' data structure is
	 *		needed; as generally, right now, there are essentially
	 *		only two data buffer sources - strings and the 'inbuf_area'
	 *		(which itself can be used from various sources - e.g.
	 *		the 'user input device' (whatever that is), files, etc.),
	 *		perform this simple hack: if when all of the input specification
	 *		data fields are restored, if the input source is not a string,
	 *		set the input buffer to the 'inbuf_area' */
	if (input_spec.source_id != SOURCE_ID_STRING)
		input_spec.buf = inbuf_area;
}

/******************************************************************************/
void /* "roll" */		do_roll(void)
/*******************************************************************************
	ROLL	 
( x_u x_{u-1} {\ldots} x_0 u -- x_{u-1) {\ldots} x_0 x_u}
	Remove 'u'. Rotate 'u'+1 items on the top of the stack.
	An ambiguous condition exists if there are less than 'u'+2
	items on the stack before 'ROLL' is executed.
*******************************************************************************/
		{ roll(); }

/******************************************************************************/
void /* "s\\\"" */	do_s_backslash_quote(void)
/*******************************************************************************
% -------------------------------------------------------------------
\cbstart\patch{ed12}
	S\bs"	[s-backslash-quote][X:escaped-strings] 
\item \vspace{-8pt}\hspace{170pt}\strike{10}{15} ~
\cbend
\interpret
	Interpretation semantics for this word are undefined.
\compile ( "ccc<quote>" -- ) \\
	Parse 'ccc' delimited by " (double-quote), using the
	translation rules below. Append the run-time semantics given below to
	the current definition.
\item[Translation rules]
	Characters are processed one at a time and appended to the compiled
	string. If the character is a `\bs' character it is processed by
	parsing and substituting one or more \linebreak characters as follows, where
	the character after the backslash is case sensitive:
		\a	& BEL		& (alert,		& ASCII 7) \\
		\b	& BS		& (backspace,	& ASCII 8) \\
		\e	& ESC		& (escape,		& ASCII 27) \\
		\f	& FF		& (form feed,	& ASCII 12) \\
		\l	& LF		& (line feed,	& ASCII 10) \\
		\m	& CR/LF	& pair			& (ASCII 13, 10) \\
		\n	& newline& \multicolumn{2}{l}{(implementation dependent , e.g., CR/LF, CR, LF, LF/CR)} \\
		\q	& \multicolumn{2}{l}{double-quote} & (ASCII 34) \\
		\r	& CR		& (carriage return,	& ASCII 13) \\
		\t	& HT		& (horizontal tab,	& ASCII 9) \\
		\v	& VT		& (vertical tab,		& ASCII 11) \\
		\z	& NUL		& (no character,		& ASCII 0) \\
		\"	& \multicolumn{2}{l}{double-quote} & (ASCII 34) \\
		\multicolumn{3}{l}{\texttt{\bs{}x}\arg{hexdigit}\arg{hexdigit}} \\
		&&\multicolumn{2}{p{27em}}{
			The resulting character is the conversion of these two hexadecimal
			digits. An ambiguous conditions exists if \texttt{\bs{x}} is not
			followed by two hexadecimal characters.} \\
		\texttt{\bs\bs} & \multicolumn{2}{l}{backslash itself} & (ASCII 92) \\
	\end{tabular}
	An ambiguous condition exists if a \bs{} is placed before any character,
	other than those defined in here.
\runtime (  -- c-addr u) \\
	Return 'c-addr' and 'u' describing a string consisting of
	the translation of the characters 'ccc'.  A program shall not
	alter the returned string.
*******************************************************************************/
{
/*
: next-sym ( -- c)
	begin
		source swap drop >in @
		= if ." refilling..." cr refill FALSE = abort" error reading character" then
		source swap drop >in @ <>
	until
	source drop >in @ + c@
	>in 1 +!
		;
: s\"
	postpone ahead
	here >r
	\ reserve space for the 'length' byte
	1 allot
	begin
		next-sym
		case
			[char] \ of ." backslash" cr
				next-sym case
				...
				endcase
			endof
			\ default
			." ordinary character: " dup emit dup . cr
			dup c,
		endcase
	[char] " = until
	\ store length
	here r@ - 1- r@ c!
	align
	postpone then
	r> postpone literal postpone count ; immediate
*/
uint8_t * s, c;
int i;
	if (state == STATE_COMPILING)
		do_ahead();
	s = here.chr + 1;
	while (1) switch (c = next_sym())
	{
		case '"': goto end_of_string;
		case '\\': switch (c = next_sym())
			   {
				   case 'a': c = '\a'; break;
				   case 'b': c = '\b'; break;
				   case 'e': c = 27; break;
				   case 'f': c = '\f'; break;
				   case 'l': c = 10; break;
				   case 'm': * s ++ = 13; c = 10; break;
				   case 'r': c = 13; break;
				   case '"':
				   case 'q': c = '"'; break;
				   case 't': c = 9; break;
				   case 'v': c = 11; break;
				   case 'z': c = 0; break;
				   case '\\': c = '\\'; break;
				   case 'x':
					      {
						      uint8_t x1 = xtolower(next_sym()), x2 = xtolower(next_sym());
						      /* Validate hex digits */
						      if ((('0' <= x1 && x1 <= '9') || ('a' <= x1 && x1 <= 'f'))
								      && (('0' <= x2 && x2 <= '9') || ('a' <= x2 && x2 <= 'f')))
						      {
							      if (x1 <= '9')
								      x1 -= '0';
							      else
								      x1 -= 'a', x1 += 10;
							      if (x2 <= '9')
								      x2 -= '0';
							      else
								      x2 -= 'a', x2 += 10;
							      c = (x1 << 4) + x2;
						      }
						      else
						      {
							      print_str("error: bad hexadecimal digit in string escape character conversion, ignoring hex escape sequence\n");
							      c = 0;
						      }
					      }
					      break;
				   default: print_str("warning: unknown/invalid escape sequence in escaped string requested, ignoring\n");
			   }
			   /* fall out */
		default:
			   * s ++ = c;
			   continue;
	}
end_of_string:
	if ((i = s - here.chr - 1) > 255)
		strabort(__func__, "escaped string too large\n");
	* here.chr = i;
	if (state == STATE_COMPILING)
	{
		push((cell) here.chr);
		here.chr = s;
		do_align();
		do_swap(); do_then();
		do_literal(); postpone("count");
	}
	else
		push((cell) here.chr), do_count();
}

/******************************************************************************/
void /* "save-input" */	do_save_input(void)
/*******************************************************************************
% -------------------------------------------------------------------
	SAVE-INPUT	 
(  -- x_n {\ldots) x_1 n}
	'x_1' through 'x_n' describe the current state of the
	input source specification for later use by 'RESTORE-INPUT'.
*******************************************************************************/
{
	push((cell) input_spec.buf);
	push((cell) input_spec.len);
	push((cell) input_spec.idx);
	push((cell) input_spec.source_id);
}

/******************************************************************************/
void /* "source-id" */	do_source_id(void)
/*******************************************************************************
	SOURCE-ID	[source-i-d] 
(  -- 0 | -1 )
	Identifies the input source as follows:
	\begin{center}
		\begin{tabular}{cl}
		\hline\hline
		'SOURCE-ID' & Input source \\
		\hline
		-1	& String (via 'EVALUATE') \\
		 0	& User input device \\
		\hline\hline
		\end{tabular}
	\end{center}
*******************************************************************************/
		{ push(input_spec.source_id); }

/******************************************************************************/
void /* "to" */		do_to(void)
/*******************************************************************************
	TO	 
\interpret
	( i*x "<spaces>name" -- )
	Skip leading spaces and parse 'name' delimited by a space.
	Perform the ``TO 'name' run-time'' semantics given in the
	definition for the defining word of 'name'.
	An ambiguous condition exists if 'name' was not defined
	by a word with ``TO 'name' run-time'' semantics.
\compile
	( "<spaces>name" -- )
	Skip leading spaces and parse 'name' delimited by a
	space. Append the ``TO 'name' run-time'' semantics given
	in the definition for the defining word	of 'name' to the
	current definition.
	An ambiguous condition exists if 'name' was not defined
	by a word with ``TO 'name' run-time'' semantics.
\runtime
	(  -- )
\note
	An ambiguous condition exists if any of	'POSTPONE',
	'[COMPILE]', ''' or '[']' are applied to
	'TO'.
*******************************************************************************/
{
/* : xto bl word find drop >body state @ 0 = if ( interpreting) ! else ( compiling) postpone literal postpone ! then ; immediate */
	do_bl(); do_word(); do_find(); if (!pop()) { do_count(); do_type(); sabort(": word not found"); }
	do_to_body();
	if (state == STATE_INTERPRETING)
	{
		cell * x = (cell *) pop();
		* x = pop();
	}
	else
		/* compiling */
		do_literal(), postpone("!");
}

/******************************************************************************/
void /* "true" */		do_true(void)
/*******************************************************************************
	TRUE	 
(  -- true)
	Return a 'true' flag, a single-cell value with all
	bits set.
*******************************************************************************/
		{ push(C_TRUE); }

/******************************************************************************/
void /* "tuck" */		do_tuck(void)
/*******************************************************************************
	TUCK	 
( x_1 x_2 -- x_2 x_1 x_2)
	Copy the first (top) stack item below the second stack item.
*******************************************************************************/
		{ ddup(); rot(); rot(); }

/******************************************************************************/
void /* "u.r" */		do_u_dot_r(void)
/*******************************************************************************
	U.R	[u-dot-r] 
( u n -- )
	Display 'u' right aligned in a field 'n' characters
	wide. If the number of characters required to display 'u'
	is greater than 'n', all digits are displayed with no leading
	spaces in a field as wide as necessary.
*******************************************************************************/
		{ scell x = pop();
		push(0); do_less_number_sign(); do_number_sign_s(); do_number_sign_greater();
		x -= top(); if (x > 0) push(x), do_spaces();
		do_type(); do_space(); }

/******************************************************************************/
void /* "u>" */		do_u_greater_than(void)
/*******************************************************************************
	U>	[u-greater-than] 
( u_1 u_2 -- flag)
	'flag' is true if and only if 'u_1' is greater than
	'u_2'.
*******************************************************************************/
		{ dcell x = popd(); push(((cell) x > (cell)(x >> CELL_NR_BITS)) ? C_TRUE : C_FALSE); }

/******************************************************************************/
void /* "unused" */	do_unused(void)
/*******************************************************************************
	UNUSED	 
(  -- u)
	'u' is the amount of space remaining in the region addressed
	by 'HERE', in address units.
*******************************************************************************/
		{ push(SFORTH_CORE_SIZE_IN_CELLS); do_cells(); push((cell) core); push((cell) here.cell); do_minus(); do_plus(); }

/******************************************************************************/
void /* "value" */	do_value(void)
/*******************************************************************************
	VALUE	 
( x "<spaces>name" -- )
	Skip leading space delimiters. Parse 'name' delimited by
	a space. Create a definition for 'name' with the execution
	semantics defined below, with an initial value equal to 'x'.
	'name' is referred to as a ``value''.
\execute[name]
	(  -- x)
	Place 'x' on the stack. The value of 'x' is that
	given when 'name' was created, until the phrase 'x'
	'TO' 'name' is executed, causing a new value of
	'x' to be assigned to 'name'.
\runtime['TO' 'name']
	( x -- )
	Assign the value 'x' to 'name'.
*******************************************************************************/
		{ do_create(); latest->cfa = runtime_value; * here.cell ++ = pop(); }

/******************************************************************************/
void /* "within" */ 	do_within(void)
/*******************************************************************************
	WITHIN	 
( n_1|u_1 n_2|u_2 n_3|u_3 -- flag)
	Perform a comparison of a test value 'n_1|u_1' with a
	lower limit 'n_2|u_2' and an upper limit
	'n_3|u_3', returning \emph{true} if either
	('n_2|u_2' <  'n_3|u_3' and
	('n_2|u_2' <= 'n_1|u_1' and
	 'n_1|u_1' <  'n_3|u_3')) or
	('n_2|u_2' >  'n_3|u_3' and
	('n_2|u_2' <= 'n_1|u_1' or
	 'n_1|u_1' <  'n_3|u_3')) is true, returning
	\emph{false} otherwise. An ambiguous condition exists
	'n_1|u_1', 'n_2|u_2', and 'n_3|u_3' are not
	all the same type.
*******************************************************************************/
{
cell u1, u2, u3;
	u3 = pop(), u2 = pop(), u1 = pop();
	if ((u2 < u3 && u2 <= u1 && u1 < u3)
		|| (u2 > u3 && (u2 <= u1 || u1 < u3)))
		push(C_TRUE);
	else
		push(C_FALSE);
}

/******************************************************************************/
/* void  "[compile]" 	do_bracket_compile(void) */
/*******************************************************************************
	[COMPILE]	[bracket-compile] 
\interpret
	Interpretation semantics for this word are undefined.
\compile
	( "<spaces>name" -- )
	Skip leading space delimiters. Parse 'name' delimited by
	a space. Find 'name'. If 'name' has other than default
	compilation semantics, append them to the current definition;
	otherwise append the execution semantics of 'name'. An
	ambiguous condition exists if 'name' is not found.
*******************************************************************************/
// THIS WORD IS NOT IMPLEMENTED; IF YOU NEED IT, YOU MUST DEFINE IT YOURSELF
/*
		{ xxx; }
*/

/******************************************************************************/
void /* "\\" */		do_backslash(void)
/*******************************************************************************
	\bs	[backslash] 
\compile
	Perform the execution semantics given below.
\execute
	( "ccc<eol>" -- )
	Parse and discard the remainder of the parse area.
	'bs' is an immediate word.
*******************************************************************************/
{
	if (input_spec.source_id == SOURCE_ID_USER_INPUT_DEVICE)
		input_spec.idx = input_spec.len;
	else
	{
		int i;
		for (i = input_spec.idx; i < input_spec.len; i ++)
			if (input_spec.str[i] == '\n')
			{
				i ++;
				break;
			}
			input_spec.idx = i;
	}
}

/*******************************************************************************
 *
 *
 * nonstandard, internal-use, helper etc. words
 *
 *
 ******************************************************************************/
/* internal use helper words */
void /* "ahead" */	do_ahead(void)
		{ * here.word ++ = & xt_runtime_branch; push((cell) here.cell ++); }

void /* "break" */	do_break(void)
		{ }

void /* "bye" */	do_bye(void)
		{ longjmp(jmpenv, SF_EXC_CODE_BYE); }

void /* "latest" */	do_latest(void)
		{ push((cell) &latest); }

void /* "jmpenv" */	do_jmpenv(void)
		{ push((cell) &jmpenv); }

void /* "included" */	do_included(void)
{
/* note:	when 'include'-ing files, it is common for one file to include another;
 *		in such a case, a maximum nesting level of inclusion is supported -
 *		nested file inclusion uses the 'istack' array below (this array
 *		is with static storage duration in order to make stack overflow
 *		less likely); in case of an exception (a longjmp taken), the
 *		files opened must be closed so that no resources are leaked;
 *		read the comments on the exception handling model adopted
 *		by the sforth engine for more details and clarificaiton
 *		(these comments are in this file, near the 'jmpenv'
 *		variable definition) */
static jmp_buf ijmpenv;
static int isp;
static struct
{
	cell fd;
	struct input_spec saved_inspec;
	uint8_t	fname[MAX_FNAME_LEN];
}
istack[MAX_FILE_INCLUSION_DEPTH];
unsigned int fname_len = pop();
const uint8_t * s = (const uint8_t *) pop();
int exc_code;
#ifdef PETIT_FS
int filepos;
	filepos = pf_getpos();
#endif
	if (isp == MAX_FILE_INCLUSION_DEPTH)
	{
		print_str("inclusion level too deep, aborting\n");
		do_abort();
	}
	if (fname_len > sizeof istack[0].fname - 1)
		fname_len = sizeof istack[0].fname - 1;
	xmemcpy(istack[isp].fname, s, fname_len);
	istack[isp].fname[fname_len] = 0;
	/*! \todo	define file open mode flags */
	if ((istack[isp].fd = sfopen((char *) istack[isp].fname, 0)) == EOF)
	{
		print_str("could not open included file\n");
		do_abort();
	}
	isp ++;
	if (isp == 1)
	{
		/* first entry in this function (not a nested invocation) -
		 * set-up the local jmpenv exception handler */
		xmemcpy(ijmpenv, jmpenv, sizeof jmpenv);
		if ((exc_code = setjmp(jmpenv)))
		{
			print_str("exception taken\n");
			while (isp --)
			{
				print_str("closing one file\n");
				sfclose(istack[isp].fd);
			}
			isp = 0;
			xmemcpy(jmpenv, ijmpenv, sizeof jmpenv);
			/* rethrow exception */
			longjmp(jmpenv, exc_code);
		}
	}
	istack[isp - 1].saved_inspec = input_spec;
	input_spec.idx = input_spec.len = 0;
	input_spec.file_id = istack[isp - 1].fd;
	input_spec.buf = inbuf_area;
	while (1)
	{
		if (input_spec.idx == input_spec.len)
		{
			do_refill();
			if (!pop())
				/* end of input */
				break;
		}
		interpret();
	}
	sfclose(istack[-- isp].fd);
	input_spec = istack[isp].saved_inspec;
	if (isp)
#ifdef PETIT_FS
	{
		sfopen((char *) istack[isp - 1].fname, 0);
		pf_lseek(filepos);
	}
#else
	;
#endif
	else
		/* leaving the chain of nested 'included' calls,
		 * restore the longjmp exception handling environment */
		xmemcpy(jmpenv, ijmpenv, sizeof jmpenv);
}

void /* ".s" */		do_dot_s(void)
{
int i;
	if (!sp)
	{
		print_str("stack empty"); do_cr();
	}
	else
	{
		for (i = 0; i < sp; i ++)
			push(dstack[i]), do_dot();
		do_cr();
	}
}
void /* ">name" */		do_to_name(void)
{
	push((cell)((struct word *)pop())->name);
}

void /* "?" */		do_question(void)
		{ do_dup(), do_fetch(), do_dot(); }

void /* "dump" */		do_dump(void)
{
cell cnt = pop();
cell addr = pop();
int i;
int saved_base = base;
	/* dump header */
	base = 16;
	if (cnt && (addr & 15))
	{
		push(addr & ~ 15);
		do_dot(), print_str(":\t");
		for (i = 0; i < (addr & 15); i ++)
			print_str(".. ");
		for (; i < 16 && cnt; i ++, cnt --)
		{
			push(addr); do_c_fetch(); if (top() < 16) print_str("0");
			do_dot(); addr ++;
		}
		do_cr();
	}
	while (cnt)
	{
		push(addr & ~ 15);
		do_dot(), print_str(":\t");
		for (i = 0; i < 16 && cnt; i ++, cnt --)
		{
			push(addr); do_c_fetch(); if (top() < 16) print_str("0");
			do_dot(); addr ++;
		}
		do_cr();
	}
	base = saved_base;
}

/* environment settings words */
void /* "cr-echo" */	do_cr_echo(void)
		{ push((cell) & env.cr_echo_enabled); }

void /* "colon-debug" */	do_colon_debug(void)
		{ push((cell) & env.colon_debug_enabled); }

void /* "smudge" */	do_smudge(void)
{
		latest->is_smudged = 1;
}
void /* "unsmudge" */	do_unsmudge(void)
		{
		latest->is_smudged = 0;
		}
/* reseting of the sforth engine is always available */
void /* "sf-reset" */	sf_reset(void)
{
	if (!latest_reset_value)
		latest_reset_value = latest;
	else
		latest = latest_reset_value;
	input_spec.source_id = SOURCE_ID_USER_INPUT_DEVICE;
	input_spec.buf = inbuf_area;
	input_spec.idx = input_spec.len = 0;
	state = STATE_INTERPRETING;
	base = 10;
	sp = rsp = 0;
	here.cell = core;
}

