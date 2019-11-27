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

#include "enabled-words.h"

#if DO_STORE_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"!",		do_store)
/*******************************************************************************
	!	[store]
( x a-addr -- )
	Store 'x' at 'a-addr'.
*******************************************************************************/
#endif
/******************************************************************************/
#if DO_NUMBER_SIGN_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
MKWORD(dictionary,	__COUNTER__,	"#",		do_number_sign)
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
#endif

/******************************************************************************/
#if DO_NUMBER_SIGN_GREATER_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"#>",		do_number_sign_greater)
/*******************************************************************************
	#>	[number-sign-greater] 
( xd -- c-addr u)
	Drop 'xd'. Make the pictured numeric output string
	available as a character string. 'c-addr' and 'u'
	specify the resulting character string. A program may replace
	characters within the string.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_NUMBER_SIGN_S_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"#s",		do_number_sign_s)
/*******************************************************************************
	#S	[number-sign-s] 
( ud_1 -- ud_2)
	Convert one digit of 'ud_1' according to the rule for
	'num'. Continue conversion until the quotient is zero.
	'ud_2' is zero. An ambiguous condition exists if
	'numS' executes outside of a 'num-start' 'num-end'
	delimited number conversion.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_TICK_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"'",		do_tick)
/*******************************************************************************
	'	[tick] 
( "<spaces>name" -- xt)
	Skip leading space delimiters. Parse 'name' delimited by
	a space. Find 'name' and return 'xt', the execution
	token for 'name'. An ambiguous condition exists if
	'name' is not found. When interpreting,
	" ' xyz EXECUTE" is equivalent to " xyz".
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_PAREN_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKIMMWORD(dictionary,	__COUNTER__,	"(",		do_paren)
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
#endif

/******************************************************************************/
#if DO_STAR_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"*",		do_star)
/*******************************************************************************
	*	[star] 
( n_1|u_1 n_2|u_2 -- n_3|u_3)
	Multiply 'n_1|u_1' by 'n_2|u_2' giving the product
	'n_3|u_3'.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_STAR_SLASH_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"*/",		do_star_slash)
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
#endif

/******************************************************************************/
#if DO_STAR_SLASH_MOD_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"*/mod",	do_star_slash_mod)
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
#endif

/******************************************************************************/
#if DO_PLUS_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"+",		do_plus)
/*******************************************************************************
	+	[plus] 
( n_1|u_1 n_2|u_2 -- n_3|u_3)
	Add 'n_2|u_2' to 'n_1|u_1', giving the sum
	'n_3|u_3'.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_PLUS_STORE_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"+!",		do_plus_store)
/*******************************************************************************
	+!	[plus-store] 
( n|u a-addr -- )
	Add 'n|u' to the single-cell number at 'a-addr'.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_PLUS_LOOP_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKIMMWORD(dictionary,	__COUNTER__,	"+loop",	do_plus_loop)
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
#endif

/******************************************************************************/
#if DO_COMMA_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	",",		do_comma)
/*******************************************************************************
	,	[comma] 
( x -- )
	Reserve one cell of data space and store 'x' in the cell.
	If the data-space pointer is aligned when ',' begins
	execution, it will remain aligned when ',' finishes
	execution. An ambiguous condition exists if the data-space
	pointer is not aligned prior to execution of ','.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_MINUS_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"-",		do_minus)
/*******************************************************************************
	-	[minus] 
( n_1|u_1 n_2|u_2 -- n_3|u_3)
	Subtract 'n_2|u_2' from 'n_1|u_1', giving the
	difference 'n_3|u_3'.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_DOT_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	".",		do_dot)
/*******************************************************************************
	.	[dot] 
( n -- )
	Display 'n' in free field format.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_DOT_QUOTE_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKIMMWORD(dictionary,	__COUNTER__,	".\"",		do_dot_quote)
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
#endif

/******************************************************************************/
#if DO_SLASH_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"/",		do_slash)
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
#endif

/******************************************************************************/
#if DO_SLASH_MOD_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"/mod",		do_slash_mod)
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
#endif

/******************************************************************************/
#if DO_ZERO_LESS_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"0<",		do_zero_less)
/*******************************************************************************
	0<	[zero-less] 
( n -- flag)
	'flag' is true if and only if 'n' is less than zero.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_ZERO_EQUALS_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"0=",		do_zero_equals)
/*******************************************************************************
	0=	[zero-equals] 
( x -- flag)
	'flag' is true if and only if 'x' is equal to zero.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_ONE_PLUS_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"1+",		do_one_plus)
/*******************************************************************************
	1+	[one-plus] 
( n_1|u_1 -- n_2|u_2)
	Add one (1) to 'n_1|u_1' giving the sum
	'n_2|u_2'.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_ONE_MINUS_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"1-",		do_one_minus)
/*******************************************************************************
	1-	[one-minus] 
( n_1|u_1 -- n_2|u_2)
	Subtract one (1) from 'n_1|u_1' giving the difference
	'n_2|u_2'.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_TWO_STORE_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"2!",		do_two_store)
/*******************************************************************************
	2!	[two-store] 
( x_1 x_2 a-addr -- )
	Store the cell pair 'x_1 x_2' at 'a-addr', with
	'x_2' at 'a-addr' and 'x_1' at the next
	consecutive cell. It is equivalent to the sequence
	'SWAP' 'OVER' '!' 'CELL+' '!'.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_TWO_STAR_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"2*",		do_two_star)
/*******************************************************************************
	2*	[two-star] 
( x_1 -- x_2)
	'x_2' is the result of shifting 'x_1' one bit toward
	the most-significant bit, filling the vacated least-significant
	bit with zero.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_TWO_SLASH_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"2/",		do_two_slash)
/*******************************************************************************
	2/	[two-slash] 
( x_1 -- x_2)
	'x_2' is the result of shifting 'x_1' one bit toward
	the least-significant bit, leaving the most-significant bit
	unchanged.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_TWO_FETCH_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"2@",		do_two_fetch)
/*******************************************************************************
	2@	[two-fetch] 
( a-addr -- x_1 x_2)
	Fetch the cell pair 'x_1 x_2' stored at 'a-addr'.
	'x_2' is stored at 'a-addr' and 'x_1' at the
	next consecutive cell. It is equivalent to the sequence
	'DUP' 'CELL+' '@' 'SWAP' '@'.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_TWO_DROP_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"2drop",	do_two_drop)
/*******************************************************************************
	2DROP	[two-drop] 
( x_1 x_2 -- )
	Drop cell pair 'x_1 x_2' from the stack.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_TWO_DUP_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"2dup",		do_two_dup)
/*******************************************************************************
	2DUP	[two-dupe] 
( x_1 x_2 -- x_1 x_2 x_1 x_2)
	Duplicate cell pair 'x_1 x_2'.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_TWO_OVER_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"2over",	do_two_over)
/*******************************************************************************
	2OVER	[two-over] 
( x_1 x_2 x_3 x_4 -- x_1 x_2 x_3 x_4 x_1 x_2)
	Copy cell pair 'x_1 x_2' to the top of the stack.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_TWO_SWAP_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"2swap",	do_two_swap)
/*******************************************************************************
	2SWAP	[two-swap] 
( x_1 x_2 x_3 x_4 -- x_3 x_4 x_1 x_2)
	Exchange the top two cell pairs.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_COLON_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	":",		do_colon)
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
#endif

/******************************************************************************/
#if DO_SEMICOLON_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKIMMWORD(dictionary,	__COUNTER__,	";",		do_semicolon)
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
#endif

/******************************************************************************/
#if DO_LESS_THAN_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"<",		do_less_than)
/*******************************************************************************
	<	[less-than] 
( n_1 n_2 -- flag)
	'flag' is true if and only if 'n_1' is less than
	'n_2'.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_LESS_NUMBER_SIGN_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"<#",	        do_less_number_sign)
/*******************************************************************************
	<#	[less-number-sign] 
(  -- )
	Initialize the pictured numeric output conversion process.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_EQUALS_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"=",	        do_equals)
/*******************************************************************************
	=	[equals] 
( x_1 x_2 -- flag)
	'flag' is true if and only if 'x_1' is bit-for-bit
	the same as 'x_2'.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_GREATER_THAN_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	">",	        do_greater_than)
/*******************************************************************************
	>	[greater-than] 
( n_1 n_2 -- flag)
	'flag' is true if and only if 'n_1' is greater than 'n_2'.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_TO_BODY_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	">body",	do_to_body)
/*******************************************************************************
	>BODY	[to-body] 
( xt -- a-addr)
	'a-addr' is the data-field address corresponding to
	'xt'. An ambiguous condition exists if 'xt' is not
	for a word defined via 'CREATE'.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_TO_IN_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	">in",	        do_to_in)
/*******************************************************************************
	>IN	[to-in] 
(  -- a-addr)
	'a-addr' is the address of a cell containing the offset in
	characters from the start of the input buffer to the start of
	the parse area.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_TO_NUMBER_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	">number",      do_to_number)
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
#endif

/******************************************************************************/
#if DO_TO_R_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	">r",	        do_to_r)
/*******************************************************************************
	>R	[to-r] 
\interpret
	Interpretation semantics for this word are undefined.
\execute
	( x -- )
	( R]{ -- x)
	Move 'x' to the return stack.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_QUESTION_DUP_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"?dup",	        do_question_dup)
/*******************************************************************************
	?DUP	[question-dupe] 
( x -- 0 | x x)
	Duplicate 'x' if it is non-zero.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_FETCH_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"@",	        do_fetch)
/*******************************************************************************
	@	[fetch] 
( a-addr -- x)
	'x' is the value stored at 'a-addr'.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_ABORT_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"abort",	do_abort)
/*******************************************************************************
	ABORT	 
( i*x -- )
	( R]{j*x -- )
	Empty the data stack and perform the function of 'QUIT',
	which includes emptying the return stack, without displaying
	a message.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_ABORT_QUOTE_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKIMMWORD(dictionary,	__COUNTER__,	"abort\"",	do_abort_quote)
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
#endif

/******************************************************************************/
#if DO_ABS_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"abs",		do_abs)
/*******************************************************************************
	ABS	[abs] 
( n -- u)
	'u' is the absolute value of 'n'.
*******************************************************************************/
#endif

/******************************************************************************/
/************/ //MKWORD(dictionary,	__COUNTER__,	"accept",	do_accept, )

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
// THIS WORD IS NOT IMPLEMENTED; IF YOU NEED IT, YOU MUST DEFINE IT YOURSELF
/******************************************************************************/
#if DO_ALIGN_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"align",	do_align)
/*******************************************************************************
	ALIGN	 
(  -- )
	If the data-space pointer is not aligned, reserve enough space
	to align it.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_ALIGNED_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"aligned",	do_aligned)
/*******************************************************************************
	ALIGNED	 
( addr -- a-addr)
	'a-addr' is the first aligned address greater than or equal
	to 'addr'.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_ALLOT_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"allot",	do_allot)
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
#endif

/******************************************************************************/
#if DO_AND_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"and",		do_and)
/*******************************************************************************
	AND	 
( x_1 x_2 -- x_3)
	'x_3' is the bit-by-bit logical ``and'' of 'x_1'
	with 'x_2'.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_BASE_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"base",		do_base)
/*******************************************************************************
	BASE	 
(  -- a-addr)
	'a-addr' is the address of a cell containing the current
	number-conversion radix \{\{2...36\}\}.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_BEGIN_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKIMMWORD(dictionary,	__COUNTER__,	"begin",	do_begin)
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
#endif

/******************************************************************************/
#if DO_BL_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"bl",		do_bl)
/*******************************************************************************
	BL	[b-l] 
(  -- char)
	'char' is the character value for a space.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_C_STORE_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"c!",		do_c_store)
/*******************************************************************************
	C!	[c-store] 
( char c-addr -- )
	Store 'char' at 'c-addr'. When character size is smaller
	than cell size, only the number of low-order bits corresponding to
	character size are transferred.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_C_COMMA_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"c,",		do_c_comma)
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
#endif

/******************************************************************************/
#if DO_C_FETCH_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"c@",		do_c_fetch)
/*******************************************************************************
	C@	[c-fetch] 
( c-addr -- char)
	Fetch the character stored at 'c-addr'. When the cell size is
	greater than character size, the unused high-order bits are all
	zeroes.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_CELL_PLUS_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"cell+",	do_cell_plus)
/*******************************************************************************
	CELL+	[cell-plus] 
( a-addr_1 -- a-addr_2)
	Add the size in address units of a cell to 'a-addr_1', giving
	'a-addr_2'.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_CELLS_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"cells",	do_cells)
/*******************************************************************************
	CELLS	 
( n_1 -- n_2)
	'n_2' is the size in address units of 'n_1' cells.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_CHAR_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"char",		do_char)
/*******************************************************************************
	CHAR	[char] 
( "<spaces>name" -- char)
	Skip leading space delimiters. Parse 'name' delimited by
	a space. Put the value of its first character onto the stack.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_CHAR_PLUS_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"char+",	do_char_plus)
/*******************************************************************************
	CHAR+	[char-plus] 
( c-addr_1 -- c-addr_2)
	Add the size in address units of a character to
	'c-addr_1', giving 'c-addr_2'.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_CHARS_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"chars",	do_chars)
/*******************************************************************************
	CHARS	[chars] 
( n_1 -- n_2)
	'n_2' is the size in address units of 'n_1'
	characters.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_CONSTANT_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"constant",	do_constant)
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
#endif

/******************************************************************************/
#if DO_COUNT_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"count",	do_count)
/*******************************************************************************
	COUNT	 
( c-addr_1 -- c-addr_2 u)
	Return the character string specification for the counted
	string stored at 'c-addr_1'. 'c-addr_2' is the
	address of the first character after 'c-addr_1'. 'u'
	is the contents of the character at 'c-addr_1', which is
	the length in characters of the string at 'c-addr_2'.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_CR_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"cr",		do_cr)
/*******************************************************************************
	CR	[c-r] 
(  -- )
	Cause subsequent output to appear at the beginning of the next
	line.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_CREATE_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"create",	do_create)
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
#endif

/******************************************************************************/
#if DO_DECIMAL_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"decimal",	do_decimal)
/*******************************************************************************
	DECIMAL	 
(  -- )
	Set the numeric conversion radix to ten (decimal).
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_DEPTH_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"depth",	do_depth)
/*******************************************************************************
	DEPTH	 
(  -- +n)
	'+n' is the number of single-cell values contained in the
	data stack before '+n' was placed on the stack.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_DO_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKIMMWORD(dictionary,	__COUNTER__,	"do",		do_do)
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
#endif

/******************************************************************************/
#if DO_DOES_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKIMMWORD(dictionary,	__COUNTER__,	"does>",	do_does)
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
#endif

/******************************************************************************/
#if DO_DROP_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"drop",	        do_drop)
/*******************************************************************************
	DROP	 
( x -- )
	Remove 'x' from the stack.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_DUP_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"dup",	        do_dup)
/*******************************************************************************
	DUP	[dupe] 
( x -- x x)
	Duplicate 'x'.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_ELSE_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKIMMWORD(dictionary,	__COUNTER__,	"else",	        do_else)
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
#endif

/******************************************************************************/
#if DO_EMIT_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"emit",	        do_emit)
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
#endif

/******************************************************************************/
/************/ //MKWORD(dictionary,	__COUNTER__,	"environment?", do_environment)
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

*/
/******************************************************************************/
#if DO_EVALUATE_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"evaluate",     do_evaluate)
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
#endif

/******************************************************************************/
#if DO_EXECUTE_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"execute",	do_execute)
/*******************************************************************************
	EXECUTE	 
( i*x xt -- j*x)
	Remove 'xt' from the stack and perform the semantics
	identified by it. Other stack effects are due to the word
	'EXECUTE'd.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_EXIT_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKIMMWORD(dictionary,	__COUNTER__,	"exit",	        do_exit)
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
#endif

/******************************************************************************/
#if DO_FILL_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"fill",	        do_fill)
/*******************************************************************************
	FILL	 
( c-addr u char -- )
	If 'u' is greater than zero, store 'char' in each of
	'u' consecutive characters of memory beginning at
	'c-addr'.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_FIND_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"find",	        do_find)
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
#endif

/******************************************************************************/
#if DO_F_M_SLASH_MOD_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"fm/mod",	do_f_m_slash_mod)
/*******************************************************************************
	FM/MOD	[f-m-slash-mod] 
( d_1 n_1 -- n_2 n_3)
	Divide 'd_1' by 'n_1', giving the floored quotient
	'n_3' and the remainder 'n_2'. Input and output stack
	arguments are signed. An ambiguous condition exists if
	'n_1' is zero or if the quotient lies outside the range of
	a single-cell signed integer.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_HERE_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"here",		do_here)
/*******************************************************************************
	HERE	 
(  -- addr)
	'addr' is the data-space pointer.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_HOLD_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"hold",		do_hold)
/*******************************************************************************
	HOLD	 
( char -- )
	Add 'char' to the beginning of the pictured numeric output
	string. An ambiguous condition exists if 'HOLD' executes
	outside of a 'num-start' 'num-end' delimited number
	conversion.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_I_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"i",		do_i)
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
#endif

/******************************************************************************/
#if DO_IF_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKIMMWORD(dictionary,	__COUNTER__,	"if",		do_if)
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
#endif

/******************************************************************************/
#if DO_IMMEDIATE_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"immediate",	do_immediate)
/*******************************************************************************
	IMMEDIATE	 
(  -- )
	Make the most recent definition an immediate word. An ambiguous
	condition exists if the most recent definition does not have a
	name or if it was defined as a 'tools'{SYNONYM}.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_INVERT_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"invert",	do_invert)
/*******************************************************************************
	INVERT	 
( x_1 -- x_2)
	Invert all bits of 'x_1', giving its logical inverse
	'x_2'.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_J_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"j",		do_j)
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
#endif

/******************************************************************************/
/************/ //MKWORD(dictionary,	__COUNTER__,	"key",		do_key, )

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
#if DO_LEAVE_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKIMMWORD(dictionary,	__COUNTER__,	"leave",	do_leave)
#endif

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
#if DO_LITERAL_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKIMMWORD(dictionary,	__COUNTER__,	"literal",	do_literal)
#endif

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
#if DO_LOOP_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKIMMWORD(dictionary,	__COUNTER__,	"loop",		do_loop)
#endif

/******************************************************************************/
#if DO_LSHIFT_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"lshift",	do_lshift)
/*******************************************************************************
	LSHIFT	[l-shift] 
( x_1 u -- x_2)
	Perform a logical left shift of 'u' bit-places on
	'x_1', giving 'x_2'. Put zeroes into the least
	significant bits vacated by the shift. An ambiguous condition
	exists if 'u' is greater than or equal to the number of
	bits in a cell.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_M_STAR_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"m*",		do_m_star)
/*******************************************************************************
	M*	[m-star] 
( n_1 n_2 -- d)
	'd' is the signed product of 'n_1' times 'n_2'.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_MAX_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"max",		do_max)
/*******************************************************************************
	MAX	 
( n_1 n_2 -- n_3)
	'n_3' is the greater of 'n_1' and 'n_2'.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_MIN_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"min",		do_min)
/*******************************************************************************
	MIN	 
( n_1 n_2 -- n_3)
	'n_3' is the lesser of 'n_1' and 'n_2'.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_MOD_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"mod",		do_mod)
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
#endif

/******************************************************************************/
#if DO_MOVE_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"move",		do_move)
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
#endif

/******************************************************************************/
#if DO_NEGATE_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"negate",	do_negate)
/*******************************************************************************
	NEGATE	 
( n_1 -- n_2)
	Negate 'n_1', giving its arithmetic inverse 'n_2'.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_OR_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"or",		do_or)
/*******************************************************************************
	OR	 
( x_1 x_2 -- x_3)
	'x_3' is the bit-by-bit inclusive-or of 'x_1' with
	'x_2'.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_OVER_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"over",		do_over)
/*******************************************************************************
	OVER	 
( x_1 x_2 -- x_1 x_2 x_1)
	Place a copy of 'x_1' on top of the stack.
*******************************************************************************/
#endif

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
#if DO_POSTPONE_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKIMMWORD(dictionary,	__COUNTER__,	"postpone",	do_postpone)
#endif

/******************************************************************************/
#if DO_QUIT_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"quit",		do_quit)
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
#endif

/******************************************************************************/
#if DO_R_FROM_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"r>",		do_r_from)
/*******************************************************************************
	R>	[r-from] 
\interpret
	Interpretation semantics for this word are undefined.
\execute
	(  -- x)
	( R]{x -- )
	Move 'x' from the return stack to the data stack.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_R_FETCH_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"r@",	        do_r_fetch)
/*******************************************************************************
	R@	[r-fetch] 
\interpret
	Interpretation semantics for this word are undefined.
\execute
	(  -- x)
	( R]{x -- x)
	Copy 'x' from the return stack to the data stack.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_RECURSE_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKIMMWORD(dictionary,	__COUNTER__,	"recurse",      do_recurse)
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
#endif

/******************************************************************************/
#if DO_REPEAT_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKIMMWORD(dictionary,	__COUNTER__,	"repeat",	do_repeat)
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
#endif

/******************************************************************************/
#if DO_ROT_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"rot",	        do_rot)
/*******************************************************************************
	ROT	[rote] 
( x_1 x_2 x_3 -- x_2 x_3 x_1)
	Rotate the top three stack entries.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_RSHIFT_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"rshift",	do_rshift)
/*******************************************************************************
	RSHIFT	[r-shift] 
( x_1 u -- x_2)
	Perform a logical right shift of 'u' bit-places on
	'x_1', giving 'x_2'. Put zeroes into the most
	significant bits vacated by the shift. An ambiguous condition
	exists if 'u' is greater than or equal to the number of
	bits in a cell.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_S_QUOTE_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKIMMWORD(dictionary,	__COUNTER__,	"s\"",	        do_s_quote)
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
#endif

/******************************************************************************/
#if DO_S_TO_D_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"s>d",	        do_s_to_d)
/*******************************************************************************
	S>D	[s-to-d] 
( n -- d)
	Convert the number 'n' to the double-cell number 'd'
	with the same numerical value.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_SIGN_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"sign",	        do_sign)
/*******************************************************************************
	SIGN	 
( n -- )
	If 'n' is negative, add a minus sign to the beginning of
	the pictured numeric output string. An ambiguous condition exists
	if 'SIGN' executes outside of a 'num-start' 'num-end'
	delimited number conversion.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_S_M_SLASH_REM_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"sm/rem",	do_s_m_slash_rem)
/*******************************************************************************
	SM/REM	[s-m-slash-rem] 
( d_1 n_1 -- n_2 n_3)
	Divide 'd_1' by 'n_1', giving the symmetric quotient
	'n_3' and the remainder 'n_2'. Input and output stack
	arguments are signed. An ambiguous condition exists if 'n_1'
	is zero or if the quotient lies outside the range of a single-cell
	signed integer.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_SOURCE_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"source",	do_source)
/*******************************************************************************
	SOURCE	 
(  -- c-addr u)
	'c-addr' is the address of, and 'u' is the number of
	characters in, the input buffer.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_SPACE_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"space",	do_space)
/*******************************************************************************
	SPACE	 
(  -- )
	Display one space.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_SPACES_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"spaces",	do_spaces)
/*******************************************************************************
	SPACES	 
( n -- )
	If 'n' is greater than zero, display 'n' spaces.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_STATE_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"state",	do_state)
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
#endif

/******************************************************************************/
#if DO_SWAP_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"swap",		do_swap)
/*******************************************************************************
	SWAP	 
( x_1 x_2 -- x_2 x_1)
	Exchange the top two stack items.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_THEN_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKIMMWORD(dictionary,	__COUNTER__,	"then",		do_then)
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
#endif

/******************************************************************************/
#if DO_TYPE_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"type",		do_type)
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
#endif

/******************************************************************************/
#if DO_U_DOT_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"u.",		do_u_dot)
/*******************************************************************************
	U.	[u-dot] 
( u -- )
	Display 'u' in free field format.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_U_LESS_THAN_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"u<",		do_u_less_than)
/*******************************************************************************
	U<	[u-less-than] 
( u_1 u_2 -- flag)
	'flag' is true if and only if 'u_1' is less than
	'u_2'.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_U_M_STAR_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"um*",		do_u_m_star)
/*******************************************************************************
	UM*	[u-m-star] 
( u_1 u_2 -- ud)
	Multiply 'u_1' by 'u_2', giving the unsigned double-cell
	product 'ud'. All values and arithmetic are unsigned.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_U_M_SLASH_MOD_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"um/mod",	do_u_m_slash_mod)
/*******************************************************************************
	UM/MOD	[u-m-slash-mod] 
( ud u_1 -- u_2 u_3)
	Divide 'ud' by 'u_1', giving the quotient 'u_3'
	and the remainder 'u_2'. All values and arithmetic are
	unsigned. An ambiguous condition exists if 'u_1' is zero or
	if the quotient lies outside the range of a single-cell unsigned
	integer.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_UNLOOP_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKIMMWORD(dictionary,	__COUNTER__,	"unloop",	do_unloop)
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
#endif

/******************************************************************************/
#if DO_UNTIL_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKIMMWORD(dictionary,	__COUNTER__,	"until",	do_until)
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
#endif

/******************************************************************************/
#if DO_VARIABLE_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"variable",	do_variable)
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
#endif

/******************************************************************************/
#if DO_WHILE_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKIMMWORD(dictionary,	__COUNTER__,	"while",	do_while)
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
#endif

/******************************************************************************/
#if DO_WORD_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"word",		do_word)
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
#endif

/******************************************************************************/
#if DO_XOR_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"xor",		do_xor)
/*******************************************************************************
	XOR	[x-or] 
( x_1 x_2 -- x_3)
	'x_3' is the bit-by-bit exclusive-or of 'x_1' with
	'x_2'.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_LEFT_BRACKET_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKIMMWORD(dictionary,	__COUNTER__,	"[",		do_left_bracket)
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
#endif

/******************************************************************************/
#if DO_BRACKET_TICK_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKIMMWORD(dictionary,	__COUNTER__,	"[']",		do_bracket_tick)
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
#endif

/******************************************************************************/
#if DO_BRACKET_CHAR_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKIMMWORD(dictionary,	__COUNTER__,	"[char]",	do_bracket_char)
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
#endif

/******************************************************************************/
#if DO_RIGHT_BRACKET_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKIMMWORD(dictionary,	__COUNTER__,	"]",		do_right_bracket)
/*******************************************************************************
	]	[right-bracket] 
(  -- )
	Enter compilation state.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_DOT_PAREN_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKIMMWORD(dictionary,	__COUNTER__,	".(",		do_dot_paren)
/*******************************************************************************
	.(	[dot-paren] 
\compile
	Perform the execution semantics given below.
\execute
	( "ccc<paren>" -- )
	Parse and display 'ccc' delimited by ")" (right
	parenthesis). '.p' is an immediate word.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_DOT_R_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	".r",		do_dot_r)
/*******************************************************************************
	.R	[dot-r] 
( n_1 n_2 -- )
	Display 'n_1' right aligned in a field 'n_2'
	characters wide. If the number of characters required to display
	'n_1' is greater than 'n_2', all digits are displayed
	with no leading spaces in a field as wide as necessary.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_ZERO_NOT_EQUALS_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"0<>",		do_zero_not_equals)
/*******************************************************************************
	0<>	[zero-not-equals] 
( x -- flag)
	'flag' is true if and only if 'x' is not equal to
	zero.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_ZERO_GREATER_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"0>",		do_zero_greater)
/*******************************************************************************
	0>	[zero-greater] 
( n -- flag)
	'flag' is true if and only if 'n' is greater than
	zero.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_TWO_TO_R_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"2>r",	        do_two_to_r)
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
#endif

/******************************************************************************/
#if DO_TWO_R_FROM_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"2r>",	        do_two_r_from)
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
#endif

/******************************************************************************/
#if DO_TWO_R_FETCH_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"2r@",	        do_two_r_fetch)
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
#endif

/******************************************************************************/
#if DO_COLON_NO_NAME_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKIMMWORD(dictionary,	__COUNTER__,	":noname",      do_colon_no_name)
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
#endif

/******************************************************************************/
#if DO_NOT_EQUALS_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"<>",	        do_not_equals)
/*******************************************************************************
	<>	[not-equals] 
( x_1 x_2 -- flag)
	'flag' is true if and only if 'x_1' is not bit-for-bit
	the same as 'x_2'.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_QUESTION_DO_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKIMMWORD(dictionary,	__COUNTER__,	"?do",	        do_question_do)
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
#endif

/******************************************************************************/
#if DO_AGAIN_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKIMMWORD(dictionary,	__COUNTER__,	"again",	do_again)
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
#endif

#if DO_BUFFER_COLON_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"buffer:",	do_buffer_colon)
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
#endif

/******************************************************************************/
#if DO_C_QUOTE_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"c\"",	        do_c_quote)
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
#endif

/******************************************************************************/
#if DO_CASE_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKIMMWORD(dictionary,	__COUNTER__,	"case",	        do_case)
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
#endif

/******************************************************************************/
#if DO_COMPILE_COMMA_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"compile,",	do_compile_comma)
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
#endif

/******************************************************************************/
#if DO_ENDCASE_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKIMMWORD(dictionary,	__COUNTER__,	"endcase",	do_endcase)
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
#endif

/******************************************************************************/
#if DO_ENDOF_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKIMMWORD(dictionary,	__COUNTER__,	"endof",	do_endof)
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
#endif

/******************************************************************************/
#if DO_ERASE_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"erase",	do_erase)
/*******************************************************************************
	ERASE	 
( addr u -- )
	If 'u' is greater than zero, clear all bits in each of
	'u' consecutive address units of memory beginning at
	'addr'.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_FALSE_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"false",	do_false)
/*******************************************************************************
	FALSE	 
(  -- false)
	Return a 'false' flag.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_HEX_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"hex",		do_hex)
/*******************************************************************************
	HEX	 
(  -- )
	Set contents of 'BASE' to sixteen.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_MARKER_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"marker",	do_marker)
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
#endif

/******************************************************************************/
#if DO_NIP_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"nip",		do_nip)
/*******************************************************************************
	NIP	 
( x_1 x_2 -- x_2)
	Drop the first item below the top of stack.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_OF_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKIMMWORD(dictionary,	__COUNTER__,	"of",		do_of)
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
#endif

/******************************************************************************/
#if DO_PAD_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"pad",		do_pad)
/*******************************************************************************
	PAD	 
(  -- c-addr)
	'c-addr' is the address of a transient region that can
	be used to hold data for intermediate processing.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_PARSE_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"parse",	do_parse)
/*******************************************************************************
	PARSE	 
( char "ccc<char>" -- c-addr u)
	Parse 'ccc' delimited by the delimiter 'char'.
	'c-addr' is the address (within the input buffer) and
	'u' is the length of the parsed string. If the parse area
	was empty, the resulting string has a zero length.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_PARSE_NAME_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"parse-name",		do_parse_name)
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
#endif

/******************************************************************************/
#if DO_PICK_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"pick",		do_pick)
/*******************************************************************************
	PICK	 
( x_u{\ldots}x_1 x_0 u -- x_u{\ldots)x_1 x_0 x_u}
	Remove 'u'. Copy the 'x_u' to the top of the stack.
	An ambiguous condition exists if there are less than 'u'+2
	items on the stack before 'PICK' is executed.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_REFILL_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"refill",	do_refill)
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
#endif

/******************************************************************************/
#if DO_RESTORE_INPUT_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"restore-input",	do_restore_input)
/*******************************************************************************
	RESTORE-INPUT	 
( x_n {\ldots} x_1 n -- flag)
	Attempt to restore the input source specification to the state
	described by 'x_1' through 'x_n'. 'flag' is
	true if the input source specification cannot be so restored.
	An ambiguous condition exists if the input source represented
	by the arguments is not the same as the current input source.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_ROLL_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"roll",		do_roll)
/*******************************************************************************
	ROLL	 
( x_u x_{u-1} {\ldots} x_0 u -- x_{u-1) {\ldots} x_0 x_u}
	Remove 'u'. Rotate 'u'+1 items on the top of the stack.
	An ambiguous condition exists if there are less than 'u'+2
	items on the stack before 'ROLL' is executed.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_S_BACKSLASH_QUOTE_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKIMMWORD(dictionary,	__COUNTER__,	"s\\\"",	do_s_backslash_quote)
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
#endif

/******************************************************************************/
#if DO_SAVE_INPUT_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"save-input",	do_save_input)
/*******************************************************************************
% -------------------------------------------------------------------
	SAVE-INPUT	 
(  -- x_n {\ldots) x_1 n}
	'x_1' through 'x_n' describe the current state of the
	input source specification for later use by 'RESTORE-INPUT'.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_SOURCE_ID_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"source-id",	do_source_id)
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
#endif

/******************************************************************************/
#if DO_TO_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKIMMWORD(dictionary,	__COUNTER__,	"to",		do_to)
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
#endif

/******************************************************************************/
#if DO_TRUE_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"true",		do_true)
/*******************************************************************************
	TRUE	 
(  -- true)
	Return a 'true' flag, a single-cell value with all
	bits set.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_TUCK_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"tuck",		do_tuck)
/*******************************************************************************
	TUCK	 
( x_1 x_2 -- x_2 x_1 x_2)
	Copy the first (top) stack item below the second stack item.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_U_DOT_R_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"u.r",		do_u_dot_r)
/*******************************************************************************
	U.R	[u-dot-r] 
( u n -- )
	Display 'u' right aligned in a field 'n' characters
	wide. If the number of characters required to display 'u'
	is greater than 'n', all digits are displayed with no leading
	spaces in a field as wide as necessary.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_U_GREATER_THAN_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"u>",		do_u_greater_than)
/*******************************************************************************
	U>	[u-greater-than] 
( u_1 u_2 -- flag)
	'flag' is true if and only if 'u_1' is greater than
	'u_2'.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_UNUSED_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"unused",	do_unused)
/*******************************************************************************
	UNUSED	 
(  -- u)
	'u' is the amount of space remaining in the region addressed
	by 'HERE', in address units.
*******************************************************************************/
#endif

/******************************************************************************/
#if DO_VALUE_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"value",	do_value)
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
#endif

/******************************************************************************/
#if DO_WITHIN_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKWORD(dictionary,	__COUNTER__,	"within",	do_within)
/************/ // MKWORD(dictionary,	__COUNTER__,	"within",	do_within)
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
#endif

/******************************************************************************/
/************/ // MKWORD(dictionary,	__COUNTER__,	"[compile]",	do_bracket_compile)
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
		) TERMINATOR
*/

/******************************************************************************/
#if DO_BACKSLASH_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
/************/ MKIMMWORD(dictionary,	__COUNTER__,	"\\",		do_backslash)
/*******************************************************************************
	\bs	[backslash] 
\compile
	Perform the execution semantics given below.
\execute
	( "ccc<eol>" -- )
	Parse and discard the remainder of the parse area.
	'bs' is an immediate word.
*******************************************************************************/
#endif

/*******************************************************************************
 *
 *
 * nonstandard, internal-use, helper etc. words
 *
 *
 ******************************************************************************/
/* internal use helper words */
#if DO_AHEAD_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
MKIMMWORD(dictionary,	__COUNTER__,	"ahead",	do_ahead)
#endif
#if DO_BREAK_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
MKWORD(dictionary,	__COUNTER__,	"break",	do_break)
#endif
#if DO_BYE_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
MKWORD(dictionary,	__COUNTER__,	"bye",	do_bye)
#endif
#if DO_LATEST_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
MKWORD(dictionary,	__COUNTER__,	"latest",	do_latest)
#endif
#if DO_JMPENV_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
MKWORD(dictionary,	__COUNTER__,	"jmpenv",	do_jmpenv)
#endif
#if DO_INCLUDED_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
MKWORD(dictionary,	__COUNTER__,	"included",	do_included)
#endif
#if DO_DOT_S_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
MKWORD(dictionary,	__COUNTER__,	".s",		do_dot_s)
#endif
#if DO_TO_NAME_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
MKWORD(dictionary,	__COUNTER__,	">name",		do_to_name)
#endif
#if DO_QUESTION_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
MKWORD(dictionary,	__COUNTER__,	"?",		do_question)
#endif
#if DO_DUMP_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
MKWORD(dictionary,	__COUNTER__,	"dump",		do_dump)
#endif
/* environment settings words */
#if DO_CR_ECHO_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
MKWORD(dictionary,	__COUNTER__,	"cr-echo",	do_cr_echo)
#endif
#if DO_COLON_DEBUG_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
MKWORD(dictionary,	__COUNTER__,	"colon-debug",	do_colon_debug)
#endif
#if DO_SMUDGE_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
MKWORD(dictionary,	__COUNTER__,	"smudge",	do_smudge)
#endif
#if DO_UNSMUDGE_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
MKWORD(dictionary,	__COUNTER__,	"unsmudge",	do_unsmudge)
#endif
#if SF_RESET_WORD_ENABLED || defined GENERATE_WORD_PROTOTYPES
MKWORD(dictionary,	__COUNTER__,	"sf-reset",	sf_reset)
#endif

#if EXCEPTIONS_ENABLED
MKWORD(dictionary,	__COUNTER__,	"catch",	do_catch)
MKWORD(dictionary,	__COUNTER__,	"throw",	do_throw)
#endif

#undef GENERATE_WORD_PROTOTYPES
#undef GENERATE_DICTIONARY_ENTRIES

