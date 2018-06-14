/*

sforth thumb2 model:
r13 - data stack pointer
r4 - return stack pointer
!!! THE RETURN STACK GROWS DOWN - ARM FULL-DESCENDING CONVENTION !!!

r5 - constant true (0xffffffff)/constant minus one
r6 - constant zero/false (0)
r7 - constant one (1)

r8 - low index for do loops
r9 - high index for do loops
r10 - variable/value data pool base address
r11 - nonzero-initialized variable data pool base address


*/

#define LITERAL_MINUS_ONE	r5
#define LITERAL_TRUE	LITERAL_MINUS_ONE
#define LITERAL_ZERO	r6
#define LITERAL_FALSE	LITERAL_ZERO
#define LITERAL_ONE	r7

#define RETURN_STACK_POINTER	r4
#define DATA_STACK_POINTER	r13

#define LOW_DO_LOOP_INDEX	r8
#define HIGH_DO_LOOP_INDEX	r9
#define ZEROED_DATA_POOL_BASE_ADDRESS	r10
#define INITIALIZED_DATA_POOL_BASE_ADDRESS	r11

.syntax unified
.code 16

do_swap_asm:
	pop	{ r0, r1 }
	push	{ r0 }
	push	{ r1 }

do_drop_asm:
	add	sp,	#4

do_rdrop_asm:
	adds	RETURN_STACK_POINTER,	#4

do_2drop_asm:
	add	sp,	#8

do_2rdrop_asm:
	adds	RETURN_STACK_POINTER,	#8

do_dup_asm:
	ldr	r0,	[sp, 0]
	push	{ r0 }

do_tuck_asm:
	pop	{ r0, r1 }
	push	{ r0 }
	push	{ r0, r1 }

do_nip_asm:
	pop	{ r0, r1 }
	push	{ r0 }

do_rot_asm:
	pop	{ r0, r1, r2 }
	push	{ r0, r1 }
	push	{ r2 }

do_one_plus_asm:
	pop	{ r0 }
	add	r0, r0, LITERAL_ONE
	push	{ r0 }

do_one_minus_asm:
	pop	{ r0 }
	add	r0, r0, LITERAL_MINUS_ONE
	push	{ r0 }

do_constant_zero_asm:
do_constant_false_asm:
	push	{ LITERAL_FALSE }

do_constant_one_asm:
	push	{ LITERAL_ONE }

do_constant_minus_one_asm:
do_constant_true_asm:
	push	{ LITERAL_TRUE }

do_2dup_asm:
	ldm	sp,	{ r0, r1 }
	push	{ r0, r1 }

do_store_asm:
	pop	{ r0, r1 }
	str	r1,	[r0, 0]

do_c_store_asm:
	pop	{ r0, r1 }
	strb	r1,	[r0, 0]

do_fetch_asm:
	pop	{ r0 }
	ldr	r0,	[r0, 0]
	push	{ r0 }

do_c_fetch_asm:
	pop	{ r0 }
	ldrb	r0,	[r0, 0]
	push	{ r0 }

word_prologue_asm:
	/* assumed is that a word is called via a 'bl' instruction */
	stmdb	RETURN_STACK_POINTER!,	{ lr }

word_epilog_asm:
	ldmia	RETURN_STACK_POINTER!,	{ pc }

do_to_r_asm:
	pop	{ r0 }
	stmdb	RETURN_STACK_POINTER!,	{ r0 }

do_r_from_asm:
	ldmia	RETURN_STACK_POINTER!,	{ r0 }
	push	{ r0 }

do_r_fetch_asm:
	ldr	r0,	[RETURN_STACK_POINTER, 0]
	push	{ r0 }

/* basic arithmetic words */
do_plus_asm:
	pop	{ r0, r1 }
	adds	r0, r0, r1
	push	{ r0 }

do_minus_asm:
	pop	{ r0, r1 }
	subs	r0, r1, r0
	push	{ r0 }
/* relation words */
do_zero_equals_asm:
	pop	{ r0 }
	orrs	r0,	r0
	ite	eq
	pusheq	{ LITERAL_TRUE }
	pushne	{ LITERAL_FALSE }

do_zero_not_equals_asm:
	pop	{ r0 }
	orrs	r0,	r0
	ite	ne
	pushne	{ LITERAL_TRUE }
	pusheq	{ LITERAL_FALSE }

do_zero_less_asm:
	pop	{ r0 }
	cmp	r0,	LITERAL_ZERO
	ite	lt
	pushlt	{ LITERAL_TRUE }
	pushge	{ LITERAL_FALSE }

do_zero_greater:
	pop	{ r0 }
	cmp	r0,	LITERAL_ZERO
	ite	gt
	pushgt	{ LITERAL_TRUE }
	pushle	{ LITERAL_FALSE }

do_equals_asm:
	pop	{ r0,	r1 }
	eors	r0,	r1
	ite	eq
	pusheq	{ LITERAL_TRUE }
	pushne	{ LITERAL_FALSE }

do_not_equals_asm:
	pop	{ r0,	r1 }
	eors	r0,	r1
	ite	ne
	pushne	{ LITERAL_TRUE }
	pusheq	{ LITERAL_FALSE }

do_less_than_asm:
	pop	{ r0,	r1 }
	cmp	r1,	r0
	ite	lt
	pushlt	{ LITERAL_TRUE }
	pushge	{ LITERAL_FALSE }

do_greater_than_asm:
	pop	{ r0,	r1 }
	cmp	r1,	r0
	ite	gt
	pushgt	{ LITERAL_TRUE }
	pushle	{ LITERAL_FALSE }

do_unsigned_less_than_asm:
	pop	{ r0,	r1 }
	cmp	r0,	r1
	ite	ls
	pushls	{ LITERAL_FALSE }
	pushhi	{ LITERAL_TRUE }

do_unsigned_greater_than_asm:
	pop	{ r0,	r1 }
	cmp	r1,	r0
	ite	hi
	pushhi	{ LITERAL_TRUE }
	pushls	{ LITERAL_FALSE }


do_do_asm:
	pop	{ r0,	r1 }
	stmdb	RETURN_STACK_POINTER!,	{ r0, r1 }

do_loop_asm:
	ldmia	RETURN_STACK_POINTER!,	{ r0 }
	add	r0,	r0,	LITERAL_ONE
	ldr	r1,	[RETURN_STACK_POINTER, #0]
	stmdb	RETURN_STACK_POINTER!,	{ r0 }
	eors	r0,	r1
	bne.n	do_loop_asm

do_do_unstacked_asm:
	/* store previous loop indeces, if any */
	stmdb	RETURN_STACK_POINTER!,	{ LOW_DO_LOOP_INDEX, HIGH_DO_LOOP_INDEX }
	pop	{ LOW_DO_LOOP_INDEX, HIGH_DO_LOOP_INDEX }

do_loop_unstacked_asm:
	add	LOW_DO_LOOP_INDEX, LITERAL_ONE
	cmp	LOW_DO_LOOP_INDEX, HIGH_DO_LOOP_INDEX
	bne.n	do_do_unstacked_asm
	/* store previous loop indeces, if any */
	ldmia	RETURN_STACK_POINTER!,	{ LOW_DO_LOOP_INDEX, HIGH_DO_LOOP_INDEX }

do_two_over_asm:
	ldrd	r0,	r1,	[DATA_STACK_POINTER, #4]
	push	{r0,	r1}

do_two_swap_asm:
	pop	{ r0,	r1,	r2,	r3 }
	push	{r0,	r1}
	push	{r2,	r3}
do_abs_asm:
	pop	{ r0 }
	cmp	R0,	LITERAL_ZERO
	it	lt
	/* negate - unified assembly (ual) syntax */
	rsblt	r0,	r0,	#0
	push	{ r0 }

do_and_asm:
	pop	{ r0, r1 }
	ands	r0, r0, r1
	push	{ r0 }
do_cell_plus_asm:
	pop	{r0}
	add	r0,	r0,	#4
	push	{r0}
do_cells_asm:
	pop	{r0}
	mov	r0,	r0,	lsl	#2
	push	{r0}
do_char_plus_asm:
	pop	{r0}
	add	r0,	r0,	LITERAL_ONE
	push	{r0}
do_count_asm:
	pop	{ r1 }
	ldrb	r0,	[r1, 0]
	add	r1,	r1,	LITERAL_ONE
	push	{ r0,	r1 }
do_invert_asm:
	pop	{ r0 }
	mvns	r0,	r0
	push	{ r0 }	
do_lshift_asm:
	pop	{ r0, r1 }
	mov	r0,	r1,	lsl r0
	push	{ r0 }	
do_m_star_asm:
	pop	{ r0, r1 }
	smull	r1,	r0,	r1,	r0
	push	{ r0, r1 }	
do_max_asm:
	pop	{ r0, r1 }
	cmp	r0,	r1
	it	lt
	movlt	r0,	r1
	push	{ r0 }
do_min_asm:
	pop	{ r0, r1 }
	cmp	r0,	r1
	it	gt
	movgt	r0,	r1
	push	{ r0 }
do_negate_asm:
	pop	{ r0 }
	rsbs	r0,	r0,	#0
	push	{ r0 }
do_or_asm:
	orrs	r0,	r1
do_over_asm:
	ldr	r0,	[DATA_STACK_POINTER, #4]
	push	{ r0 }
do_rshift_asm:
	pop	{ r0, r1 }
	mov	r0,	r1,	lsr r0
	push	{ r0 }
do_xor_asm:
	pop	{ r0, r1 }
	eors	r0,	r1
	push	{ r0 }
do_two_to_r_asm:
	pop	{ r0, r1 }
	stmdb	RETURN_STACK_POINTER!,	{ r0, r1 }
do_two_r_from_asm:
	ldmia	RETURN_STACK_POINTER!,	{ r0,	r1 }
	push	{ r0,	r1 }
do_two_r_fetch_asm:
	ldmia	RETURN_STACK_POINTER,	{ r0,	r1 }
	push	{ r0,	r1 }


/*
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
*/

do_slow_long_division:
#define DIVIDEND_HIGH_WORD	r1
#define REMAINDER DIVIDEND_HIGH_WORD
#define DIVIDEND_LOW_WORD	r2
#define DIVISOR		r0
#define QUOTIENT		r3
#define LOOP_COUNTER		r4
	pop	{ DIVISOR, DIVIDEND_HIGH_WORD, DIVIDEND_LOW_WORD }
	push	{ LOOP_COUNTER }
	/* check for overflow */
	cmp	DIVISOR,	DIVIDEND_HIGH_WORD
	ittt	ls
	/* r6 holds constant 0 */
	mvnls	QUOTIENT,	r6
	eorls	REMAINDER,	REMAINDER
	bls	done_division

	movs	LOOP_COUNTER,	#1
	
division_loop:

	/* r7 holds constant 1 */
	lsls	DIVIDEND_HIGH_WORD,	#1
	lsls	DIVIDEND_LOW_WORD,	#1
	it	cs
	orrcs	DIVIDEND_HIGH_WORD,	r7

	lsls	QUOTIENT,	r7
	cmp	DIVISOR,	DIVIDEND_HIGH_WORD
	itt	ls
	subls	DIVIDEND_HIGH_WORD,	DIVIDEND_HIGH_WORD,	DIVISOR
	orrls	QUOTIENT,	r7

	lsls	LOOP_COUNTER,	#1
	bcc	division_loop

done_division:
	pop	{ LOOP_COUNTER }

do_slash_mod_asm:
	pop	{ r2,	r3 }
	sdiv	r0,	r3,	r2
	muls	r2,	r3,	r2
	subs	r1,	r3,	r2
	push	{ r0,	r1 }

target_runtime_initialization:
	ldr	sp,	data_stack_pointer
	bl	1f
1:
	subs.w	lr,	lr,	#1
	/* first two loads are dummies, and their purpose is to advance the link register */
	ldmia	lr, { r0, r1, RETURN_STACK_POINTER, LITERAL_MINUS_ONE, LITERAL_ZERO, LITERAL_ONE, ZEROED_DATA_POOL_BASE_ADDRESS, INITIALIZED_DATA_POOL_BASE_ADDRESS, pc }
return_stack_pointer:
.word	0
literal_minus_one:
.word -1
literal_zero:
.word 0
literal_one:
.word 1
zeroed_data_pool_base_address:
.word	0
initialized_data_pool_base_address:
.word	0
startup_vector:
.word	0
data_stack_pointer:
.word	0



do_mod_asm:
	/* r1 = x; r0 = y */
	pop	{r0, r1}
	sdiv	r2,	r1,	r0
	muls	r1,	r2,	r1
	subs	r0,	r0,	r1

	mov	pc,	lr
	pop	{r0, r1, r2}
	strd	r1, r2, [r0]
	mov	r0,	r0,	lsl	#1
	mov	r0,	r0,	asr	#1
	ldrd	r0, r1, [r0]
	add	r8, r0
	adds	r0,	#4
	lsls	r0,	r1
	lsls	r0,	#1
	lsls	r0,	#2
	lsrs	r0,	r1
	asrs	r0,	#1
	adds	r0,	ZEROED_DATA_POOL_BASE_ADDRESS
do_move_asm:
	
	/* r2 - src, r1 - dest, r0 - count */
	pop	{r0,	r1,	r2}
	cbz	r0,	2f
1:
	ldrb	r4,	[r2], #1
	strb	r4,	[r1], #1
	subs	r0,	#1
	bne	1b
2:

do_fill_asm:
	
	/* r2 - c-addr, r1 - u, r0 - char */
	pop	{r0,	r1,	r2}
	cbz	r1,	2f
1:
	strb	r0,	[r2], #1
	subs	r1,	#1
	bne	1b
2:

/* miscellaneous helper words */
do_call_thru_r0:
	blx	r1
	blx	r0
do_bl_pc_relative:
	bl	here
here:
	bl	here

	ldr	r2,[r0]
	adds	r1,r1,r2
	str	r1,[r0]
	sdiv	r0,r1,r0
	adds	r0,	r0,	#4
	mov	r1,	sp
	subs	r0,	r0,	r1
	lsrs	r0,	#2

	lsls	r0,	r1,	r0
	ldmia	RETURN_STACK_POINTER!,	{ r8, r9 }


	pop	{ r0, r1, r2}
	push	{ r0 }
	push	{ r1,	r2 }

	mov	r1,	r0
	push	{r0,	lr}
	subs	lr,	#1
	pop	{r1,r2}
	orrs	r1,	r0
