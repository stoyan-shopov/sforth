.( a rudimentary cross-forth for armv7m machines) cr cr


\ sforth thumb2 model:
\ r13 - data stack pointer
\ r4 - return stack pointer
\ !!! THE RETURN STACK GROWS DOWN - ARM FULL-DESCENDING CONVENTION !!!

\ r5 - constant true (0xffffffff)/constant minus one
\ r6 - constant zero/false (0)
\ r7 - constant one (1)

\ r8 - low index for do loops
\ r9 - high index for do loops
\ r10 - zero-initialized variable data pool base address - 't-variable' populates this area
\ r11 - nonzero-initialized variable data pool base address - 't-value' populates this area

.( IMPORTANT: peephole optimizations to be done:) cr
.( [1] fuse redundant push/pop sequences) cr
.( [2] in regard to [1] - rotate literal generation between r0-r3 target registers;) cr
.( 	this should make fusing in point [1] easier and more efficient, and should benefit words such as @ and !) cr
.( [3] fuse common constant {0,1,-1} loading into the data stack with immediately succeeding pop{r0,...} ) cr
.( 	instructions into a mov{r0,rX} instruction ) cr
.( [4] fuse consecutive 'target-literal target-literal' sequences ) cr
.( [5] fuse consecutive 'rot rot' sequences ) cr
cr

here

\ set up a control stack for building do-leave-loop constructs
include cstack.fs

\ target state - equals true when compiling into target
false value t-state

4 constant return-stack-pointer-register
5 constant literal-true-register
5 constant literal-minus-1-register
6 constant literal-false-register
6 constant literal-zero-register
7 constant literal-one-register
8 constant do-loop-low-index-register
9 constant do-loop-high-index-register
10 constant zeroed-data-pool-base-register
11 constant initialized-data-pool-base-register
13 constant data-stack-pointer-register
14 constant armv7m-link-register

: bit ( bit-number -- bitmask)
	1 swap lshift ;



0 [if]
( --------------------------------------------------------------------------------------------------)
( --------------------------------------------------------------------------------------------------)
( --------------------------------------------------------------------------------------------------)
 _                       _
| |                     | |
| |_ __ _ _ __ __ _  ___| |_   _ __ ___   ___ _ __ ___   ___  _ __ _   _    __ _ _ __ ___  __ _ ___
| __/ _` | '__/ _` |/ _ \ __| | '_ ` _ \ / _ \ '_ ` _ \ / _ \| '__| | | |  / _` | '__/ _ \/ _` / __|
| || (_| | | | (_| |  __/ |_  | | | | | |  __/ | | | | | (_) | |  | |_| | | (_| | | |  __/ (_| \__ \
 \__\__,_|_|  \__, |\___|\__| |_| |_| |_|\___|_| |_| |_|\___/|_|   \__, |  \__,_|_|  \___|\__,_|___/
               __/ |                                                __/ |
              |___/                                                |___/
                             _       __ _       _ _   _
                            | |     / _(_)     (_) | (_)
                          __| | ___| |_ _ _ __  _| |_ _  ___  _ __  ___
                         / _` |/ _ \  _| | '_ \| | __| |/ _ \| '_ \/ __|
                        | (_| |  __/ | | | | | | | |_| | (_) | | | \__ \
                         \__,_|\___|_| |_|_| |_|_|\__|_|\___/|_| |_|___/

( --------------------------------------------------------------------------------------------------)
( --------------------------------------------------------------------------------------------------)
( --------------------------------------------------------------------------------------------------)
[then]
\ target specific memory areas
128 value data-stack-cell-size
128 value return-stack-cell-size
128 value zeroed-data-pool-cell-size
128 value initialized-data-pool-cell-size

$20000000 $5000 + 2 1024 * - value data-stack-base-address
data-stack-base-address return-stack-cell-size cells - value return-stack-base-address
return-stack-base-address return-stack-cell-size cells - zeroed-data-pool-cell-size cells - value zeroed-data-pool-base-address
zeroed-data-pool-base-address zeroed-data-pool-cell-size cells - initialized-data-pool-cell-size cells - value initialized-data-pool-base-address

0 value target-initial-data-stack-pointer-value-offset
$20000000 value target-code-base-address

2 constant target-halfword-size
target-halfword-size 2* constant target-word-size
1024 10 * constant target-core-halfword-space-size
0 value target-halfword-index
create target-core target-core-halfword-space-size target-halfword-size * allot
0 value target-variable-core-pool-index

\ thumb2 condition codes
\			 mnemonic	condition flags
0 constant cond-eq	\ equal		z == 1
1 constant cond-neq	\ not equal	z == 0
2 constant cond-cs	\ carry set	c == 1
3 constant cond-cc	\ carry clear	c ==0
4 constant cond-mi	\ minus/negative		n == 1
5 constant cond-pl	\ plus, positive or zero	n == 0
6 constant cond-vs	\ overflow	v == 1
7 constant cond-vc	\ no overflow	v == 0
8 constant cond-hi	\ unsigned higher	c == 1 and z == 0
9 constant cond-ls	\ unsigned lower or same	c == 0 or z == 1
10 constant cond-ge	\ signed greater than or equal	n == v
11 constant cond-lt	\ signed less than	n != v
12 constant cond-gt	\ signed greater than	z == 0 and n == v
13 constant cond-le	\ signed less than or equal	z == 1 or n != v
14 constant cond-al	\ always (unconditional)	any

\ this table contains the inverse condition of the condition codes above, in that order
\ these are used by some target code-generation words
create inverse-condition-table
cond-neq c,
cond-eq c,
cond-cc c,
cond-cs c,
cond-pl c,
cond-mi c,
cond-vc c,
cond-vs c,
cond-ls c,
cond-hi c,
cond-lt c,
cond-ge c,
cond-le c,
cond-gt c,

\ these values are mainly for use by the optimizer
\ the number of halfwords, starting from index target-halfword-index, and
\ running backwards; this determines how far backwards the optimizer is
\ allowed to look for code sequences to optimize
variable compiled-halfwords-length
: reset-compiled-length 0 compiled-halfwords-length ! ;
reset-compiled-length
:noname ; value optimizer-xt

: invert-condition-code ( condition-code -- inverse-condition-code)
	inverse-condition-table + c@
	;

: t-align ( --)
	target-halfword-index 1+ 1 invert and to target-halfword-index
	;
: thw-helper ( n --) \ target half-word comma, used for storing halfwords into the target core
	target-halfword-index target-halfword-size * target-core +
	2dup ( store low byte) c!
	swap 8 rshift swap 1+ ( store high byte) c!
	target-halfword-index 1+ to target-halfword-index
	;
: thw, ( n --) \ target half-word comma, used for storing halfwords into the target core
	thw-helper reset-compiled-length ;
: chw, ( n --) \ compile target half-word, used for inline assembling
	1 compiled-halfwords-length +! thw-helper
	optimizer-xt execute
	;

: thw! ( halfword-value halfword-index --)
	target-halfword-size * target-core + 2dup c!
	1+ swap 8 rshift swap c!
	optimizer-xt execute
	;
: thw@ ( halfword-index --) \ target-halfword-fetch
	target-halfword-size * target-core + dup c@ swap 1+ c@ 8 lshift or
	;

: swap-halfwords ( n -- )
	dup 16 lshift swap 16 rshift $ffff and or ;


: tw, ( n --) \ target word comma, used for storing words into the target core
	dup thw, 16 rshift thw,
	;
: cw, ( n --) \ compile target word, used for inline assembling
	dup chw, 16 rshift chw,
	;

: tw! ( word-value halfword-index --)
	2dup thw! 1+ swap 16 rshift swap thw!
	;
: tw@ ( halfword-index -- word-value)
	dup thw@ swap 1+ thw@ 16 lshift or
	;

: generate-mov(t)-literal ( n -- n)
	\ given a 16 bit literal - shuffles its bits so the result is then
	\ suitable for or-ing into the machine opcode of a mov(t) arm thumb2 instruction
	$ffff and

	\ extract bit12
	dup [ 1 11 lshift literal ] and 0<> [ 1 26 lshift literal ] and
	\ extract bits[0:7]
	over $ff and or
	\ extract bits[8:10]
	over 8 rshift 7 and 12 lshift or
	\ extract bits[12:15]
	swap 12 rshift $f and 16 lshift or
	;

: clz ( n -- number-of-leading-zeros) \ count leading zeros
	0 0 target-word-size 8 * 1- do
		over i bit and 0<> if leave then
		1+
	-1 +loop nip
	;

: generate-literal-in-rX ( n register-number --)
	15 and >r
	\ optimize - see if the value loaded is zero
	r@ 8 < over 0= and if
		\ generate an 'eors{rd,rd}' instruction
		drop r> dup 3 lshift or %0100000001000000 or chw,
	exit then
	\ optimize - see if the value loaded is less than $ff and use a short opcode if this is the case
	r@ 8 < over $100 u< and if
		\ generate a 'mov{rd,#imm8}' instruction
		%0010000000000000 r> 8 lshift or or chw,
       	exit then

	\ optimize - see if the value loaded is a single bit set
	r@ 8 < over dup 1- and 0= and if
		\ generate an 'lsls{rd,literal-one-register,#imm5}' instruction
		target-word-size 8 * 1- swap clz - $1f and 6 lshift r> or literal-one-register 3 lshift or
		%0000000000000000 or chw,
       	exit then

	\ generate a pair of (mov, movt) instructions to generate the literal
	\ generate a 'mov' instruction to load the bottom half of r0
	dup generate-mov(t)-literal
	%11110010010000000000000000000000 or
	\ fuse the register number
	r@ 8 lshift or
	\ instruction synthesized
	swap-halfwords cw,

	\ generate a 'movt' instruction to load the top half of r0
	\ if the top half is zero - there is nothing more to do as the top
	\ half will already have been zeroed by the previously synthesized instruction
	16 rshift
	dup 0= if r> 2drop exit then
	generate-mov(t)-literal
	%11110010110000000000000000000000 or
	\ fuse the register number
	r> 8 lshift or
	\ instruction synthesized
	swap-halfwords cw,
	;

: generate-literal-in-r0 ( n --)
	0 generate-literal-in-rX
	;

: generate-data-pool-address-in-r0 ( data-pool-offset --)
	\ if the offset fits in 12 bits, use an add instruction
	dup $1000 < if
		\ generate the immediate field of the instruction
		dup 11 bit and 0<> 26 bit and
		over 7 8 lshift and 4 lshift or
		swap $ff and or
		\ fuse-in the instruction opcode
		%11110010000000000000000000000000
		zeroed-data-pool-base-register 16 lshift or
		or
		swap-halfwords cw,
	exit then
	\ use a literal generation to r0, and an add instruction
	generate-literal-in-r0
	%0100010000000000 zeroed-data-pool-base-register 3 lshift or chw,
	;

0 [if]
( ------------------------------------------------------)
( ------------------------------------------------------)
( ------------------------------------------------------)
                ___ _ __ ___  ___ ___
               / __| '__/ _ \/ __/ __|
              | (__| | | (_) \__ \__ \
               \___|_|  \___/|___/___/
                                  _     _
                                 | |   | |
      __ _ ___ ___  ___ _ __ ___ | |__ | | ___ _ __
     / _` / __/ __|/ _ \ '_ ` _ \| '_ \| |/ _ \ '__|
    | (_| \__ \__ \  __/ | | | | | |_) | |  __/ |
     \__,_|___/___/\___|_| |_| |_|_.__/|_|\___|_|

( ------------------------------------------------------)
( ------------------------------------------------------)
( ------------------------------------------------------)
[then]

16 value last-register-pushed
0 value last-code-location

: push{rX} ( register-number --)
\  2c:	b401      	push	{r0}
	dup 7 > over armv7m-link-register <> and
	if ." bad register number for push{rX}: " . cr abort
	then
	\ use short instruction opcode
	1 swap
	\ save last push instruction generation data
	dup to last-register-pushed target-halfword-index to last-code-location
	\ special case for the link register
	armv7m-link-register over = if drop 8 then
	lshift
	$b400 or chw, ;

: pop{rX} ( register-number --)
	dup 7 > abort" bad register number for push{rX}"
	1 swap
	\ see if it is possible this pop instruction to cancel with a push instruction immediately preceding it
	dup last-register-pushed = last-code-location target-halfword-index 1 - = and
	[ [defined] ENABLE-PEEPHOLE-OPTIMIZER literal ] and
	if
		." push/pop instructions fused together" cr
		\ store an invalid number to the last register saved
		16 to last-register-pushed
		target-halfword-index 1 - to target-halfword-index
		2drop exit
	then
\  22:	bc01      	pop	{r0}
	lshift
	$bc00 or chw,
	;

include armv7m-stack-optimizer.fs
' run-optimizer to optimizer-xt

: muls{r0,r1}		%0100001101001000 chw, ;
: adds(r0,r0,r1)	%0001100001000000 chw, ;
: ldr(r2,[r0,#0])	$6802 chw, ;
: adds(r1,r1,r2)	$1889 chw, ;
: str(r1,[r0,#0])	$6001 chw, ;
: subs(r0,r1,r0)	$1a08 chw, ;
: sdiv(r0,r1,r0)	$fb91 chw, $f0f0 chw, ;
: cmp(r0,literal-zero)	$42b0 chw, ;
: it ( condition-code --) \ if-then
	%1011111100001000 over $f and 4 lshift or swap
	1 and 1 xor 3 lshift or chw, ;

-1 value last-if-then-else-halfword-index
cond-al value last-if-then-else-condition
-1 value last-literal-true-push-halfword-index
-1 value last-literal-false-push-halfword-index

: ite ( condition-code --) \ if-then-else
	target-halfword-index to last-if-then-else-halfword-index
	dup to last-if-then-else-condition
	%1011111100000100 over $f and 4 lshift or swap
	1 and 1 xor 3 lshift or chw, ;
: push(literal-true)	target-halfword-index to last-literal-true-push-halfword-index literal-true-register push{rX} ;
: push(literal-false)	target-halfword-index to last-literal-false-push-halfword-index literal-false-register push{rX} ;

: pop-regs{...} ( register-bitmap --)
	dup 0= abort" bad register bitmap for pop-regs{...}"
	\ select the most suitable encoding
	\ see if only one register is being popped
	\ see if only low registers (and/or the program counter) are being popped
	dup 15 bit $ff or invert and 0= if
		\ only low registers (and/or the program counter) are being popped
		dup 15 bit and 0<> if 15 bit xor 8 bit or then
		%1011110000000000 or chw,
	then
	." error - (must be fixed) register combination not yet supported" cr abort
	;

: pop{r0,r1}-unfusing
\   d6:	bc03      	pop	{r0, r1}
	$bc03 chw,
	;
: ?preceding-insn-push{r0}
	last-register-pushed 0 = last-code-location target-halfword-index 1 - = and
	;
: pop{r0,r1}-fusing
\ try to fuse with any immediately preceding push{r0} instruction
	?preceding-insn-push{r0}
	[ [defined] ENABLE-PEEPHOLE-OPTIMIZER literal ] and
	if
		." fusing a pop{r0,r1} instruction with an immediately preceding push{r0} instruction" cr
		target-halfword-index 1 - to target-halfword-index
		1 pop{rX}
	else
		pop{r0,r1}-unfusing
	then
	;


: pop{r0,r1,r2}
	$bc07 chw, ;

: pop{r0,r1} pop{r0,r1}-fusing ;
: push{r0,r1}
\   16:	b403      	push	{r0, r1}
	$b403 chw, ;

: orrs(r0,r0)
\  78:	4300      	orrs	r0, r0
	$4300 chw,
	;

: blx(r0) ( --)
\  e2:	4780      	blx	r0
	$4780 chw, ;

: mov{rd,rm} ( r-dest r-m --)
	15 and 3 lshift swap 15 and
	3 bit over and 0<> 7 bit and or or
	\ fuse-in opcode bits
	$4600 or chw,
	;
: ldr{r0,[sp]}
\    e:	9800      	ldr	r0, [sp, #0]
\ try to fuse with any immediately preceding push{r0} instruction
	?preceding-insn-push{r0} if
		." cancellation of a ldr{r0,[sp]} instruction by an immediately preceding push{r0} instruction" cr
	else
		$9800 chw,
	then
	;

: fixup-pc-halfword-address
\ given a halfword address, assuming it is the address of an instruction,
\ computes the value that the target program counter contains when
\ the instruction at that address is being executed
\ basically, adds two halfwords (4 bytes) to the address of the instruction
2 + ;
: generate-cb(n)z-instruction ( source-halfword-index destination-halfword-index opcode-halfword -- )
	\ generates a cb(n)z instruction at the source branch halfword index; the exact opcode
	\ (cbz or cbnz) is specified by the opcode-halfword value
	rot fixup-pc-halfword-address rot swap -
	dup $3f > abort" generate-cb(n)z-instruction - cannot resolve a short forward branch on zero/nonzero - branch target too far"
	\ construct branch target
	dup 6 bit and 0<> 9 bit and swap $1f and 3 lshift or
	\ fuse into opcode, and assemble to target
	or swap
	thw!
	;


: generate-conditional-branch ( branch-source-halfword-index branch-target-halword-index condition-code -- conditional-branch-halfword-instruction)
	8 lshift %1101000000000000 or rot rot
	swap fixup-pc-halfword-address -
	( make sure branch target is in range)
	dup -128 < over 127 > or
	abort" branch target address out of range"
	$ff and or
	;

: generate-unconditional-branch ( branch-source-halfword-index branch-target-halword-index --)
	swap fixup-pc-halfword-address -
	( make sure branch target is in range)
	dup -2048 < over 2047 > or
	abort" branch target address out of range"
	$7ff and %1110000000000000 or
	;

\ flow control words

: generate-branch-with-link ( destination-target-halfword-index source-target-halfword-index -- branch-and-link-opcode-word)
\ normalize - much of this can be optimized away; still it makes the code more readable, and closer to the book
	\ compute relative address
	target-halfword-size * swap target-halfword-size * swap
	( take in account the length of the bl instruction ) 4 +
	-
	\ validate range
	dup -16777216 < over 16777214 > or
	abort" bl target address out of range"
	2/
	dup 0< 3 21 lshift dup rot and xor xor
	dup
	$7ff and
	over 11 rshift $3ff and 16 lshift or
	over 0< 1 and 26 lshift or
	over 21 rshift 1 and 11 lshift or
	swap 22 rshift 1 and 13 lshift or
	\ add the instruction opcode fields
	%11110000000000001101000000000000 or
	swap-halfwords
	;

false value ?branch-and-link-calls-in-current-word
: t: create smudge
false to ?branch-and-link-calls-in-current-word
\ 0000004e <word_prologue_asm>:
\   4e:	f844 ed04 	str.w	lr, [r4, #-4]!
	true to t-state
	target-halfword-index , $f844 chw, $ed04 chw,
	does> @ target-halfword-index generate-branch-with-link cw, true to ?branch-and-link-calls-in-current-word
	;

: t-literal ( n --)
	\ optimize cases for common constants
	dup 0= if literal-zero-register push{rX} drop exit then
	dup 1 = if literal-one-register push{rX} drop exit then
	dup -1 = if literal-minus-1-register push{rX} drop exit then
	generate-literal-in-r0 0 push{rX}
	;

: t-var-intermixed
	create t-align target-halfword-index , 0 cw, does> @ target-halfword-size * generate-literal-in-r0 0 push{rX} ;

: t-var-pooled
	create target-variable-core-pool-index dup ,
	1+ to target-variable-core-pool-index
	does>
	@ target-word-size * generate-literal-in-r0
\  f4:   4460            add     r0, ip
	$4460	chw, 0 push{rX}
	;

0 value next-register
: t-var-pooled-alternating-registers
	create target-variable-core-pool-index dup ,
	1+ to target-variable-core-pool-index
	does>
	@ target-word-size * next-register generate-literal-in-rX
\  f4:   4460            add     r0, ip
	$4460	chw, next-register dup push{rX}
	1+ 3 and to next-register
	;

\ choose which variable behavior to use
: t-var t-var-pooled ;

true [if]

: t-l t-literal ;
: t-bit
\  20a:   fa37 f000       lsrs.w  r0, r7, r0
	0 pop{rX} $fa37 chw, $f000 chw, 0 push{rX}
	;


0 [if]
( ------------------------------------------------------)
( ------------------------------------------------------)
( ------------------------------------------------------)
                 ___ ___  _ __ ___
                / __/ _ \| '__/ _ \
               | (_| (_) | | |  __/
                \___\___/|_|  \___|
                                   _
                                  | |
            __      _____  _ __ __| |___
            \ \ /\ / / _ \| '__/ _` / __|
             \ V  V / (_) | | | (_| \__ \
              \_/\_/ \___/|_|  \__,_|___/

( ------------------------------------------------------)
( ------------------------------------------------------)
( ------------------------------------------------------)
[then]

\ !	[store]
: t-!
	pop{r0,r1}
	." XXX compiled halfwords: " compiled-halfwords-length @ . cr
	str(r1,[r0,#0]) ;
\ #	[number-sign]		- not available
\ #	[number-sign-greater]	- not available
\ #S	[number-sign-s]		- not available
\ '	[tick]
: t-'
	' >body @ target-halfword-size * ( set the thumb bit) 1 or ;
\ (	[paren]			- not available
\ *	[star]
: t-*
	pop{r0,r1} muls{r0,r1} 0 push{rX}
	;
\ */	[star-slash]		- pending
\ */MOD	[star-slash-mod		- pending
\ +	[plus]
: t-+
	pop{r0,r1} adds(r0,r0,r1) 0 push{rX}
	;
\ +!	[plus-store]
: t-+!
	pop{r0,r1} ldr(r2,[r0,#0]) adds(r1,r1,r2) str(r1,[r0,#0]) ;
\ ,	[comma]			- not available
\ -	[minus]
: t--
	pop{r0,r1} subs(r0,r1,r0) 0 push{rX} ;
\ .	[dot]			- not available
\ ."	[dot-quote]		- not available
\ /	[slash]
: t-/
	pop{r0,r1} sdiv(r0,r1,r0) 0 push{rX} ;
\ /MOD	[slash-mod]
: t-/mod
\ 000001b4 <do_slash_mod_asm>:
\ 1b4:   bc0c            pop     {r2, r3}
\ 1b6:   fb93 f0f2       sdiv    r0, r3, r2
\ 1ba:   435a            muls    r2, r3
\ 1bc:   1a99            subs    r1, r3, r2
\ 1be:   b403            push    {r0, r1}
2 bit 3 bit or pop-regs{...}
$fb93 chw, $f0f2 chw, $435a chw, $1a99 chw,
push{r0,r1}
	;

\ 0<	[zero-less]
: t-0<
0 pop{rX} cmp(r0,literal-zero) cond-lt ite push(literal-true) push(literal-false) ;
\ 0=	[zero-equals]
: t-0=
\ 00000076 <do_zero_equals_asm>:
\   76:	bc01      	pop	{r0}
\   78:	4300      	orrs	r0, r0
\   7a:	bf0c      	ite	eq
\   7c:	b420      	pusheq	{r5}
\   7e:	b440      	pushne	{r6}
0 pop{rX} orrs(r0,r0) cond-eq ite push(literal-true) push(literal-false) ;
\ 1+	[one-plus]
: t-1+
\ 00000022 <do_one_plus_asm>:
\   22:	bc01      	pop	{r0}
\   24:	4438      	add	r0, r7
\   26:	b401      	push	{r0}
0 pop{rX} $4438 chw, 0 push{rX}
	;
\ 1-	[one-minus]
: t-1-
\ 00000028 <do_one_minus_asm>:
\   28:	bc01      	pop	{r0}
\   2a:	4428      	add	r0, r5
\   2c:	b401      	push	{r0}
0 pop{rX} $4428 chw, 0 push{rX}
	;
\ 2!	[two-store]		- pending
: t-2!
\ 1da:   bc07            pop     {r0, r1, r2}
\ 1dc:   e9c0 1200       strd    r1, r2, [r0]
	pop{r0,r1,r2} $e9c0 chw, $1200 chw,
	;
\ 2*	[two-star]		- pending
: t-2*
\ 1f0:   0040            lsls    r0, r0, #1
	0 pop{rX} $0040 chw, 0 push{rX}
	;
\ 2/	[two-slash]		- pending
: t-2/
\ 1f6:   1040            asrs    r0, r0, #1
	0 pop{rX} $1040 chw, 0 push{rX}
	;
\ 2@	[two-fetch]		- pending
: t-2@
\ 1e8:   e9d0 0100       ldrd    r0, r1, [r0]
	0 pop{rX} $e9d0 chw, $0100 chw, push{r0,r1}
	;
\ 2DROP	[two-drop]
: t-2drop
\ 0000000c <do_2drop_asm>:
\    c:	b002      	add	sp, #8
	$b002 chw, ;

\ 2DUP	[two-dupe]
: t-2dup
\ 00000034 <do_2dup_asm>:
\   34:	e89d 0003 	ldmia.w	sp, {r0, r1}
\   38:	b403      	push	{r0, r1}
	$e89d chw, $0003 chw, push{r0,r1} ;
\ 2OVER	[two-over]
: t-2over
\ 000000fa <do_two_over_asm>:
\ fa:	e9dd 0101 	ldrd	r0, r1, [sp, #4]
\ fe:	b403      	push	{r0, r1}
	$e9dd chw, $0101 chw, push{r0,r1}
	;
\ 2SWAP					[two-swap]
: t-2swap
\ 00000100 <do_two_swap_asm>:
\ 100:	bc0f      	pop	{r0, r1, r2, r3}
\ 102:	b403      	push	{r0, r1}
\ 104:	b40c      	push	{r2, r3}
	$bc0f chw, push{r0,r1} $b40c chw,
	;

\ :					[colon]		- not available; use word 't:' instead
\ ;					[semicolon]
: t;
\ 00000052 <word_epilog_asm>:
\   52:	f854 fb04 	ldr.w	pc, [r4], #4
	false to t-state
	\ if the current word does not use branch-and-link call instructions,
	\ then the link register needs not be saved, and the word prologue and
	\ epilogue can be optimized
	?branch-and-link-calls-in-current-word if
		\ do not optimize
		$f854 chw, $fb04 chw,
	else
		\ optimize
		\ destroy word prologue - it is not needed
		latest @ >body @ target-halfword-size *
		target-halfword-index target-halfword-size * over -
		target-halfword-size 2* - >r
		target-core + dup target-halfword-size 2* + swap r>
		cmove
		target-halfword-index 2 - to target-halfword-index
		\ 1d6:   46f7            mov     pc, lr
		$46f7 chw,
	then unsmudge
	;

\ <					[less-than]
: t-<
\ 000000ba <do_less_than_asm>:
\   ba:	bc03      	pop	{r0, r1}
\   bc:	4281      	cmp	r1, r0
\   be:	bfb4      	ite	lt
\   c0:	b420      	pushlt	{r5}
\   c2:	b440      	pushge	{r6}
pop{r0,r1} $4281 chw, cond-lt ite push(literal-true) push(literal-false)
	;
\ <\num					[less-number-sign]	- not available
\ =					[equals]
: t-=
\ 0000009e <do_equals_asm>:
\   9e:	bc03      	pop	{r0, r1}
\   a0:	4048      	eors	r0, r1
\   a2:	bf0c      	ite	eq
\   a4:	b420      	pusheq	{r5}
\   a6:	b440      	pushne	{r6}
pop{r0,r1} $4048 chw, cond-eq ite push(literal-true) push(literal-false)
	;
\ >					[greater-than]
: t->
\ 000000c4 <do_greater_than_asm>:
\   c4:	bc03      	pop	{r0, r1}
\   c6:	4281      	cmp	r1, r0
\   c8:	bfcc      	ite	gt
\   ca:	b420      	pushgt	{r5}
\   cc:	b440      	pushle	{r6}
pop{r0,r1} $4281 chw, cond-gt ite push(literal-true) push(literal-false)
	;
\ >BODY					[to-body]	- not available
\ >IN					[to-in]		- not available
\ >NUMBER				[to-number]	- not available
\ >R					[to-r]
: t->r
\ 00000056 <do_to_r_asm>:
\ 56:	bc01      	pop	{r0}
\ 58:	f844 0d04 	str.w	r0, [r4, #-4]!
	0 pop{rX} $f844 chw, $0d04 chw,
	;

\ ?DUP					[question-dupe]
: t-?dup
	ldr{r0,[sp]} orrs(r0,r0) cond-neq it 0 push{rX}
	;
\ @					[fetch]
: t-@
\ 00000042 <do_fetch_asm>:
\   42:	bc01      	pop	{r0}
\   44:	6800      	ldr	r0, [r0, #0]
\   46:	b401      	push	{r0}
	0 pop{rX} $6800 chw, 0 push{rX}
	;
\ ABORT		- not available
\ ABORT"					[abort-quote]		- not available
\ ABS					[abs]
: t-abs
\ 00000106 <do_abs_asm>:
\ 106:	bc01      	pop	{r0}
\ 108:	42b0      	cmp	r0, r6
\ 10a:	bfb8      	it	lt
\ 10c:	4240      	neglt	r0, r0
\ 10e:	b401      	push	{r0}
	0 pop{rX} $42b0 chw, cond-lt it $4240 chw, 0 push{rX}
	;
\ ACCEPT	- not available
\ ALIGN 	- not available
\ ALIGNED	- not available
\ ALLOT		- not available
\ AND
: t-and
\ 00000110 <do_and_asm>:
\ 110:	bc03      	pop	{r0, r1}
\ 112:	4008      	ands	r0, r1
\ 114:	b401      	push	{r0}
	pop{r0,r1} $4008 chw, 0 push{rX}
	;
\ BASE	- not available
\ BEGIN
: t-begin
	target-halfword-index
	reset-compiled-length
	;
\ BL					[b-l]
: t-bl
	bl t-l
	;
\ C!					[c-store]
: t-c!
\ 0000003e <do_c_store_asm>:
\ 3e:	bc03      	pop	{r0, r1}
\ 40:	7001      	strb	r1, [r0, #0]
	pop{r0,r1} $7001 chw,
	;

\ C,					[c-comma]	- not available
\ C@					[c-fetch]
: t-c@
\ 00000048 <do_c_fetch_asm>:
\ 48:	bc01      	pop	{r0}
\ 4a:	7800      	ldrb	r0, [r0, #0]
\ 4c:	b401      	push	{r0}
	0 pop{rX} $7800 chw, 0 push{rX}
	;
\ CELL+					[cell-plus]
: t-cell+
\ 00000116 <do_cell_plus_asm>:
\ 228:   3004            adds    r0, #4
	0 pop{rX} $3004 chw, 0 push{rX}
	;
\ CELLS
: t-cells
\ 0000011e <do_cells_asm>:
\ 11e:	bc01      	pop	{r0}
\ 1f2:  0080            lsls    r0, r0, #2
\ 124:	b401      	push	{r0}
	0 pop{rX} $0080 chw, 0 push{rX}
	;
\ CHAR					[char]	- not available
\ CHAR+					[char-plus]
: t-char+ t-1+
	;
\ CHARS					[chars]
: t-chars	;	\ nothing
\ CONSTANT
: t-constant create , does> @ t-state true = if t-literal then ;
\ COUNT
: t-count
\ 0000012c <do_count_asm>:
\ 12c:	bc02      	pop	{r1}
\ 12e:	7808      	ldrb	r0, [r1, #0]
\ 130:	4439      	add	r1, r7
\ 132:	b403      	push	{r0, r1}
	1 pop{rX} $7808 chw, $4439 chw, push{r0,r1}
	;
\ CR					[c-r]	- not available
\ CREATE
: t-create ( --)
	target-halfword-index 2* target-code-base-address + t-constant
	;
\ DECIMAL	- not available
\ DEPTH
: t-depth
\ 22a:   4669            mov     r1, sp
\ 22c:   1a40            subs    r0, r0, r1
\ 22e:   0880            lsrs    r0, r0, #2
	target-initial-data-stack-pointer-value-offset
	target-code-base-address + t-l
	t-@ 0 pop{rX}
	$4669 chw, $1a40 chw, $0880 chw,
	0 push{rX}
	;
\ DO
: t-do-stacked
\ 000000d6 <do_do_asm>:
\   d6:	bc03      	pop	{r0, r1}
\   d8:	e924 0003 	stmdb	r4!, {r0, r1}
	pop{r0,r1} $e924 chw, $0003 chw,
	target-halfword-index
	;
: t-do-unstacked
\ 000000e8 <do_do_unstacked_asm>:
\   e8:	e924 0300 	stmdb	r4!, {r8, r9}
\   ec:	e8bd 0300 	ldmia.w	sp!, {r8, r9}
	\ initialize 'leave' counter onto the control stack
	0 >cstack
\ store previous loop control indeces, if any
	$e924 chw, $0300 chw,
\ load new loop control indeces
	$e8bd chw, $0300 chw,
	\ store branch target to be resolved by t-loop
	target-halfword-index
	;
: t-do t-do-unstacked ;
\ DOES>					[does]	- not available
\ DROP
: t-drop ( --)
\ 0000000a <do_drop_asm>:
\    a:	b001      	add	sp, #4
	$b001 chw, ;
\ DUP					[dupe]
: t-dup
\ 0000000e <do_dup_asm>:
\    e:	9800      	ldr	r0, [sp, #0]
\   10:	b401      	push	{r0}
	ldr{r0,[sp]} 0 push{rX} ;
\ THEN
: t-then ( source-halfword-index --)
	\ the halfword at index 'source-halfword-index' contains the condition of the branch instruction,
	\ or - as a special case - a c(b)nz instruction opcode
	\ resolve forward branch target initiated by t-if
	dup target-halfword-index over
	\ process branch instruction condition code
	thw@
	\ check for a special-case short forward branch instruction requested by armv7m cb(n)z instructions
	dup 12 rshift %1011 = if
		." t-then - detected a short forward branch on zero/nonzero" cr
		generate-cb(n)z-instruction
		exit
	then
	\ see if this is a special branch opcode
	dup 15 > abort" unsupported branch opcode handed for resolution to t-then"
	dup cond-al = if drop generate-unconditional-branch else generate-conditional-branch then
	swap thw!
	;
\ ELSE
: t-else
	\ reserve space for an unconditional branch to be later resolved by t-then
	target-halfword-index cond-al chw,
	swap t-then
	;

\ EMIT	- not available
\ ENVIRONMENT?					[environment-query]	- not available
\ EVALUATE	- not available
\ EXECUTE
: t-execute
	0 pop{rX} blx(r0)
	;

\ EXIT
: t-exit
	\ if code in the current word until now does not use branch-and-link call instructions,
	\ then the link register has not been trashed, and can be used for the return address
	?branch-and-link-calls-in-current-word false = if
		\ 1d6:   46f7            mov     pc, lr
		$46f7 chw,
	else
		\ load the return address from the return stack
		$f854 chw, $fb04 chw,
	then
	;
\ FIND	- not available
\ FM/MOD					[f-m-slash-mod]	- MUST BE DONE
\ HERE	- not available
\ HOLD	- not available
\ I
: t-i
	do-loop-low-index-register push{rX}
	;
\ IF
: t-if-long-branch \ target-if
	0 pop{rX}
	orrs(r0,r0)
	\ remember the address for branch resolution by t-then
	target-halfword-index
	\ reserve space for the branch-on-zero instruction - save condition for instruction
	cond-eq chw,
	;

: ?immediately-after-if-then-else-sequence ( -- f:true, if the current target code location is immediately after an if-then-else code sequence)
	last-if-then-else-halfword-index target-halfword-index 3 - =
	last-literal-false-push-halfword-index last-literal-true-push-halfword-index
	2dup min target-halfword-index 2 - = rot rot max target-halfword-index 1- =
	and and
	;

: t-if-short-branch \ target-if
	\ try to detect if this t-if word is immediately preceded by
	\ an 'ite cond; push.cond literal-true-or-false; push.not.cond literal-true-or-false'
	\ sequence, and if so - try to optimize the sequence
	?immediately-after-if-then-else-sequence if
		last-literal-false-push-halfword-index target-halfword-index 1- =
		if
			." optimizer: merging an if-then-else sequence into t-if"cr
			target-halfword-index 3 - to target-halfword-index
			\ generate a conditional branch on inverse condition
			last-if-then-else-condition invert-condition-code chw,
		else
			." finish this"cr cr cr
			0 @
		then
		target-halfword-index 1-
		exit
	then
	\ use an armv7m cbz instruction
	0 pop{rX}
	\ remember the address for branch resolution by t-then
	target-halfword-index
	\ reserve space for the branch-on-zero instruction - save an armv7m 'cbz' instruction opcode
	%1011000100000000 chw,
	;

: t-if t-if-short-branch ;
\ IMMEDIATE	- not available
\ INVERT
: t-invert
\ 00000134 <do_invert_asm>:
\ 134:	bc01      	pop	{r0}
\ 136:	43c0      	mvns	r0, r0
\ 138:	b401      	push	{r0}

\ try to detect an immediately preceding if-then-else sequence, and if one is present
\ optimize the generated code
	?immediately-after-if-then-else-sequence if
		." optimizer: merging 'invert' with an immediately preceding if-then-else sequence"cr
		\ fetch the 'ite' generated instruction, and modify its condition field
		\ to use the inverse condition
		last-if-then-else-condition invert-condition-code dup to last-if-then-else-condition
		4 lshift %1011111100000100 or last-if-then-else-halfword-index thw!
	exit then
	0 pop{rX} $43c0 chw, 0 push{rX}
	;
\ J	- MUST BE DONE, EQUIVALENT TO t-r@
\ KEY	- not available
\ LITERAL	- not available
\ LOOP
: t-loop-stacked
\ 000000dc <do_loop_asm>:
\   dc:	cc01      	ldmia	r4!, {r0}
\   de:	4438      	add	r0, r7
\   e0:	6821      	ldr	r1, [r4, #0]
\   e2:	f844 0d04 	str.w	r0, [r4, #-4]!
\   e6:	4048      	eors	r0, r1
\   e8:	f47f aff8 	bne.w	dc <do_loop_asm>
$cc01 chw, $4438 chw, $6821 chw, $f844 chw, $0d04 chw, $4048 chw,
target-halfword-index swap cond-neq generate-conditional-branch chw,
\ 0000000c <do_2rdrop_asm>:
\    c:	3408      	adds	r4, #8
	$3408 chw,
	;

: t-loop-unstacked
\ 000000f0 <do_loop_unstacked_asm>:
\   f0:	44b8      	add	r8, r7
\   f2:	45c8      	cmp	r8, r9
\   f4:	d1f8      	bne.n	e8 <do_do_unstacked_asm>
\   f6:	e8b4 0300 	ldmia.w	r4!, {r8, r9}
	$44b8 chw, $45c8 chw,
\ generate branch
	target-halfword-index swap cond-neq generate-conditional-branch chw,
\ restore previous loop control indeces, if any
	$e8b4 chw, $0300 chw,
\ resolve any loop 'leave' targets
	cstack> 0 ?do cstack> t-then loop
	;
: t-loop t-loop-unstacked ;
\ +LOOP		[plus-loop]	- pending
: t-+loop
\ 1ea:   4480            add     r8, r0

\   f2:	45c8      	cmp	r8, r9
\   f4:	d1f8      	bne.n	e8 <do_do_unstacked_asm>
\   f6:	e8b4 0300 	ldmia.w	r4!, {r8, r9}
	0 pop{rX} $4480 chw,
\ generate branch
	target-halfword-index swap cond-lt generate-conditional-branch chw,
\ restore previous loop control indeces, if any
	$e8b4 chw, $0300 chw,
\ resolve any loop 'leave' targets
	cstack> 0 ?do cstack> t-then loop
	;
\ LSHIFT					[l-shift]
: t-lshift
\ 0000013a <do_lshift_asm>:
\ 13a:   bc03            pop     {r0, r1}
\ 13c:   fa01 f000       lsl.w   r0, r1, r0
\ 140:   b401            push    {r0}
	pop{r0,r1} $fa01 chw, $f000 chw, 0 push{rX}
	;
\ M*					[m-star]
: t-m*
\ 00000142 <do_m_star_asm>:
\ 142:	bc03      	pop	{r0, r1}
\ 144:	fb81 1000 	smull	r1, r0, r1, r0
\ 148:	b403      	push	{r0, r1}
	pop{r0,r1} $fb81 chw, $1000 chw, push{r0,r1}
	;
\ MAX
: t-max
\ 0000014a <do_max_asm>:
\ 14a:	bc03      	pop	{r0, r1}
\ 14c:	4288      	cmp	r0, r1
\ 14e:	bfb8      	it	lt
\ 150:	4608      	movlt	r0, r1
\ 152:	b401      	push	{r0}
	pop{r0,r1} $4288 chw, cond-lt it $4608 chw, 0 push{rX}
	;
\ MIN
: t-min
\ 00000154 <do_min_asm>:
\ 154:	bc03      	pop	{r0, r1}
\ 156:	4288      	cmp	r0, r1
\ 158:	bfc8      	it	gt
\ 15a:	4608      	movgt	r0, r1
\ 15c:	b401      	push	{r0}
	pop{r0,r1} $4288 chw, cond-gt it $4608 chw, 0 push{rX}
	;
\ MOD	- MUST BE DONE
: t-mod
\ 000001ec <do_mod_asm>:
\ 1ec:   bc03            pop     {r0, r1}
\ 1ee:   fb91 f2f0       sdiv    r2, r1, r0
\ 1f2:   4351            muls    r1, r2
\ 1f4:   1a40            subs    r0, r0, r1
	pop{r0,r1} $fb91 chw, $f2f0 chw, $4351 chw, $1a40 chw, 0 push{rX}
	;
\ MOVE
: t-move
\ todo - optimize this to move words
\ 0000021c <do_move_asm>:
\ 21c:   bc07            pop     {r0, r1, r2}
\ 21e:   b128            cbz     r0, 22c <do_fill_asm>
\ 220:   f812 4b01       ldrb.w  r4, [r2], #1
\ 224:   f801 4b01       strb.w  r4, [r1], #1
\ 228:   3801            subs    r0, #1
\ 22a:   d1f9            bne.n   220 <do_move_asm+0x4>
	pop{r0,r1,r2} $b128 chw, $f812 chw, $4b01 chw, $f801 chw, $4b01 chw, $3801 chw, $d1f9 chw, ;

\ NEGATE
: t-negate
\ 0000015e <do_negate_asm>:
\ 15e:	bc01      	pop	{r0}
\ 160:	4240      	negs	r0, r0
\ 162:	b401      	push	{r0}
	0 pop{rX} $4240 chw, 0 push{rX}
	;
\ OR
: t-or
\ 00000164 <do_or_asm>:
\ 164:	4308      	orrs	r0, r1
	pop{r0,r1} $4308 chw, 0 push{rX}
	;
\ OVER
: t-over
\ 00000166 <do_over_asm>:
\ 166:	9801      	ldr	r0, [sp, #4]
\ 168:	b401      	push	{r0}
	$9801 chw, 0 push{rX}
	;
\ POSTPONE	- not available
\ QUIT	- not available
\ R>					[r-from]
: t-r>
\ 0000005c <do_r_from_asm>:
\ 5c:   cc01            ldmia   r4!, {r0}
\ 5e:   b401            push    {r0}
	$cc01 chw, 0 push{rX}
	;
\ R@					[r-fetch]
: t-r@
\ 00000060 <do_r_fetch_asm>:
\ 60:   6820            ldr     r0, [r4, #0]
\ 62:   b401            push    {r0}
	$6820 chw, 0 push{rX}
	;
\ RECURSE	- MUST BE DONE
: t-recurse
	latest @ >body @ target-halfword-index generate-branch-with-link cw,
	;
\ REPEAT
: t-repeat
	\ generate uncoditional branch to the loop start denoted by t-begin
	target-halfword-index rot generate-unconditional-branch chw,
	\ resolve the conditional branch for t-while
	dup target-halfword-index cond-eq generate-conditional-branch swap thw!
	;
\ ROT					[rote]
: t-rot
\ 0000001c <do_rot_asm>:
\ 1c:   bc07            pop     {r0, r1, r2}
\ 1e:   b403            push    {r0, r1}
\ 20:   b404            push    {r2}
	$bc07 chw, push{r0,r1} 2 push{rX}
	;

\ RSHIFT					[r-shift]
: t-rshift
\ 0000016a <do_rshift_asm>:
\ 16a:   bc03            pop     {r0, r1}
\ 16c:   fa21 f000       lsr.w   r0, r1, r0
\ 170:   b401            push    {r0}
	pop{r0,r1} $fa21 chw, $f000 chw, 0 push{rX}
	;
: string-building-helper ( -- compiled-string-length)
				\ parses a '"' delimited string, and compiles code that, at runtime,
				\ will generate in the link-register a pointer to the beginning of the
				\ string; if this pointer is decremented, it will point to the
				\ counted-string representation of the string

	\ reserve space for a forward branch-and-link instruction
	target-halfword-index $deadbeef cw,
	[char] " parse
	\ store character literals into target core
	>r r@ dup target-halfword-size / 0
	?do
		0 target-halfword-size 0 do
			i 0= j 0= and if
				\ special case - put the length in the very first byte of the string
				drop $ff and
			else
				swap count rot swap 8 i * lshift or
			then
		loop
		thw,
	loop
	r@ 1 and 0<> if count else 0 then thw, drop
	target-halfword-index swap dup >r
	generate-branch-with-link r> tw!
	r>
	;
\ S"					[s-quote]
: t-s"
	string-building-helper
	t-literal
\ b501            push    {r0, lr}
	0 pop{rX} $b501 chw,
	;

\ S>D					[s-to-d]	- MUST BE DONE
\ SIGN	- not available
\ SM/REM					[s-m-slash-rem]	- MUST BE DONE
\ SOURCE	- not available
\ SPACE	- not available
\ SPACES	- not available
\ STATE	- not available
\ SWAP
: t-swap
	pop{r0,r1} 0 push{rX} 1 push{rX}
	;
\ TYPE	- not available
\ U.					[u-dot]	- not available
\ U<					[u-less-than]
: t-u<
\ 000000ce <do_unsigned_less_than_asm>:
\   ce:	bc03      	pop	{r0, r1}
\   d0:	4288      	cmp	r0, r1
\   d2:	bf94      	ite	ls
\   d4:	b440      	pushls	{r6}
\   d6:	b420      	pushhi	{r5}
pop{r0,r1} $4288 chw, cond-ls ite literal-false-register push{rX} literal-true-register push{rX}
	;
\ UM*					[u-m-star]	- MUST BE DONE
\ UM/MOD					[u-m-slash-mod]	- MUST BE DONE
\ UNLOOP
: t-unloop
\ 24e:   e8b4 0300       ldmia.w r4!, {r8, r9}
	$e8b4 chw, $0300 chw,
	;
\ LEAVE
: t-leave
	t-unloop ( compile 'ahead') target-halfword-index cond-al chw, cstack> 1+ swap ." XXX ".s >cstack >cstack
	\ t-unloop ( compile 'ahead') target-halfword-index drop cond-al chw, cstack> >cstack
	;
\ UNTIL
: t-until
	0 pop{rX}
	orrs(r0,r0)
	target-halfword-index swap cond-eq generate-conditional-branch chw,
	;

\ VARIABLE
: t-variable
	create target-variable-core-pool-index ,
	target-variable-core-pool-index 1+ to target-variable-core-pool-index
	does> @ target-word-size * generate-data-pool-address-in-r0 0 push{rX}
	;
\ WHILE
: t-while
	0 pop{rX}
	orrs(r0,r0)
	target-halfword-index
	\ save space for the branch instruction to be resolved by t-repeat
	0 chw,
	;
\ WORD	- not available
\ XOR					[x-or]
: t-xor
\ 00000172 <do_xor_asm>:
\ 172:   bc03            pop     {r0, r1}
\ 174:   4048            eors    r0, r1
\ 176:   b401            push    {r0}
	pop{r0,r1} $4048 chw, 0 push{rX}
	;
\ [[left-bracket]	- not available
\ ['][bracket-tick]
: t-[']
	' >body @ target-halfword-size * ( set the thumb bit) 1 or generate-literal-in-r0 0 push{rX} ;
\ [CHAR]				[bracket-char]	- MUST BE DONE
\ ]					[right-bracket]		- not available
\ .(					[dot-paren]	- natively supported
\ .R					[dot-r]		- not available
\ 0<>					[zero-not-equals]
: t-0<>
\ 00000080 <do_zero_not_equals_asm>:
\   80:	bc01      	pop	{r0}
\   82:	4300      	orrs	r0, r0
\   84:	bf14      	ite	ne
\   86:	b420      	pushne	{r5}
\   88:	b440      	pusheq	{r6}
	0 pop{rX} $4300 chw, cond-neq ite push(literal-true) push(literal-false)
	;
\ 0>					[zero-greater]
: t-0>
\ 00000098 <do_zero_greater>:
\   98:	bc01      	pop	{r0}
\   9a:	42b0      	cmp	r0, r6
\   9c:	bfcc      	ite	gt
\   9e:	b420      	pushgt	{r5}
\   a0:	b440      	pushle	{r6}
0 pop{rX} $42b0 chw, cond-gt ite literal-true-register push{rX} literal-false-register push{rX}
	;
\ 2>R					[two-to-r]
: t-2>r
\ 00000178 <do_two_to_r_asm>:
\ 178:   bc03            pop     {r0, r1}
\ 17a:   e924 0003       stmdb   r4!, {r0, r1}
	pop{r0,r1} $e924 chw, $0003 chw,
	;
\ 2R>					[two->-from]
: t-2r>
\ 0000017e <do_two_r_from_asm>:
\ 17e:   cc03            ldmia   r4!, {r0, r1}
\ 180:   b403            push    {r0, r1}
	$cc03 chw, push{r0,r1}
	;
\ 2R@					[two-r-fetch]
: t-2r@
\ 00000182 <do_two_r_fetch_asm>:
\ 182:   e894 0003       ldmia.w r4, {r0, r1}
\ 186:   b403            push    {r0, r1}
	$e894 chw, $0003 chw, push{r0,r1}
	;
\ :NONAME					[colon-no-name]	- not available
\ <>					[not-equals]
: t-<>
\ 000000a8 <do_not_equals_asm>:
\   a8:	bc03      	pop	{r0, r1}
\   aa:	4048      	eors	r0, r1
\   ac:	bf14      	ite	ne
\   ae:	b420      	pushne	{r5}
\   b0:	b440      	pusheq	{r6}
pop{r0,r1} $4048 chw, cond-neq ite literal-true-register push{rX} literal-false-register push{rX}
	;
\ ?DO					[question-do]	- MUST BE DONE
: t-?do
	t-2dup t-= t-if t-2drop ( compile 'ahead' here) target-halfword-index cond-al chw, swap t-then
	t-do ( amend t-do) cstack> drop swap >cstack 1 >cstack
	;
\ ACTION-OF					[][X:deferred]	- not available
\ AGAIN
: t-again
	target-halfword-index swap generate-unconditional-branch chw,
	;

\ NIP
: t-nip
\ 00000018 <do_nip_asm>:
\   18:	bc03      	pop	{r0, r1}
\   1a:	b401      	push	{r0}
	pop{r0,r1} 0 push{rX} ;

\ TUCK
: t-tuck
\ 00000012 <do_tuck_asm>:
\   12:	bc03      	pop	{r0, r1}
\   14:	b401      	push	{r0}
\   16:	b403      	push	{r0, r1}
	pop{r0,r1} 0 push{rX} push{r0,r1} ;
\ U>
: t-u>
\ 000000d8 <do_unsigned_greater_than_asm>:
\   d8:	bc03      	pop	{r0, r1}
\   da:	4281      	cmp	r1, r0
\   dc:	bf8c      	ite	hi
\   de:	b420      	pushhi	{r5}
\   e0:	b440      	pushls	{r6}
pop{r0,r1} $4281 chw, cond-hi ite push(literal-true) push(literal-false)
	;

\ FILL
: t-fill ( runtime: c-addr u-count value --)
\ 0000022c <do_fill_asm>:
\ 22c:   bc07            pop     {r0, r1, r2}
\ 22e:   b119            cbz     r1, 238 <do_call_thru_r0>
\ 230:   f802 0b01       strb.w  r0, [r2], #1
\ 234:   3901            subs    r1, #1
\ 236:   d1fb            bne.n   230 <do_fill_asm+0x4>
	\ t-rot t-rot 0 t-l t-?do t-2dup t-! t-1+ t-loop t-2drop
	pop{r0,r1,r2} $b119 chw, $f802 chw, $0b01 chw, $3901 chw, $d1fb chw,
	;
\ ERASE
: t-erase ( runtinme: c-addr u-count --)
	0 t-l t-fill ;
\ C"					[c-quote]
: t-c"
\ f1be 0e01       subs.w  lr, lr, #1
	string-building-helper
	$f1be chw, $0e01 chw,
	0 push{rX}
	;
\ VALUE
: t-value
	;
0 [if]
BUFFER:					[buffer-colon][x:buffer]
CASE
COMPILE,					[compile-comma]
DEFER					[][X:deferred]
DEFER!					[defer-store][X:deferred]
DEFER@					[defer-fetch][X:deferred]
ENDCASE					[end-case]
ENDOF					[end-of]
FALSE
HEX
HOLDS					[][x:xchar]
IS					[][X:deferred]
MARKER
OF
PAD
PARSE
PARSE-NAME					[][X:parse-name]
PICK
REFILL
RESTORE-INPUT
ROLL
S\bs"					[s-backslash-quote][X:escaped-strings]
SAVE-INPUT
SOURCE-ID					[source-i-d]
TO
TRUE
U.R					[u-dot-r]
UNUSED
WITHIN
					[COMPILE][bracket-compile]
\bs					[backslash]
[then]

: dump-target-core ( --)
	target-core target-halfword-index target-halfword-size * hdump ;

: generate-sforth-loader ( --)
	base @ hex
	cr cr ." ( vx/invader loader code)"cr ." base @ hex"cr
	target-halfword-index 1+ 2/
	0 do i 2* tw@ u. target-code-base-address i target-word-size * + u. ." t! " i 3 and 0= if cr then loop cr ." base !" cr cr
	base !
	;
: generate-vx-bare-loader ( --)
	base @ hex
	target-halfword-index 1+ 2/
	0 do i 2* tw@ u. target-code-base-address i target-word-size * + u. ." ! " i 3 and 0= if cr then loop cr
	." 20000001 jump"cr
	base !
	;
: >absolute-address ( local-core-relative-address -- relocated-target-absolute-address)
	target-code-base-address +
	;

0 value native-head
0 value cross-head
latest @ to cross-head

.( 'dump-target-core' dumps the generated target machine code to file out.bin) cr
.( switching main sforth dictionary to the cross-forth dictionary) cr cr
.( 'generate-sforth-loader' outputs forth code suitable for execution by a vx/invader-compatible)cr
.( probe in order to load the generated machine code to target)cr cr


: >cross latest @ to native-head cross-head latest ! ; immediate
\ manually switch to the cross dictionary
latest @ to native-head
cross-head latest !
: >native latest @ to cross-head native-head latest ! ; immediate

.( generating target runtime initialization sequence...)cr cr
\ <target_runtime_initialization>:
\ f8df d028       ldr.w   sp, [pc, #40]   ; 1ec <data_stack_pointer>
\ f000 f800       bl      1c8 <target_runtime_initialization+0x8>
\ f1be 0e01       subs.w  lr, lr, #1
\ e89e 8cf3       ldmia.w lr, {r0, r1, r4, r5, r6, r7, sl, fp, pc}
\ <return_stack_pointer>:
\ 00000000        .word   0x00000000
\ <literal_minus_one>:
\ ffffffff        .word   0xffffffff
\ <literal_zero>:
\ 00000000        .word   0x00000000
\ <literal_one>:
\ 00000001        .word   0x00000001
\ <zeroed_data_pool_base_address>:
\ 00000000        .word   0x00000000
\ <initialized_data_pool_base_address>:
\ 00000000        .word   0x00000000
\ <startup_vector>:
\ 00000000        .word   0x00000000
\ <data_stack_pointer>:
\ 00000000        .word   0x00000000
$f8df thw, $d028 thw, $f000 thw, $f800 thw, $f1be thw, $0e01 thw, $e89e thw, $8cf3 thw,

: >return-stack-init-value ( return-stack-value-on-startup --)
	[ target-halfword-index literal ] tw!
	;
\ reserve space for the return stack initialization value
0 tw,
\ store literal -1
-1 tw,
\ store literal 0
0 tw,
\ store literal 1
1 tw,
: >zeroed-data-pool-init-value ( zeroed-data-pool-base-address-value-on-startup --)
	[ target-halfword-index literal ] tw!
	;
\ reserve space for the zeroed-data pool
0 tw,
: >initialized-data-pool-init-value ( initialized-data-pool-base-address-value-on-startup --)
	[ target-halfword-index literal ] tw!
	;
\ reserve space for the initialized-data pool
0 tw,
: >startup-vector ( reset-xt --)
	\ updates the address of the word to execute after
	\ the initial startup runtime initialization
	[ target-halfword-index literal ] tw!
	;
\ reserve space for the startup vector
target-halfword-index target-halfword-size * to target-initial-data-stack-pointer-value-offset
0 tw,
: >data-stack-init-value ( data-stack-value-on-startup --)
	[ target-halfword-index literal ] tw!
	;
\ reserve space for the data stack initialization value
0 tw,

.( you are now using the cross-dictionary, use the >cross and >native words to switch between dictionaries)cr cr

: - t-- ;
: + t-+ ;
: * t-* ;
: begin t-begin ;
: again t-again ;
: 1+ t-1+ ;
: over t-over ;
: dup t-dup ;
: constant t-constant ;

\ -------------------------------------------------
true [if]
: swap t-swap ;
: cells t-cells ;
: @ t-@ ;
: ! t-! ;
: < t-< ;
: invert t-invert ;
: if t-if ;
: else t-else ;
: then t-then ;
: >r t->r ;
: r> t-r> ;
: lshift t-lshift ;
: and t-and ;
: or t-or ;
: rot t-rot ;
[then]
\ -------------------------------------------------

: n; postpone ; ; immediate
: n: : ;
: ; t; ;
: : t: n;

0 [if]

include armv7m-scb-registers.fs
include stm32f103.fs

: main
	0 bit 2 bit 3 bit >native or or >cross t-l apb2enr !
	1 t-l 11 t-l
	pin-mode-output-2-mhz
	pin-cfg-output-push-pull 2 t-l t-lshift t-or
	configure-port-pin
	port-b-base gpio-x-srr
	begin
		11 bit t-l over ! >native 11 16 + bit >cross t-l over !
	again
	;

include vx-utils.fs


t-s" abcdefgh"
t-?dup
t-depth
: cbz-test t-0= t-if 12 t-l t-else 13 t-l t-then ;
: until-test t-begin 12 t-l 13 t-l t-- t-0<> t-until ;
: do-test 10 t-l 0 t-l t-?do t-leave t-leave t-drop true t-l t-if t-leave t-then t-loop ;
: +loop-test
	10 t-l 0 t-l t-do 2 t-l t-leave t-+loop
	;
: fill-test t-fill ;
: erase-test t-erase ;

t-c" abcde"

[then]

: main ;
t-variable var0
t-variable var1
t-variable var2


: runtime-initialization
	\ initialize .bss section - zero-initialized data
	zeroed-data-pool-base-address t-l target-variable-core-pool-index t-l t-cells t-erase
	main
	;

$aaaa tw,
: var-test var2 @ var0 @ var1 @ t-* t-+ ;
: led-test 
	$aabbccdd t-l 1000000 t-l 0 t-l t-do 0 t-l over t-! 1 t-l t-over t-! t-loop t-drop
	;
: var-test $aabbccdd t-l @ $f00 t-l invert and $aabbccdd t-l ! ;
: var-test var2 @ 10 bit t-l invert and var2 ! ;
: var-test var2 t-@ $aabbccdd t-l t-< t-if 1234 t-l var2 t-! t-then ;
: var-test var2 t-@ $aabbccdd t-l t-< t-if 0 t-l var2 t-! t-then ;
: var-test var1 t-@ var2 t-! ;
1 [if]
: var-test 10 t-l var2 t-+! ;
: var-test var2 @ 10 t-+ var2 ! ;
: var-test var2 @ var1 @ + var2 ! ;

: var-test var2 var1 @ swap + var2 ! ;
: var-test var2 @ t-bit var2 ! ;
[then]


zeroed-data-pool-base-address >zeroed-data-pool-init-value
initialized-data-pool-base-address >initialized-data-pool-init-value
data-stack-base-address >data-stack-init-value
return-stack-base-address >return-stack-init-value
t-' runtime-initialization >absolute-address >startup-vector

dump-target-core
generate-vx-bare-loader

decimal >native

here swap - .( cross-sforth kernel memory usage: ) . .( bytes)cr
sf-reset
bye

