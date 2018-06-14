\ armv7m stack optimizer

\ the purpose of this code is to optimize commonly occurring armv7m stack
\ operation sequences by fusing/merging/discarding such detected sequences

\ central to the operation of the optimizer is the so-called
\ 'optimizer list' - this is a list of records which have the following format:
\ cell 0 - size of the machine code sequence to look for - in halfwords
\ cell 1 - execution token of the word which to execute when the machine
\		code sequence has been detected
\ halfwords[0; 2 * n] - the halfwords of the machine code sequence itself -
\		the number of halfwords equals the value of cell 0
\		of this record - times 2; there are 2 halfwords per 
\		target instruction halfword - the first halfword contains
\		a bitmask which to apply by 'and'-ing into the instruction
\		halfword fetched from target core before performing the
\		comparison, and the second halfword contains
\		the opcode value against which to compare; this scheme
\		allows the optimizer to perform more flexibly by allowing
\		it to ignore concrete literal values and register numbers
\		when performing optimizations
\
\ the records on the list are ordered from longest to shortest in size,
\ with the longest sequences appearing near the start of the list, and
\ the shortest appearing near the end of the list
\
\ the optimization list nodes themselves contain two cells (fields):
\ cell 0 - a pointer to the next node on the list; contains 0 if this is the last node on the list
\ cell 1 - a pointer to the optimization record

\ create an empty optimizer list
create optimizer-list-head 0 ,
\ helper words
: create-new-node ( optimization-record-node -- new-list-node)
	here ( reserve space for the 'next' pointer') 0 , swap ,
	;
: size@ ( optimization-list-note -- record-sequence-size-in-halfwords)
	cell+ @ @ ;
: >code-sequence ( optimization-list-note -- first-record-sequence-word)
	cell+ @ 2 cells +
	;
: >optimization-rule ( optimization-list-note -- optimization-rule-xt)
	cell+ @ cell+ @
	;
: link-after ( new-list-node optimization-list-node --)
	2dup @ swap ! ! ;
: >next-node ( optimization-list-node -- next-optimization-list-node)
	@ ;

: iterate-list ( xt-of-visitor-word --)
\ !!! imprortant: the stack diagram of the visitor word must be as such:
\ visitor-word ( optimization-list-node -- t=continue-scanning-list|f=stop-scanning-list)
	optimizer-list-head >next-node
	begin dup while
		over over swap execute false = if 2drop exit then >next-node
	repeat 2drop
	;
: >optimizer-list ( optimization-record-node -- )
\ inserts an optimization record into the optimizer list, keeps longest
\ machine code sequences first

	create-new-node
	\ 1 - if no next node, or next node size <= new node size - link after this one and exit
	\ 2 - skip to next node
	\ locate insertion point 
	dup size@
	optimizer-list-head >r
	begin
		r@ >next-node dup 0<> if size@ over > else ( this is the last node) drop false then
		while ( skip to next node) r> >next-node >r
	repeat drop
	r> link-after
	;

: ?sequence-matches ( optimization-list-node -- t=sequence matches | f=sequence mismatch)
	dup size@
	\ compute starting target core index for comparison
	target-halfword-index over -
	rot >code-sequence
	rot 0 do
	2dup 2@ rot thw@ and <> if 2drop false unloop exit then
	2 cells + swap 1+ swap
	loop 2drop true
	." optimizer: successfully matched a code sequence"cr
	;

: try-to-optimize ( optimization-list-node -- t=continue-scanning-list|f=stop-scanning-list)
	dup size@ compiled-halfwords-length @ .s > if drop true exit then
	dup ?sequence-matches invert swap over false = if >optimization-rule execute else drop then
	;
: run-optimizer
	['] try-to-optimize iterate-list
	;

\ various helper words
: ?single-bit-set ( n -- t=a-single-bit-is-set-in-n)
	dup dup 1- and 0= swap 0<> and
	;
: bitmap>number ( n -- bit-index)
	\ given a bitmap with a single bit set - returns the number of the bit set
	dup ?single-bit-set invert abort" bad bitmap passed to bitmap>number"
	1- 0 begin over while 1+ swap 2/ swap repeat nip
	;

0 [if]
( --------------------------------------------------------------------------------------------------)
( --------------------------------------------------------------------------------------------------)
( --------------------------------------------------------------------------------------------------)
             _   _           _              
            | | (_)         (_)             
  ___  _ __ | |_ _ _ __ ___  _ _______ _ __ 
 / _ \| '_ \| __| | '_ ` _ \| |_  / _ \ '__|
| (_) | |_) | |_| | | | | | | |/ /  __/ |   
 \___/| .__/ \__|_|_| |_| |_|_/___\___|_|   
      | |                                   
      |_|                                   
            _                               
           | |                              
 _ __ _   _| | ___  ___                     
| '__| | | | |/ _ \/ __|                    
| |  | |_| | |  __/\__ \                    
|_|   \__,_|_|\___||___/                    

( --------------------------------------------------------------------------------------------------)
( --------------------------------------------------------------------------------------------------)
( --------------------------------------------------------------------------------------------------)
[then]

\ b403            push    {r0, r1}
\ bc03            pop     {r0, r1}
:noname ." optimizer: cancelling redundant 'push{r0,r1}/pop{r0,r1}'"cr
	target-halfword-index 2 - to target-halfword-index
	-2 compiled-halfwords-length +!
	;
here 2 , swap , $ffff , $b403 , $ffff , $bc03 , >optimizer-list


\ rot rot
\ bc07            pop     {r0, r1, r2}
\ b403            push    {r0, r1}
\ b404            push    {r2}
\ bc07            pop     {r0, r1, r2}
\ b403            push    {r0, r1}
\ b404            push    {r2}
:noname ." optimizer: fusing 'rot rot'"cr
\ bc07            pop     {r0, r1, r2}
\ b401            push    {r0}
\ b406            push    {r1, r2}
target-halfword-index 6 - to target-halfword-index
	-6 compiled-halfwords-length +!
	$bc07 chw, $b401 chw, $b406 chw,
	;
here 6 , swap , $ffff , $bc07 , $ffff , $b403 , $ffff , $b404 , $ffff , $bc07 , $ffff , $b403 , $ffff , $b404 , >optimizer-list

\ b401      	push	{r0}
\ bc01      	pop	{r0}
:noname ." optimizer: cancelling redundant 'push{r0}/pop{r0}'"cr
	target-halfword-index 2 - to target-halfword-index
	-2 compiled-halfwords-length +!
	;
here 2 , swap , $ffff , $b401 , $ffff , $bc01 , >optimizer-list

\ swap swap
\ bc03      	pop	{r0, r1}
\ b401      	push	{r0}
\ b402      	push	{r1}
\ bc03      	pop	{r0, r1}
\ b401      	push	{r0}
\ b402      	push	{r1}
:noname ." optimizer: cancelling redundant 'swap swap'"cr
	target-halfword-index 6 - to target-halfword-index
	-6 compiled-halfwords-length +!
	;
here 6 , swap , $ffff , $bc03 , $ffff , $b401 , $ffff , $b402 , $ffff , $bc03 , $ffff , $b401 , $ffff , $b402 , >optimizer-list


\ bc01      	pop	{r0}
\ 4300      	orrs	r0, r0
\ d117      	bne.n	xxx	- cond-neq
:noname ." optimizer: optimizing branch on non-zero"cr
	target-halfword-index 2 - to target-halfword-index
	-2 compiled-halfwords-length +!
	\ adjust the branch source address for the jump instruction to be later resolved by t-then
	drop target-halfword-index
	\ compile a 'cbnz r0, ...' opcode
	%1011100100000000 chw,
	;
here 3 , swap , $ffff , $bc01 , $ffff , $4300 , $ffff , cond-neq , >optimizer-list


\ b401      	push	{r0}
\ bc03      	pop	{r0, r1}
:noname ." optimizer: fusing a 'push{r0}/pop{r0,r1}' sequence"cr
	target-halfword-index 2 - to target-halfword-index
	-2 compiled-halfwords-length +!
	1 pop{rX}
	;
here 2 , swap , $ffff , $b401 , $ffff , $bc03 , >optimizer-list

\ bc02      	pop	{r1}
\ b401      	push	{r0}
\ b402      	push	{r1}
\ bc03      	pop	{r0, r1}
:noname ." optimizer: optimizing register exchange"cr
	target-halfword-index 4 - to target-halfword-index
	-4 compiled-halfwords-length +!
\ 4601            mov     r1, r0
	$4601 chw,
	0 pop{rX}
	;
here 4 , swap , $ffff , $bc02 , $ffff , $b401 , $ffff , $b402 , $ffff , $bc03 , >optimizer-list

\ b401      	push	{r0}
\ bc07      	pop	{r0, r1, r2}
:noname ." optimizer: fusing 'push{r0}/pop{r0,r1,r2}'"cr
	target-halfword-index 2 - to target-halfword-index
	-2 compiled-halfwords-length +!
\ bc06            pop     {r1, r2}
	$bc06 chw,
	;
here 2 , swap , $ffff , $b401 , $ffff , $bc07 , >optimizer-list

\ 4308      	orrs	r0, r1
\ 4601      	mov	r1, r0
:noname ." optimizer: optimizing swap"cr
	target-halfword-index 2 - to target-halfword-index
	-2 compiled-halfwords-length +!
\ 4301            orrs    r1, r0
	$4301 chw,
	;
here 2 , swap , $ffff , $4308 , $ffff , $4601 , >optimizer-list


\ f200 0000 	addw	r0, rX, #xxxx
\ 6800      	ldr	rY, [r0, #0]
:noname ." optimizer: optimizing indexed data fetch"cr
	target-halfword-index 3 - to target-halfword-index
	-3 compiled-halfwords-length +!
	\ extract source register and the most significant bit of the immediate operand
	target-halfword-index thw@
	dup $f and >r 10 bit and 0<> 11 bit and
	\ extract the 11 least significant bits of the immediate operand and add the most significant bit
	target-halfword-index 1+ thw@
	dup 4 rshift $f00 and swap $ff and or or
	\ synthesize fetch instruction - ldr r0, [rn,#imm12]
	%11111000110100000000000000000000
	\ fuse-in the immediate field
	or
	\ fuse in the source register number
	r> 16 lshift or
	\ fetch and use the target register number
	target-halfword-index 2 + thw@ 7 and 12 lshift or
	swap-halfwords cw,
	;
here 3 , swap , %1111101111100000 , $f200 , $0f00 , $0000 , $fff8 , $6800 , >optimizer-list

\ f200 0000 	addw	r0, rX, #xxxx
\ 6001      	str	r1, [r0, #0]
:noname ." optimizer: optimizing indexed data store"cr
	target-halfword-index 3 - to target-halfword-index
	-3 compiled-halfwords-length +!
	\ extract source register and the most significant bit of the immediate operand
	target-halfword-index thw@
	dup $f and >r 10 bit and 0<> 11 bit and
	\ extract the 11 least significant bits of the immediate operand and add the most significant bit
	target-halfword-index 1+ thw@
	dup 4 rshift $f00 and swap $ff and or or
	\ synthesize store instruction - str r1, [rn,#imm12]
	%11111000110000000001000000000000
	\ fuse-in the immediate field
	or
	\ fuse in the source register number
	r> 16 lshift or
	swap-halfwords cw,
	;
here 3 , swap , $f200 , $f200 , $0f00 , $0000 , $ffff , $6001 , >optimizer-list

\ 4601            mov     r1, r0
\ f8ca 1008       str.w   r1, [rX, #8]
:noname ." optimizer: discarding redundant 'mov r1,r0'"cr
	target-halfword-index 3 - to target-halfword-index
	-3 compiled-halfwords-length +!
	\ extract base register and immediate offset value
	target-halfword-index 2 + thw@
	target-halfword-index 1+ thw@
	chw, ( hard-code register 0) $f000 invert and chw,
	;
here 3 , swap , $ffff , $4601 , $fff0 , $f8c0 , $f000 , $1000 , >optimizer-list

\ b401      	push	{rA}
\ f20a 0004 	addw	rB, rC, #xxxx
\ bc02      	pop	{rD}
:noname
	\ rB != rD and rC != rD
	\ extract rB, rC and rD register numbers
	target-halfword-index dup 3 - thw@ $f and over 2 - thw@ 8 rshift $f and rot
	1 - thw@ $ff and dup ?single-bit-set invert if 2drop drop exit then
	rot over = >r = r> or if ( rB = rD or rc = rD - do not optimize)exit then
	." optimizer: merging out-of-place 'push{rX}/pop{rY}'"cr
	target-halfword-index 1 - to target-halfword-index
	-1 compiled-halfwords-length +!
	\ synthesize a 'mov rD, rA' instruction
	\ retrieve rD and rA
	target-halfword-index dup thw@ $ff and bitmap>number swap 3 - thw@ $ff and bitmap>number
	3 lshift %0100011000000000 or or
	target-halfword-index 3 - thw!
	;
here 4 , swap , $ff00 , $b400 , $f200 , $f200 , $0000 , $0000 , $ff00 , $bc00 , >optimizer-list

false [if]
\ test drive

variable node-count 0 node-count !
: count-nodes ( optimization-list-node -- true) drop 1 node-count +! true ;
: dump-node ( optimization-list-node -- true)
	." optimization node sequence length " size@ . cr true
	;
: dump-nodes ['] dump-node iterate-list ;
	
: count-nodes
	0 node-count !
	['] count-nodes iterate-list
	." there are " node-count @ . ." nodes on the list" cr ;
	count-nodes dump-nodes
	here 10 , 0 , 10 cells allot >optimizer-list
	count-nodes dump-nodes
	here 9 , 0 , 10 cells allot >optimizer-list
	count-nodes dump-nodes
	here 11 , 0 , 10 cells allot >optimizer-list
	count-nodes dump-nodes
	here 10 , 0 , 10 cells allot >optimizer-list
	count-nodes dump-nodes

[then]
