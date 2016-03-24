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
#include "engine.h"

/*
15.6.2.2531 [ELSE]
“bracket-else”
TOOLS EXT
Compilation: Perform the execution semantics given below.
Execution: ( “hspacesiname ...” -- )
Skipping leading spaces, parse and discard space-delimited words from the parse area,
including nested occurrences of [IF] ...
[THEN] and [IF] ...
[ELSE] ...
[THEN], until the word [THEN] has been parsed and discarded. If the parse area be-
comes exhausted, it is refilled as with REFILL. [ELSE] is an immediate word.
See: 3.4.1 Parsing, A.15.6.2.2531 [ELSE].
15.6.2.2532 [IF]
“bracket-if”
TOOLS EXT
*/
static void do_bracket_else(void)
{
/*

: [else]
	1 ( initialize [if]/[else]/[then] nesting level)
	begin
	bl word count dup 
	0= ( refill input buffer) if drop drop refill 0= if ( refill failed) drop ( drop nesting level) ." error: input exhausted" cr exit then [ over ] ( continue loop) again then
	0 if 2dup ." found word: " type cr then
	2dup s" [if]" compare 0= if ( increase nesting level) rot 1+ rot rot then
	2dup s" [else]" compare 0= if ( special-case for the nesting level) rot dup 1 = if 1- then rot rot then
	s" [then]" compare 0= if ( decrease nesting level) 1- then
	dup 0= until
	drop ; immediate

: [if] 0= if postpone [else] then ; immediate

: [then] ; immediate
*/
	/* initialize [if]/[else]/[then] nesting level */
	sf_push(1);
	do
	{
		do_bl(); do_word(); do_count();
		if (!sf_top())
		{
			/* input exhausted, refill input buffer */
			do_drop(); do_drop(); do_refill();
			if (!sf_pop()) { /* 'refill' failed */ /* drop nesting level */ do_drop(); print_str(__func__);
				print_str("(): input exhausted; aborting\n"); do_abort(); }
			continue;
		}

		do_two_to_r();

		do_two_r_fetch();
		//sf_push((cell) "\04[if]"); do_count(); sf_push((cell) compare_word_xt); do_execute();
		if (sf_pop() == 4) { if (!xmemcmp((void *) sf_pop(), "[if]", 4)) /* increase nesting level */ do_one_plus(); }
		else do_drop();

		do_two_r_fetch();
		//sf_push((cell) "\06[else]"); do_count(); sf_push((cell) compare_word_xt); do_execute();
		if (sf_pop() == 6) { if (!xmemcmp((void *) sf_pop(), "[else]", 6)) /* see if an [if] block should be terminated */ if (sf_top() == 1) do_one_minus(); }
		else do_drop();

		do_two_r_from();
		//sf_push((cell) "\06[then]"); do_count(); sf_push((cell) compare_word_xt); do_execute();
		if (sf_pop() == 6) { if (!xmemcmp((void *) sf_pop(), "[then]", 6)) /* decrease nesting level */ do_one_minus(); }
		else do_drop();
	}
	while (sf_top());
	/* drop nesting level */
	do_drop();

}


/*
15.6.2.2532 [IF]
“bracket-if”
TOOLS EXT
Compilation: Perform the execution semantics given below.
Execution: ( flag | flag “hspacesiname ...” -- )
If flag is true, do nothing. Otherwise, skipping leading spaces, parse and discard space-
delimited words from the parse area, including nested occurrences of [IF] ... [THEN]
and [IF] ...
[ELSE] ...
[THEN], until either the word [ELSE] or the word
[THEN] has been parsed and discarded. If the parse area becomes exhausted, it is
refilled as with REFILL. [IF] is an immediate word.
An ambiguous condition exists if [IF] is POSTPONEd, or if the end of the input buffer
is reached and cannot be refilled before the terminating [ELSE] or [THEN] is parsed.
See: 3.4.1 Parsing, A.15.6.2.2532 [IF].

*/
static void do_bracket_if(void) { /*: [if] 0= if postpone [else] then ; immediate */ if (!sf_pop()) do_bracket_else(); }

/*
15.6.2.2533 [THEN]
“bracket-then”
TOOLS EXT
Compilation: Perform the execution semantics given below.
Execution: ( -- )
Does nothing. [THEN] is an immediate word.
See: A.15.6.2.2533 [THEN].

*/
static void do_bracket_then(void) { /*: [then] ; immediate */ }
static void do_bracket_defined(void)
{
	/*: [defined] bl word find swap drop 0<> ; immediate */
	do_bl(); do_word(); do_find(); do_swap(); do_drop(); do_zero_not_equals();
}
static void do_bracket_undefined(void) { /* : [undefined] [defined] invert ; immediate */ do_bracket_defined(); do_invert(); }


#include "sf-word-wizard.h"
static struct word dict_base_dummy_word[1] = { MKWORD(0, 0, "", 0), };
static const struct word custom_dict[] = {
	/* override the sforth supplied engine reset */
	MKWORD(dict_base_dummy_word,	0,		"[else]",	do_bracket_else),
	MKWORD(custom_dict,		__COUNTER__,	"[if]",		do_bracket_if),
	MKWORD(custom_dict,		__COUNTER__,	"[then]",	do_bracket_then),
	MKWORD(custom_dict,		__COUNTER__,	"[defined]",	do_bracket_defined),
	MKWORD(custom_dict,		__COUNTER__,	"[undefined]",	do_bracket_undefined),

}, * custom_dict_start = custom_dict + __COUNTER__;

static void sf_opt_prog_tools_init(void)
{
	sf_merge_custom_dictionary(dict_base_dummy_word, custom_dict_start);
}

static void (* const dictionary_initializer)(void) __attribute__((used)) __attribute__ ((section(".sf_init"))) = sf_opt_prog_tools_init;
