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
17.6
Glossary
17.6.1
String words
*/


/*
17.6.1.0170 -TRAILING
“dash-trailing”
STRING
( c-addr u1-- c-addr u2)
If u1 is greater than zero, u2 is equal to u1 less the number of spaces at the end of the
character string specified by c-addr u1. If u1 is zero or the entire string consists of spaces,
u2 is zero.

*/
static void do_dash_trailing(void)
{ cell u = sf_pop(); uint8_t * s = (uint8_t *) sf_top() + u; while (u) if (* -- s != ' ') break; else u --; sf_push(u); }

/*
17.6.1.0245 /STRING
“slash-string”
STRING
( c-addr1 u1 n -- c-addr2 u2)
Adjust the character string at c-addr1 by n characters. The resulting character string,
specified by c-addr2 u2, begins at c-addr1 plus n characters and is u1 minus n characters
long.
See: A.17.6.1.0245 /STRING.
*/
static void do_slash_string(void)
{ scell n = sf_pop(); cell u = sf_pop(); uint8_t * p = (uint8_t *) sf_pop(); sf_push((cell)(p + n)); sf_push(u - n); }

/*
17.6.1.0780 BLANK
STRING
( c-addr u -- )
If u is greater than zero, store the character value for space in u consecutive character
positions beginning at c-addr.
*/
static void do_blank(void) { cell u = sf_pop(); xmemset((uint8_t *) sf_pop(), ' ', u); }

/*
17.6.1.0910 CMOVE
“c-move”
STRING
( c-addr1 c-addr2 u -- )
If u is greater than zero, copy u consecutive characters from the data space starting at c-
addr1 to that starting at c-addr2, proceeding character-by-character from lower addresses
to higher addresses.
See: 17.6.1.0920 CMOVE>, A.17.6.1.0910 CMOVE.
*/
static void do_cmove(void) { cell u = sf_pop(); uint8_t * dest = (uint8_t *) sf_pop(), * src = (uint8_t *) sf_pop(); xmemcpy(dest, src, u); }

/*
17.6.1.0920 CMOVE>
“c-move-up”
STRING
( c-addr1 c-addr2 u -- )
If u is greater than zero, copy u consecutive characters from the data space starting at
c-addr1 to that starting at c-addr2, proceeding character-by-character from higher ad-
dresses to lower addresses.
See: 17.6.1.0910 CMOVE, A.17.6.1.0920 CMOVE>.
*/
static void do_cmove_up(void) { cell u = sf_pop(); uint8_t * dest = (uint8_t *) sf_pop() + u, * src = (uint8_t *) sf_pop() + u; while (u --) * -- dest = * -- src; }


/*
17.6.1.0935 COMPARE
STRING
( c-addr1 u1 c-addr2 u2 -- n )
Compare the string specified by c-addr1 u1 to the string specified by c-addr2 u2. The
strings are compared, beginning at the given addresses, character by character, up to the
length of the shorter string or until a difference is found. If the two strings are identical,
n is zero. If the two strings are identical up to the length of the shorter string, n is minus-
one (-1) if u1 is less than u2 and one (1) otherwise. If the two strings are not identical up
to the length of the shorter string, n is minus-one (-1) if the first non-matching character
in the string specified by c-addr1 u1 has a lesser numeric value than the corresponding
character in the string specified by c-addr2 u2 and one (1) otherwise.
See: A.17.6.1.0935 COMPARE.
*/
static void do_compare(void)
{ cell u1, u2, x; int res; uint8_t * s1, * s2; u2 = sf_pop(); s2 = (uint8_t *) sf_pop(); u1 = sf_pop(); s1 = (uint8_t *) sf_pop(); if (u1 == u2) { sf_push(xstrncmp((void *) s1, (void *) s2, u1)); return; } x = (u1 < u2) ? u1 : u2; u1 -=x; u2 -= x; res = xstrncmp((void *) s1, (void *) s2, x); if (res) sf_push(res); else sf_push(u1 ? 1 : -1); }

/*
17.6.1.2191 SEARCH
STRING
( c-addr1 u1 c-addr2 u2 -- c-addr3 u3 flag )
Search the string specified by c-addr1 u1 for the string specified by c-addr2 u2. If flag is
true, a match was found at c-addr3 with u3 characters remaining. If flag is false there was
no match and c-addr3 is c-addr1 and u3 is u1.
See: A.17.6.1.2191 SEARCH.
*/
static void do_search(void)
{ cell u1, u2, f; int i; uint8_t * s1, * s2; u2 = sf_pop(); s2 = (uint8_t *) sf_pop(); u1 = sf_pop(); s1 = (uint8_t *) sf_pop(); f = C_FALSE; for (i = 0; i <= u1 - u2; i ++) if (!xstrncmp((void *) (s1 + i), (void *) s2, u2)) { s1 += i; u1 -= i; f = C_TRUE; break; } sf_push((cell) s1); sf_push(u1); sf_push(f); }

/*
17.6.1.2212 SLITERAL
STRING
Interpretation: Interpretation semantics for this word are undefined.
Compilation: ( c-addr1 u -- )
Append the run-time semantics given below to the current definition.
Run-time: ( -- c-addr2 u )
Return c-addr2u describing a string consisting of the characters specified by c-addr1 u
during compilation. A program shall not alter the returned string.
See: A.17.6.112.0 SLITERAL.
*/
static void do_sliteral(void)
{
/*
: sliteral ( c-addr u -- )
	postpone ahead rot rot
	( save a pointer to the string at runtime)
	here >r
	( copy string to data space)
	2dup here swap cmove
	dup allot align
	rot postpone then
	( compile runtime string address)
	r> postpone literal
	( compile runtime string length)
	postpone literal
	drop ( c-addr)
	; immediate

: str s" sample string" ;
str type cr cr
: stest [ str ] sliteral ;
: stest [ str ] sliteral 1- swap 1+ swap ." string is:" cr cr type cr cr ;

 */
do_ahead(); do_rot(); do_rot();
do_here(); do_to_r();
do_two_dup(); do_here(); do_swap(); do_cmove();
do_dup(); do_allot(); do_align();
do_rot(); do_then();
do_r_from(); do_literal();
do_literal();
do_drop();

}

/*
17.6.2
String extension words
17.6.2.2141 REPLACES
STRING EXT
x:substitute
( c-addr1u1c-addr2u2-- )
Set the string c-addr1u1as the text to substitute for the substitution named by c-addr2
u2. If the substitution does not exist it is created. The program may then reuse the buffer
c-addr1u1without affecting the definition of the substitution.
Ambiguous conditions occur as follows:
– The substitution cannot be created.
– The name of a substitution contains the ‘%’ delimiter character.
! “ # $ % & ’ ( ) * + , - . / digits : ; < = > ? @ ALPHA [ \ ] ? _ ‘ alpha { | } ~
135
17. STRING Word Set
Forth 200x / RC0
REPLACES may allot data space and create a definition. This breaks the contiguity of
the current region and is not allowed during compilation of a colon definition
See: 3.3.3.2 Contiguous regions, 3.4.5 Compilation, 17.6.2.2255 SUBSTITUTE.
17.6.2.2255 SUBSTITUTE
STRING EXT
x:substitute
( c-addr1u1c-addr2u2-- c-addr2u3n )
Perform substitution on the string c-addr1u1placing the result at string c-addr2u2, re-
turning c-addr2and u3, the length of the resulting string. An ambiguous condition occurs
if the resulting string will not fit into c-addr2u2or if c-addr2is the same as c-addr1. The
return value n is positive on success and indicates the number of substitutions made.
A negative value for n indicates that an error occurred, leaving c-addr2u3undefined.
Substitution occurs from the start of c-addr1in one pass and is non-recursive.
When a substitution name, surrounded by ‘%’ (ASCII $25) delimiters is encountered by
SUBSTITUTE, the following occurs:
a) If the name is null, a single delimiter character is substituted, i.e., %% is replaced
by %.
b) Ifthenameisavalidsubstitutionname, theleadingandtrailingdelimitercharacters
and the enclosed substitution name are replaced by the substitution text.
c) If the name is not a valid substitution name, the name with leading and trailing
delimiters is passed unchanged to the output.
See: 17.6.2.2141 REPLACES, 17.6.2.2375 UNESCAPE, A.17.6.255.0 SUBSTITUTE.
17.6.2.2375 UNESCAPE
STRING EXT
x:substitute
( c-addr1u1c-addr2-- c-addr2u2)
Replace each ‘%’ character in the input string c-addr1u1by two ‘%’ characters. The
output is represented by c-addr2u2. The buffer at c-addr2must be big enough to hold
the unescaped string. An ambiguous condition occurs if the resulting string will not fit
into the destination buffer (c-addr2).
See: 17.6.2.2255 SUBSTITUTE.

*/

#include "sf-word-wizard.h"
static struct word dict_base_dummy_word[1] = { MKWORD(0, 0, "", 0), };
static const struct word custom_dict[] = {
	/* override the sforth supplied engine reset */
	MKWORD(dict_base_dummy_word,	0,		"-trailing",	do_dash_trailing),
	MKWORD(custom_dict,		__COUNTER__,	"/string",	do_slash_string),
	MKWORD(custom_dict,		__COUNTER__,	"blank",	do_blank),
	MKWORD(custom_dict,		__COUNTER__,	"cmove",	do_cmove),
	MKWORD(custom_dict,		__COUNTER__,	"cmove>",	do_cmove_up),
	MKWORD(custom_dict,		__COUNTER__,	"compare",	do_compare),
	MKWORD(custom_dict,		__COUNTER__,	"search",	do_search),
	MKIMMWORD(custom_dict,		__COUNTER__,	"sliteral",	do_sliteral),

}, * custom_dict_start = custom_dict + __COUNTER__;

static void sf_opt_string_init(void)
{
	sf_merge_custom_dictionary(dict_base_dummy_word, custom_dict_start);
}

static void (* const dictionary_initializer)(void) __attribute__((used)) __attribute__ ((section(".sf_init"))) = sf_opt_string_init;
