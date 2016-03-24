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

#ifndef __CORE_WORDS_H__
#define __CORE_WORDS_H__

#include "sf-cfg.h"
#include "sf-arch.h"

/*! sforth engine constants */
enum
{
	/*! the size of the sforth engine core memory arena, in words */
	SFORTH_CORE_SIZE_IN_CELLS	= CORE_CELLS_COUNT,
	/*! the size of the data stack, in cells */
	DSTACK_SIZE		= STACK_DEPTH,
	/*! the size of the return stack, in cells */
	RSTACK_SIZE		= STACK_DEPTH,
	/*! pictured numeric output data buffer size, in characters */
	PNOBUF_SIZE		= 2 * 2 * 32 + 2,
	/*! input data buffer size, in characters */
	INBUF_SIZE		= 256,
	/*! size of the wordpad array, in characters */
	WORDPAD_SIZE		= INBUF_SIZE,
	/*! watermark level used by the 'refill' routine when repopulating the input buffer
	 *
	 * see the code of the refill routine for the usage of this */
	REFILL_WATERMARK_LEVEL	= (INBUF_SIZE * 3) / 4,
	/*! constant used for signifying a 'true' condition */
	C_TRUE			= -1,
	/*! constant used for signifying a 'false' condition */
	C_FALSE			= 0,
	/*! minimum allowed value for 'base' (inclusive) */
	MIN_BASE		= 2,
	/*! maximum allowed value for 'base' (inclusive) */
	MAX_BASE		= 36,
	/*! number of bits in a machine cell */
	CELL_NR_BITS		= sizeof (cell) * 8,
	/*! maximum filenamename length of a file that can be 'included' */
	MAX_FNAME_LEN		= 32,
	/*! maximum file inclusion depth for the 'included' word */
	MAX_FILE_INCLUSION_DEPTH	= 5,
};


/*! this describes a counted string */
union cstr
{
	struct
	{
		/* size of the sdata array below */
		uint8_t		len;
		/*! the string data itself */
		uint8_t		sdata[0];
	};
	uint8_t	str[0];
};


/*! current state of the sforth engine
 *
 * \warning	for architectures (e.g. armv6/armv7m),
 *		it is possible that your toolchain is
 *		configured (my current one is) to use
 *		'-fshort-enums' by default; as the
 *		sforth engine stack is CELL_NR_BITS bit, this
 *		can cause very unusual and hard to
 *		track bugs, because of the many typecasts
 *		when operating with the stack; making
 *		such enumerations explicitly 'non-short'
 *		should save much grief */
enum ENUM_SFORTH_STATE
{
	/*! the sforth engine is currently operating in compilation mode */
	STATE_COMPILING		= (unsigned) -1,
	/*! the sforth engine is currently operating in interpretation mode */
	STATE_INTERPRETING	= 0,
};


/*! a data structure describing the format of the words stored in the dictionary
 *
 * in the sforth virtual machine, a word execution token (abbreviated 'xt') is
 * a pointer to the word's 'struct word' (this data structure) */
struct word
{
	/*! a link to the next word in the dictionary search order
	 *
	 * the dictionary search starts with the most recently defined word,
	 * and proceeds to words previously defined; the very first word
	 * defined (the first one added to the dictionary) will have
	 * this field set to 0, at which point the search terminates */
	struct word *	link;
	/*! a pointer to the name of this word, represented as a counted string */
	union
	{
		union cstr	* name;
		char		* xname;
	};
	/*! various flags for this word */
	struct
	{
		/*! immediacy flag; if nonzero, then this word is immediate */
		uint16_t	is_immediate : 1;
		/*! if nonzero, then this word will be ignored when searching the dictionary */
		uint16_t	is_smudged : 1;
		/*! 'does>' flag
		 *
		 * if nonzero, this word is a special case of a word
		 * defined with the 'does>' construct; in this case,
		 * the cfa field of this word is a pointer to the
		 * word behaviour defined by 'does>', and exec_does
		 * is invoked directly by do_colon(), instead of
		 * jumping to the cfa address of this word */
		uint16_t	is_does_proper : 1;
	};
	/*! code field for this word
	 * 
	 * normally, this is a pointer to a function that is being invoked
	 * when this word is being excecuted (for an exception - see the
	 * comments about the 'is_does_proper' flag above) */
	void (* cfa)(void);
	/*! data field for this word */
	union
	{
		/*! word data contents, represented as an array of execution tokens (used by ':')
		 *
		 * terminated by a zero (null) entry for colon definitions; also,
		 * a zero (null) entry is a special case to execute EXIT when
	         * doing a colon definition execution */
		struct word * pfa[0];
		/*! word data contents, represented as an array of cells */
		cell	cells[0];
	};
};

#if 0
#include "sf-opt-string.h"
#include "sf-opt-prog-tools.h"
#include "sf-opt-file.h"
#endif

/* environment settings words */
void do_cr_echo(void);
void do_colon_debug(void);

void do_dot_s(void);
void do_question(void);
void do_s_backslash_quote(void);
void do_dump(void);

/* miscellaneous helper routines */
int xtolower(int c);
int xtoupper(int c);
int xisspace(int c);
int xstrlen(const char * str);
void * xmemcpy(void * dest, const void * src, int len);
void * xmemset(void * dest, int c, int len);
void * xmemmove(void * dest, const void * src, int len);
void * xmemmove(void * dest, const void * src, int len);
int xmemcmp(const void *s1, const void *s2, int len);
int xstrncmp(const char * s1, const char * s2, int n);
int xstrncasecmp(const char * s1, const char * s2, int n);
void print_str(const char * str);
void print_udecimal(cell x);
void sf_eval(const char * sfcode);

/*
 *
 * macros facilitating the creation of new dictionaries
 *
 */ 

#define MAKE_SINGLE_WORD(strname, fname) { .link = 0, .name = (union cstr *) & (const struct { uint8_t len; uint8_t s[sizeof strname]; } ) { .len = sizeof strname - 1, .s = strname, }, .is_immediate = 0, .is_smudged = 0, .is_does_proper = 0, .cfa = fname, }

/*
 *
 * make available all core words
 *
 */ 
#include "sf-generate-prototypes.h"
#include "dictionary.h"
/*
 *
 * sforth engine data stack manipulation routines
 *
 */
/* pop routines */
cell sf_pop(void);
scell sf_spop(void);
dcell sf_popd(void);
sdcell sf_spopd(void);
/* push routines */
void sf_push(cell x);
void sf_spush(scell x);
void sf_pushd(dcell x);
void sf_spushd(sdcell x);
/* top routines */
cell sf_top(void);
scell sf_stop(void);
dcell sf_topd(void);
sdcell sf_stopd(void);
/* over routines */
void sf_over(void);
void sf_two_over(void);
/* rot routines */
void sf_rot(void);
/* pick routines */
void sf_pick(void);
/* roll routines */
void sf_roll(void);

int sf_get_depth(void);

int sf_get_results(cell results[], int nr_results);


void sf_reset(void);
void sf_merge_custom_dictionary(struct word * dict_end, const struct word * dict_start);

void sf_dump_current_parse_loc(void);

void sf_init(void);

#endif /* __CORE_WORDS_H__ */

