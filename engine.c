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


void do_abort(void) __attribute__ ((noreturn));
#include <stdbool.h>
#include <setjmp.h>

#include "engine.h"

/******************************************
 *
 * sforth engine virtual machine variables
 *
 ******************************************/

/*! instruction pointer; normally, points to the next word to be executed */
static union
{
	/*! allows treating the instruction pointer as pointing to an array of execution tokens (pointers to words) */
	struct word	** word;
	/*! allows treating the instruction pointer as pointing to a cell (unsigned) */
	cell	* cell;
	/*! allows treating the instruction pointer as pointing to a cell (signed) */
	scell		* scell;
}
IP;
/*! working word pointer; holds the execution token of the word currently executed */
static struct word * WP;

/*! core memory available to the sforth engine
 *
 * this memory is used for compiling new words; when the sforth engine is reset,
 * 'here' is initialized with this
 */
static cell core[SFORTH_CORE_SIZE_IN_CELLS];

/*! various sforth machine tweaking parameters */
static struct
{
	/*! enables/disables echoing of the 'carriage-return' character when receiving input from the user input device
	 *
	 * echoing is useful e.g. when the sforth machine is running over a serial port */
	cell	cr_echo_enabled;
	/*! enables/disables debug message prints from the runtime colon interpreter
	 * 
	 * this can be useful for quick localization of mistakes that
	 * cause crashes and/or coredumps */
	cell	colon_debug_enabled;
}
env = 
{
	.cr_echo_enabled	=	C_FALSE,
	.colon_debug_enabled	=	C_FALSE,
};

/*! data stack; 'empty ascending' convention has been adopted for the data stack 
 *
 * the data stack grows up */ 
static cell dstack[DSTACK_SIZE + /* add two more locations in order to be able to print a warning when a stack overflow occurs */ 2];
/*! data stack stack pointer
 *
 * as the data stack grows up, this actually contains the number of elements currently
 * held in the data stack; when the data stack is empty, this equals zero */ 
static int sp;
/*! return stack; 'empty ascending' convention has been adopted for the return stack 
 *
 * the return stack grows up */ 
static cell rstack[RSTACK_SIZE];
/*! return stack stack pointer
 *
 * as the return stack grows up, this actually contains the number of elements currently
 * held in the return stack; when the return stack is empty, this equals zero */ 
static int rsp;
/*! current state of the sforth engine */
static /*enum ENUM_SFORTH_STATE*/ cell state;

/*
 *
 * sforth engine exception handling related data
 *
 */

/* the current exception model adopted is very simple, because
 * it is intended to have small overhead and memory footprint,
 * so that it fits well in small embedded targets (e.g. the
 * vx probe)
 *
 * it is assumed that there are only two entry points/operating modes
 * that the sforth engine can be activated; these two operating modes are:
 * 	- reading input from the user input device -
 * 		this is done by 'quit' (function do_quit())
 * 	- reading input from a string - this is done by 'evaluate'
 * 		(function 'sf_eval()')
 *
 * these two functions check if the longjmp buffer has already been set up
 * and if so, they don't touch it; if it has not been set up, they initialize it;
 * there is a flag variable below ('is_jmpenv_valid') to denote if the longjmp
 * buffer has been set up or not; also, these two functions mark the longjmp buffer as
 * not set up (uninitialized) whenever they terminate; this way, in a case
 * of mutual recursion of 'quit' and 'evaluate', if an exception occurs that
 * makes a longjmp() to be taken, execution always resumes at the outermost
 * invocation of 'quit'/'evaluate', whichever was invoked first */
/*! a flag variable to denote if the 'jmpenv' buffer below contains valid data or not
 *
 * also read the paragraph above */
static bool is_jmpenv_valid = false;
/*! a jump buffer used by the abort routines to return control to 'quit'/'evaluate' */
static jmp_buf jmpenv;
/*! an enumeration used to denote the exception reason that caused a 'longjmp' exception to be taken */
enum SF_EXC_CODE_ENUM
{
	/*! an invalid exception code; 0 cannot be passed to 'longjmp()' */
	SF_EXC_CODE_INVALID	= 0,
	/*! execution was aborted by a general 'abort' cause */
	SF_EXC_CODE_ABORT,
	/*! execution was aborted by execution of the 'bye' word */
	SF_EXC_CODE_BYE,
};

/*! the numeric radix currently in effect, used for number-conversions */
static cell base;
/*! the data space pointer */
static union
{
	/*! allows treating the data space pointer as pointing to a cell */
	cell	* cell;
	/*! allows treating the data space pointer as pointing to a character */
	uint8_t		* chr;
	/*! allows treating the data space pointer as pointing to an execution token */
	const struct word	** word;
}
here;
/*! data area used by the 'word' and 'pad' sforth words */
static struct
{
	/*! length of the word string currently held in the 's' data ares below */
	uint8_t		len;
	/*! the word string itself */
	uint8_t		buf[WORDPAD_SIZE];

}
wordpad;

/*! input data buffer, in case the current input source is the user input device, or a file
 *
 * whenever the current input source is the user input device, or a file,
 * the 'buf' field in the 'input_spec' data structure below *must* be initialized
 * to point to this array
 *
 * normally, this is repopulated by 'refill' */
static uint8_t		inbuf_area[INBUF_SIZE];
/*! input buffer data area; this is the buffer filled by the 'refill' word */
static struct input_spec
{
	/*! sforth engine input source specification
	 *
	 * this value specifies the current source from which the
	 * sforth engine is getting its input - an in-memory string
	 * (specified via 'evaluate'), the user input device, or a file;
	 * the value of this field determines what field of the
	 * (anonymous) input source union below is in effect for feeding
	 * input to the sforth engine */
	union
	{
		enum SOURCE_ID
		{
			/*! the sforth engine is currently getting its input from the user input device */
			SOURCE_ID_USER_INPUT_DEVICE	= 0,
			/*! the sforth engine is currently getting its input from an in-memory string
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
			SOURCE_ID_STRING		= (scell) -1,
		}
		source_id;
		/* any other number designates a file identifier */
		int	file_id;
	};
	/*! current index in the current input source at which parsing proceeds; this is what is returned by the '>IN' word */
	cell	idx;
	/*! the number of characters held in the current input source
	 *
	 * specifically, when the 'idx' field above equals this field, all
	 * of the characters from the input source have  have been consumed;
	 * when this field is zero, the input source is empty */
	unsigned	len;
	/*! this union holds the current input source proper
	 *
	 * what field of this union is currently in effect depends on the value of the
	 * 'source_id' field above */
	union /* input_source */
	{
		/*! input data buffer, in case the current input source is the user input device, or a file
		 *
		 * whenever the current input source is the user input device, or a file,
		 * this *must* be initialized to point to the 'inbuf_area' array defined above
		 *
		 * normally, this is repopulated by 'refill' */
		uint8_t		* buf;
		/*! start of the string, in case the current input source is an in-memory string */
		const uint8_t	* str;
	};
}
input_spec;

/*! pictured numeric output data area */
static struct
{
	/*! the length of the string currently held in the pictured numeric output buffer */
	//unsigned	len;
	/*! the pictured numeric output string start index, this is the index at which the next character will be added */
	unsigned	idx;
	/*! the pictured numeric output string buffer itself */
	uint8_t		buf[PNOBUF_SIZE];
}
pno;


static void strabort(const char * func, const char * msg) __attribute__ ((noreturn));
static void strabort(const char * func, const char * msg) { if (0) *(int*)0=0; print_str(func); print_str(": "); print_str(msg); print_str("\n"); do_abort(); }
#define sabort(x) strabort(__func__, x)

/* return and data stacks manipulation words */

/* pop routines */
static inline cell pop(void) { if (!sp) sabort("data stack underflow"); return dstack[-- sp]; }
static inline scell spop(void) { if (!sp) sabort("data stack underflow"); return (scell) dstack[-- sp]; }
static inline dcell popd(void) { dcell res; if (sp < 2) sabort("data stack underflow"); res = (dcell) dstack[-- sp]; res <<= CELL_NR_BITS; return res | dstack[-- sp]; }
static inline sdcell spopd(void) { sdcell res; if (sp < 2) sabort("data stack underflow"); res = (sdcell) dstack[-- sp]; res <<= CELL_NR_BITS; return res | dstack[-- sp]; }
static inline cell rpop(void) { if (!rsp) sabort("return stack underflow"); return rstack[-- rsp]; }

/* push routines */
static inline void push(cell x) { if (sp == DSTACK_SIZE) sabort("data stack overflow"); dstack[sp ++] = x; }
static inline void spush(scell x) { if (sp == DSTACK_SIZE) sabort("data stack overflow"); dstack[sp ++] = (cell) x; }
static inline void pushd(dcell x) { if (sp > DSTACK_SIZE - 2) sabort("data stack overflow"); dstack[sp ++] = (cell) x; dstack[sp ++] = (cell) (x >> CELL_NR_BITS); }
static inline void spushd(sdcell x) { pushd((dcell) x); }
static inline void rpush(cell x) { if (rsp == RSTACK_SIZE) sabort("return stack overflow"); rstack[rsp ++] = x; }

/* top routines */
static inline cell top(void) { if (!sp) sabort("data stack empty"); return dstack[sp - 1]; }
static inline dcell topd(void) { dcell res; if (sp < 2) sabort("data stack underflow"); res = dstack[sp - 1]; res <<= CELL_NR_BITS; return res | dstack[sp - 2]; }
static inline cell rtop(void) { if (!rsp) sabort("return stack empty"); return rstack[rsp - 1]; }

/* swap routines */
static inline void swap(void) { cell t; if (sp < 2) sabort("data stack underflow"); t = dstack[sp - 2]; dstack[sp - 2] = dstack[sp - 1]; dstack[sp - 1] = t; }

/* dup routines */
static inline void ddup(void) { if (!sp) sabort("data stack empty"); if (sp == DSTACK_SIZE) sabort("data stack overflow"); dstack[sp] = dstack[sp - 1]; sp ++; }

/* return stack manipulation routines */
static inline void two_to_r(void) { if (sp < 2 || rsp > RSTACK_SIZE - 2) sabort("bad stack"); rstack[rsp] = dstack[sp - 2]; rstack[rsp + 1] = dstack[sp - 1]; sp -= 2; rsp += 2; }
static inline void two_r_from(void) { if (rsp < 2 || sp > DSTACK_SIZE - 2) sabort("bad stack"); dstack[sp] = rstack[rsp - 2]; dstack[sp + 1] = rstack[rsp - 1]; rsp -= 2; sp += 2; }
static inline void two_r_fetch(void) { if (rsp < 2 || sp > DSTACK_SIZE - 2) sabort("bad stack"); dstack[sp] = rstack[rsp - 2]; dstack[sp + 1] = rstack[rsp - 1]; sp += 2; }

/* over routines */
static inline void over(void) { if (sp < 2 || sp == DSTACK_SIZE) sabort("cannot execute over in data stack"); dstack[sp] = dstack[sp - 2]; sp ++; }
static inline void two_over(void) { if (sp < 4 || sp > DSTACK_SIZE - 2) sabort("data stack overflow"); dstack[sp] = dstack[sp - 4]; dstack[sp + 1] = dstack[sp - 3]; sp += 2; }

/* rot routines */
static inline void rot(void) { if (sp < 3) sabort("cannot execute rot in data stack"); cell x; x = dstack[sp - 3]; dstack[sp - 3] = dstack[sp - 2]; dstack[sp - 2] = dstack[sp - 1]; dstack[sp - 1] = x; }

/* r-from */
static inline void rfrom(void) { push(rpop()); }
/* pick routines */
static void pick(void) { cell i; i = pop(); if (sp < i + 1) sabort("bad stack for pick"); push(dstack[sp - 1 - i]); }
/* roll routines */
static void roll(void) { cell i; i = top(); pick(); xmemmove(dstack + sp - 1 - i - 1, dstack + sp - 1 - i, (i + 1) * sizeof * dstack); pop(); }

/* miscellaneous helper routines */
int xtolower(int c) { if ('A' <= c && c <= 'Z') c = c - 'A' + 'a'; return c; }
int xtoupper(int c) { if ('a' <= c && c <= 'z') c = c - 'a' + 'A'; return c; }
int xisspace(int c) { return (c == ' ' || c == '\n' || c == '\t' || c == '\f' || c == '\r' || c == '\v') ? 1 : 0; }
int xstrlen(const char * str) { int i = 0; while (* str ++) i ++; return i; }
void * xmemcpy(void * dest, const void * src, int len) { uint8_t * d = (uint8_t *) dest; const uint8_t * s = (const uint8_t *) src; while (len --) * d ++ = * s ++; return dest; }
void * xmemset(void * dest, int c, int len) { uint8_t * d = (uint8_t *) dest; while (len --) * d ++ = c; return dest; }
void * xmemmove(void * dest, const void * src, int len) { uint8_t * d = (uint8_t *) dest; const uint8_t * s = (const uint8_t *) src;
	if (d <= s) while (len --) * d ++ = * s ++; else { d += len; s += len; while (len --) * -- d = * -- s; } return dest; }
int xmemcmp(const void *s1, const void *s2, int len) { unsigned const char * c1 = (unsigned const char *) s1, * c2 = (unsigned const char *) s2; while (len) { if (* c1 < * c2) return -1; if (* c1 ++ > * c2 ++) return 1; len --; } return 0; }
int xstrncasecmp(const char * s1, const char * s2, int n) { int i; while (n --) { i = xtolower(* (uint8_t *) s1); i -= xtolower(* (uint8_t *) s2); if (! i) { s1 ++, s2 ++; continue; } if (i < 0) return -1; return 1; } return 0; }
int xstrncmp(const char * s1, const char * s2, int n) { int i; while (n --) { i = * (uint8_t *) s1; i -= * (uint8_t *) s2; if (! i) { s1 ++, s2 ++; continue; } if (i < 0) return -1; return 1; } return 0; }
void print_str(const char * str) { /* note that we can't simply do: 'push((cell) str); push(xstrlen(str)); do_type();', because the stack may be corrupted */ while (* str) sfputc(* (uint8_t *) str ++); }
void print_udecimal(cell x) { cell prev_base = base; base = 10; push(x); do_u_dot(); base = prev_base; }


static void postpone(const char * s)
{
cell i;
	i = xstrlen(s);
	if (i > sizeof wordpad.buf)
		i = sizeof(wordpad.buf);
	wordpad.len = i;
	xmemcpy(wordpad.buf, s, i);
	push((cell) & wordpad);
	do_find();
	switch (pop())
	{
		case 0:
			/* word not found */
			print_str("'");
			do_count();
			do_type();
			print_str("'");
			print_str(": cannot compile word, word not found\n");
			do_abort();
			break;
		default:
		/* compile */
			do_compile_comma();
			break;
	}
}


/* words for internal use, these are not findable in the base dictionary */
static void runtime_does(void);
static void exec_does(void);
static void runtime_loop(void);
static void runtime_plus_loop(void);
static void runtime_literal(void);
static void runtime_branch(void);
static void runtime_branch_on_false(void);

static const struct word xt_runtime_does = MAKE_SINGLE_WORD("<<< runtime-does >>>", runtime_does);
static const struct word xt_runtime_loop = MAKE_SINGLE_WORD("<<< runtime-loop >>>", runtime_loop);
static const struct word xt_runtime_plus_loop = MAKE_SINGLE_WORD("<<< runtime-plus-loop >>>", runtime_plus_loop);
static const struct word xt_runtime_literal = MAKE_SINGLE_WORD("<<< runtime-literal >>>", runtime_literal);
static const struct word xt_runtime_branch = MAKE_SINGLE_WORD("<<< runtime-branch >>>", runtime_branch);
static const struct word xt_runtime_branch_on_false = MAKE_SINGLE_WORD("<<< runtime-branch-on-false >>>", runtime_branch_on_false);

static const struct word dictionary[] =
{
	/* the end of the dictionary, the first - the empty word */
	{ .link = 0, .name = (union cstr *) & (const union cstr) { .len = 0, .sdata = "", }, .is_immediate = 0, .is_smudged = 0, .is_does_proper = 0, .cfa = 0, },
#include "sf-generate-dictionary-entries.h"
#include "dictionary.h"
};

/*! the sforth dictionary head, points to the most recently added dictionary word */
static struct word * latest = (struct word *)(dictionary + __COUNTER__), * latest_reset_value;

/*********************************************************************************/
static void runtime_branch_on_false(void)
{ if (!pop()) IP.word += * IP.cell; else IP.word ++; }
static void runtime_create(void)
{
	push((cell) & WP->pfa);
}
static void runtime_constant(void)
{ push(* WP->cells); }
static void interpret(void)
{
scell i;
struct word * xt;

	while (1)
	{
		push(' ');
		do_word();
		ddup();
		do_count();
		if (!pop())
		{
			pop();
			pop();
			break;
		}
		pop();
		do_find();
		switch (spop())
		{
			case 0:
				/* attempt to recognize a number */
				i = pop();
				pushd(0);
				push(i);
				do_count();
				do_to_number();

				if (!pop())
				{
					pop();
					/* number detected */
					push(popd());
					if (state == STATE_COMPILING)
						do_literal();
				}
				else
				{
					/* no number detected and word not found */
					print_str("word '");
					push(i);
					do_count();
					do_type();
					print_str("' not found\n");
					if (0 /* the 'abort' below will clean the stacks */) pop(), popd();
					do_abort();
				}
				break;
			case -1:
				/* non-immediate word */
				if (state == STATE_COMPILING)
				{
					do_compile_comma();
					break;
				}
				/* fallout */
			case 1:
				/* immediate word, or interpreting */
				xt = (struct word *) pop();
				WP = xt;
				if (!xt->is_does_proper)
					xt->cfa();
				else
					exec_does();
				break;

		}
	}
}
static void exec_does(void)
{
	struct word * w, ** prev_ip;

	prev_ip = IP.word;
	IP.word = (struct word **) WP->cfa;
	push((cell) WP->pfa);

	while ((w = * IP.word ++))
	{
		WP = w;
		if (!w->is_does_proper)
			w->cfa();
		else
			exec_does();
	}
	IP.word = prev_ip;
}
static void runtime_does(void)
{
	latest->is_does_proper = 1;
	latest->cfa = (void(*)(void)) (IP.cell + 1);
	/* latest->is_smudged = 0; */
}

static void runtime_plus_loop(void)
{
scell index, index1, limit, step;

	if (rsp < 2)
		sabort("runtime +loop: bad return stack");
	index = rstack[rsp - 1];
	limit = rstack[rsp - 2];
	step = spop();
	index1 = index + step;
	int sindex, sstep, slimit, sindex1;
	sindex = (index < 0) ? 1 : 0;
	sindex1 = ((scell) index + (scell) step < 0) ? 1 : 0;
	sstep = (step < 0) ? 1 : 0;
	slimit = (limit < 0) ? 1 : 0;

	if (!(sindex ^ sstep) && (sindex ^ sindex1))
	{
		/* signed overflow */
		print_str("signed overflow\n");
		if (((cell)index < (cell)limit && (cell)limit <= (cell)index1)
				|| ((cell)index1 < (cell)limit && (cell)limit <= (cell)index))
			dstack[sp] = 1;
		else
			dstack[sp] = 0;
	}
	else
	{
		if ((index < limit && limit <= index1)
				|| (index1 < limit && limit <= index))
			dstack[sp] = 1;
		else
			dstack[sp] = 0;
	}

	sp ++;
	rstack[rsp - 1] = step + index;
}

static void runtime_plus_loop_1(void)
{
static volatile scell limit1;
scell cnt, limit/*, limit1*/;
sdcell x;

	if (rsp < 2)
		sabort("runtime +loop: bad return stack");
	cnt = ((scell *)rstack)[rsp - 1];
	limit = ((scell *)rstack)[rsp - 2];
	/* note that this may underflow from negative to positive (e.g., min_int - 1 == max_int */
	limit1 = limit - 1;
	x = cnt;
	x += spop();

	if (0 || limit1 < limit)
	{
		/* the usual case - no arithmetic underflow */
		if (cnt < limit)
			sf_push((x >= limit) ? 1 : 0);
		else
			sf_push((x < limit) ? 1 : 0);
	}
	else
		/* arithmetic underflow */
		sf_push(1)/*, *(int*)0=0*/;

	rstack[rsp - 1] = x;
}
static void runtime_plus_loop_2(void)
{
cell cnt, limit/*, limit1*/;
cell x;
static volatile cell limit1;

	if (rsp < 2)
		sabort("runtime +loop: bad return stack");
	cnt = ((cell *)rstack)[rsp - 1];
	limit = ((cell *)rstack)[rsp - 2];
	/* note that this may underflow from negative to positive (e.g., min_int - 1 == max_int */
	limit1 = limit - 1;
	x = cnt;
	x += pop();

	if (0 || limit1 < limit)
	{
		/* the usual case - no arithmetic underflow */
		if (cnt < limit)
			sf_push((x >= limit) ? 1 : 0);
		else
			sf_push((x < limit) ? 1 : 0);
	}
	else
		/* arithmetic underflow */
		sf_push(1)/*, *(int*)0=0*/;

	rstack[rsp - 1] = x;
}

static void runtime_colon(void)
{
struct word * w;
int nest;

	nest = 0;
reenter:
	rpush((cell) IP.word);

	if (env.colon_debug_enabled)
	{
		print_str(__func__);
		print_str("(): starting execution of colon word: ");
		/* handle ':noname' (i.e., unnamed) definitions */
		if (WP->name) { push((cell) WP->name); do_count(); do_type(); do_cr(); }
		else print_str("<<<unnamed (':noname' ?) execution token>>>\n");
	}
	IP.word = (struct word **) & WP->pfa;
back:
	while ((w = * IP.word ++))
	{
		WP = w;
		if (! w->is_does_proper)
		{
			if (WP->cfa == runtime_colon)
			{
				nest ++;
				goto reenter;
			}
			else
			{
				if (env.colon_debug_enabled)
				{
					print_str(__func__);
					print_str("(): natively executing word: ");
					if (WP->name)
						push((cell) WP->name), do_count(), do_type(), do_cr();
					else
						print_str("<<< internal >>>"), do_cr();
				}
				WP->cfa();
			}
		}
		else
			exec_does();
	}
	IP.word = (struct word **) rpop();
	if (nest --)
		goto back;
}
static void runtime_branch(void)
{ IP.word += * IP.cell; }
static void runtime_literal(void)
{ push(* IP.cell ++); }
static void runtime_loop(void)
{
	two_r_fetch();
	push(pop() + 1);
	rpop();
	rpush(top());
	do_equals();
}
static void runtime_marker(void)
{
	latest = (struct word *) WP->pfa[0];
	here.cell = (cell *) WP->pfa[1];
}
static void runtime_value(void)
{ push(WP->cells[0]); }


/*
 *
 * exported routines for manipulation of the sforth engine
 *
 */

int sf_get_results(cell results[], int nr_results)
{
int i, j;

	i = (sp > nr_results) ? nr_results : sp;
	j = 0;
	while (sp && nr_results --)
		results[-- i] = sf_pop(), j ++;
	return j;
}

int sf_get_depth(void)
{
	return sp;
}

cell sf_pop(void) { return pop(); }
cell sf_top(void) { return top(); }
void sf_push(cell x) { push(x); }
void sf_eval(const char * sfcode)
{
int i;
	i = xstrlen(sfcode);
	push((cell) sfcode);
	push(i);
	do_evaluate();
}
void sf_merge_custom_dictionary(struct word * dict_end, const struct word * dict_start)
{
	dict_end->link = latest;
	* ((const struct word **) & latest) = dict_start;
}

static uint8_t next_sym(void)
{
	while (input_spec.len == input_spec.idx) { do_refill(); if (pop() == C_FALSE) strabort(__func__, "error parsing escaped string\n"); }
	return input_spec.buf[input_spec.idx ++];
}
#include "dictionary.c"

void sf_init(void)
{
	sf_reset();
}

