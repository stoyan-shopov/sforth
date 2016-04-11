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

#if ENABLE_HDUMP
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#endif /* ENABLE_HDUMP */

#include "engine.h"

enum
{
	STRBUF_SIZE	= 128,
};
/* buffer for use by 's"'; the string is stored as a counted string (the first byte equals the string length) */
static uint8_t strbuf[STRBUF_SIZE + 1];

static void do_opt_file_s_quote(void)
{
	/* state-smart extension of the 's"' word from the core wordset */
	do_state(); do_fetch();
	if (sf_pop() == STATE_COMPILING)
		do_s_quote();
	else
	{
		cell len;
		/* interpreting */
		sf_push('"');
		do_parse();
		len = (cell) sf_pop();
		len = (len > sizeof strbuf - 1) ? sizeof strbuf - 1 : len;
		* strbuf = len;
		xmemcpy(strbuf + 1, (void *) sf_pop(), len);
		sf_push((cell)strbuf);
		do_count();
	}
}

static void do_include(void) { /* : include bl parse included ; */ do_bl(); do_parse(); do_included(); }


/*
11.6.1.2142
REPOSITION-FILE
FILE
( ud fileid -- ior )
Reposition the file identified by fileid to ud. ior is the implementation-defined I/O result
code. An ambiguous condition exists if the file is positioned outside the file boundaries.
At the conclusion of the operation, FILE-POSITION returns the value ud.
*/
static void do_reposition_file(void) { cell fd, offset; offset = sf_pop(); fd = sf_pop(); sffseek(fd, offset); }
#if ENABLE_HDUMP
static void do_hack_bin_dump(void)
{ /* ( address count --) */ size_t count = sf_pop(); void * buf = (void *) sf_pop(); int fd = open("out.bin", O_CREAT|O_WRONLY|O_TRUNC); if (fd != -1) { write(fd, buf, count); close(fd); } }
#endif /* ENABLE_HDUMP */

#include "sf-word-wizard.h"
static struct word dict_base_dummy_word[1] = { MKWORD(0, 0, "", 0), };
static const struct word custom_dict[] = {
	/* override the sforth supplied engine reset */
	MKIMMWORD(dict_base_dummy_word,	0,		"s\"",		do_opt_file_s_quote),
	MKWORD(custom_dict,		__COUNTER__,	"include",	do_include),
	MKWORD(custom_dict,		__COUNTER__,	"reposition-file",	do_reposition_file),
#if ENABLE_HDUMP
	MKWORD(custom_dict,		__COUNTER__,	"hdump",	do_hack_bin_dump),
#endif /* ENABLE_HDUMP */

}, * custom_dict_start = custom_dict + __COUNTER__;

static void sf_opt_file_init(void) __attribute__((constructor));
static void sf_opt_file_init(void)
{
	sf_merge_custom_dictionary(dict_base_dummy_word, custom_dict_start);
}
