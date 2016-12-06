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
#include "sf-word-wizard.h"

static void do_squared(void) { cell x = sf_pop(); sf_push(x * x); }
static void do_gcd(void)
/* implements the FORTH word:
: gcd ( n1 n2 | n) begin over mod swap over 0= until nip ;
*/
{ do { do_over(); do_mod(); do_swap(); do_over(); do_zero_not_equals(); } while (sf_pop()); do_nip(); }

static struct word dict_base_dummy_word[1] = { MKWORD(0, 0, "", 0), };
static const struct word custom_dict[] = {
	MKWORD(dict_base_dummy_word,	0,		"squared",	do_squared),
	MKWORD(custom_dict,		__COUNTER__,	"gcd",	do_gcd),

}, * custom_dict_start = custom_dict + __COUNTER__;

static void sf_opt_sample_init(void) __attribute__((constructor));
static void sf_opt_sample_init(void)
{
	sf_merge_custom_dictionary(dict_base_dummy_word, custom_dict_start);
}
