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
#if defined	GENERATE_WORD_PROTOTYPES
	/* generate function prototypes */

	#undef MKWORD
	#undef MKEXTWORD
	#undef MKIMMWORD
	#undef TERMINATOR

	#define MKWORD(dictname, idx, strname, fname) void fname(void);
	#define MKEXTWORD(dictname, idx, strname, fname) void fname(void);
	#define MKIMMWORD(dictname, idx, strname, fname) void fname(void);
	#define TERMINATOR

#elif defined GENERATE_DICTIONARY_ENTRIES
	/* generate dictionary entries */

	#undef MKWORD
	#undef MKEXTWORD
	#undef MKIMMWORD
	#undef TERMINATOR

	#define MKWORD(dictname, idx, strname, fname) { .link = (struct word *) dictname + idx, .name = (union cstr *) & (const struct { uint8_t len; uint8_t s[sizeof strname]; } ) { .len = sizeof strname - 1, .s = strname, }, .is_immediate = 0, .is_smudged = 0, .is_does_proper = 0, .cfa = fname, },
	#define MKEXTWORD(dictname, idx, strname, fname) { .link = (struct word *) dictname + idx, .name = (union cstr *) & (const struct { uint8_t len; uint8_t s[sizeof strname]; } ) { .len = sizeof strname - 1, .s = strname, }, .is_immediate = 0, .is_smudged = 0, .is_does_proper = 0, .cfa = fname, }
	#define MKIMMWORD(dictname, idx, strname, fname) { .link = (struct word *) dictname + idx, .name = (union cstr *) & (const struct { uint8_t len; uint8_t s[sizeof strname]; } ) { .len = sizeof strname - 1, .s = strname, }, .is_immediate = 1, .is_smudged = 0, .is_does_proper = 0, .cfa = fname, },
	#define TERMINATOR ,

#else

	/* fall back and stick to generating dictionary entries */

	#undef MKWORD
	#undef MKEXTWORD
	#undef MKIMMWORD
	#undef TERMINATOR

	#define MKWORD(dictname, idx, strname, fname, dummy ...) { .link = (struct word *) dictname + idx, .name = (union cstr *) & (const struct { uint8_t len; uint8_t s[sizeof strname]; } ) { .len = sizeof strname - 1, .s = strname, }, .is_immediate = 0, .is_smudged = 0, .is_does_proper = 0, .cfa = fname, }
	#define MKEXTWORD(dictname, idx, strname, fname, dummy ...) { .link = (struct word *) dictname + idx, .name = (union cstr *) & (const struct { uint8_t len; uint8_t s[sizeof strname]; } ) { .len = sizeof strname - 1, .s = strname, }, .is_immediate = 0, .is_smudged = 0, .is_does_proper = 0, .cfa = fname, }
	#define MKIMMWORD(dictname, idx, strname, fname, dummy ...) { .link = (struct word *) dictname + idx, .name = (union cstr *) & (const struct { uint8_t len; uint8_t s[sizeof strname]; } ) { .len = sizeof strname - 1, .s = strname, }, .is_immediate = 1, .is_smudged = 0, .is_does_proper = 0, .cfa = fname, }
	#define TERMINATOR ,

#endif

