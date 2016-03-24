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
	/* override the sforth supplied engine reset */
	MKWORD(dict_base_dummy_word,	0,		"squared",	do_squared),
	MKWORD(custom_dict,		__COUNTER__,	"gcd",	do_gcd),

}, * custom_dict_start = custom_dict + __COUNTER__;

static void sf_opt_sample_init(void)
{
	sf_merge_custom_dictionary(dict_base_dummy_word, custom_dict_start);
}

static void (* volatile const dictionary_initializer)(void) __attribute__((used)) __attribute__ (( section(".sf_init"))) = sf_opt_sample_init;

int main(void)
{
	sf_init();
	sf_eval("12 7 * 13 7 * .( the greatest common divisor of ) over . .( and ) dup . .( is ) gcd . cr");

	return 0;
}
