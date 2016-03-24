#include "engine.h"

static void do_hello(void)
{
const char * msg = "hello\n";
	sf_push((cell) msg);
	sf_push(xstrlen(msg));
	do_type();
}

static void do_squared(void)
{
scell x = sf_pop();
	sf_push(x * x);
}

#include "sf-word-wizard.h"
static struct word dict_base_dummy_word[1] = { MKWORD(0, 0, "", 0), };

static const struct word custom_dict[] = {
	MKWORD(dict_base_dummy_word, 0, "hello", do_hello),
	MKWORD(custom_dict, 0, "^2", do_squared),
}, * custom_dict_start = custom_dict + 1;


int main(void)
{
	sf_init();
	sf_merge_custom_dictionary(dict_base_dummy_word, custom_dict_start);
	do_quit();

	return 0;
}
