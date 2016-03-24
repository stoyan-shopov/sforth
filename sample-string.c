#include "engine.h"

int main(int argc, char ** argv)
{
const char * eval_str = ".( hello, sforth!)cr 12 12 *";
	sf_init();
	if (argc > 1)
		eval_str = argv[1];
	sf_reset();
	sf_eval(eval_str);
	print_str("string evaluation results: ");
	sf_eval(".s cr");
	return 0;
}
