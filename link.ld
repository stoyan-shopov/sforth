SECTIONS
{
.sf_init_1 :
	{
		. = ALIGN(16);
		__start_sf_init	= .;
		KEEP(*(.sf_init))
		__end_sf_init	= .;
	}
}
INSERT AFTER .text;
