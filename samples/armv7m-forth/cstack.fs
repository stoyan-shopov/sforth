\ helper control-stack
\ this is currently used by armv7m.fs for building do-leave-loop control structures
\ another stack is needed to avoid complexities when handling intermixed if-leave-then
\ constructs; such complexities can be easily handled by using a dedicated control stack

\ use an empty-ascending convention for the control stack
16 constant cstack-depth
create cstack
cstack-depth cells allot
variable cstack-pointer
0 cstack-pointer !
: ?cstack-overflow cstack-pointer @ cstack-depth = abort" control stack overflow" ;
: ?cstack-underflow cstack-pointer @ 0 = abort" control stack underflow" ;

: >cstack
	?cstack-overflow cstack cstack-pointer @ cells + !
	1 cstack-pointer +! ;

: cstack>
	?cstack-underflow
	-1 cstack-pointer +!
	cstack cstack-pointer @ cells + @
       	;
