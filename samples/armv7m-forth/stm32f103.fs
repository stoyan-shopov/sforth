( stm32f103-specific peripheral registers)

\ offset computation inlining
n: +offset \ defining word - expects an offset on the stack
	create >native , >cross does> >native @ 
	\ if the offset is zero - do not generate any code
	?dup 0<> if t-l t-+ then >cross n;

\ reset and clock control
$40021000 constant rcc-base
rcc-base >native $18 + >cross constant apb2enr
$40010800 constant port-a-base
$40010c00 constant port-b-base
$40011000 constant port-c-base


$0 +offset gpio-x-crl
$4 +offset gpio-x-crh
$8 +offset gpio-x-idr
$c +offset gpio-x-odr
$10 +offset gpio-x-srr
$14 +offset gpio-x-rr


\ /* port pin operation mode constants */
0 constant pin-mode-input
1 constant pin-mode-output-10-mhz
2 constant pin-mode-output-2-mhz
3 constant pin-mode-output-50-mhz
\ /* port pin input configuration values */
0 constant pin-cfg-input-analog
1 constant pin-cfg-input-floating
2 constant pin-cfg-input-with-pull-up-or-down
\ /* port pin output configuration values */
0 constant pin-cfg-output-push-pull
1 constant pin-cfg-output-open-drain
2 constant pin-cfg-output-alt-func-push-pull
3 constant pin-cfg-output-alt-func-open-drain

\ port-number to port-base-register table
t-create port-base-addresses
port-a-base tw, port-b-base tw, port-c-base tw,

0 [if]
: eth>read
	$e0028008 t-l t-@ $1ffe00ff t-l t-and $e00000f0 t-l t-or $e0028008 t-l t-!
	;
: eth>read
	$e0028008 t-l dup t-@ $1ffe00ff t-l t-and $e00000f0 t-l t-or swap t-!
	;
[then]

false [if]
: over over ;
: swap swap ;
: rot rot ;
: or or ;
: and and ;
: lshift lshift ;
: invert invert ;
: @ @ ;
: ! ! ;
: - - ;
: + + ;
: cells cells ;
: * * ;
[then]


: [cell] ( cell-array-base cell-index -- array-cell-address)
	cells +
	;
: [cell]@ ( cell-array-base cell-index -- array-cell-value)
	[cell] @
	;

: >config-register-address ( port-number pin-number -- config-register-address pin-number-in-register)
	\ t-swap t-cells port-base-addresses t-+ t-@
	\ t-over 8 t-l t-< t-invert t-if gpio-x-crh t-swap 8 t-l t-- t-else t-swap t-then
	port-base-addresses rot [cell]@
	over 8 t-l < invert if gpio-x-crh swap 8 t-l - else swap then
	;

: configure-port-pin ( port-number pin-number operation-mode --)
	>r >config-register-address 4 t-l * over @ over $f t-l swap lshift invert and
	r> rot lshift or swap !
	;

