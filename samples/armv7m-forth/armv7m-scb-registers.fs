
$e000ed00 constant CPUID
$e000ed04 constant ICSR
$e000ed08 constant VTOR
$e000ed0c constant AIRCR
$e000ed10 constant SCR
$e000ed14 constant CCR
$e000ed18 constant SHPR1
$e000ed1c constant SHPR2
$e000ed20 constant SHPR3
$e000ed24 constant SHCSR
$e000ed28 constant CFSR
$e000ed2c constant HFSR
$e000ed30 constant DFSR
$e000ed34 constant MMFAR
$e000ed3c constant AFSR
$e000ed7c constant CPUID
$e000ed88 constant CPACR

.( armv7m SCB registers defined)cr 

\ system timer registers
$e000e010 constant SYSTICK-CSR
$e000e014 constant SYSTICK-RVR
$e000e018 constant SYSTICK-CVR

\ nested vectored interrupt controller (nvic) registers
$e000e100 constant NVIC-ISER-BASE
$e000e180 constant NVIC-ICER-BASE
$e000e200 constant NVIC-ISPR-BASE
$e000e280 constant NVIC-ICPR-BASE
$e000e300 constant NVIC-IABR-BASE
$e000e400 constant NVIC-IPR-BASE

