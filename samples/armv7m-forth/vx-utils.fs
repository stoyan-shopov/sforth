
$08000000 constant flash-mem-base

flash-mem-base >native 12 1024 * + 12 - >cross constant usb-emit-vector
flash-mem-base >native 12 1024 * + 8 - >cross constant usb-getchar-vector
flash-mem-base >native 12 1024 * + 4 - >cross constant usb-sync-vector

: usb-emit ( character -- )
\ 1e4:   4788            blx     r1
	usb-emit-vector @ 1 pop{rX} 0 pop{rX} $4788 thw, ;

: usb-sync ( -- )
\ 1e6:   4780            blx     r0
	usb-sync-vector @ 0 pop{rX} $4780 thw, ;
