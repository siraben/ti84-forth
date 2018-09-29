\ Adapted from jonesforth

\ Interesting to note that this still works despite having a register
\ (BC) dedicated to the top of the stack, because the register
\ contents get pushed onto it before we print the entire stack, so it
\ works out.

: .S
	SP@
	BEGIN
		DUP SP0 @ <
	WHILE
		DUP @ .
		SPACE
		2 +
	REPEAT
	DROP
;
