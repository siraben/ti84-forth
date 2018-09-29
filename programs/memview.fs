\ View memory locations interactively.

\ Number of bytes that the plot window can see.

NUM 767 CONST PLOTSZ
NUM 12 CONST STEP
NUM 36 CONST STEPL

\ Memory slice; view the memory location on the stack.
: MEMSLICE ( addr -- )
  PLOTSS SWAP PLOTSZ CMOVE PP
;

: MEMVIEW ( addr -- addr )
  BEGIN
    KEY DUP 5 <>
  WHILE
    OVER MEMSLICE    
    CASE
      3 OF STEP + ENDOF
      4 OF STEP - ENDOF
      1 OF STEPL + ENDOF
      2 OF STEPL - ENDOF
    ENDCASE
  REPEAT
  DROP
;

\ View memory starting from the string buffer:
\ BUF MEMVIEW
