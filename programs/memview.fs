\ View memory locations interactively.
\ Number of bytes that the plot window can see.

767 CONST PLOTSZ
12 CONST STEP
36 CONST STEPL

: PP PAGE PLOT ;
\ Memory slice; view the memory location on the stack.
: MEMSLICE ( addr -- )
  PLOTSS PLOTSZ CMOVE PP
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
." Memview loaded."
